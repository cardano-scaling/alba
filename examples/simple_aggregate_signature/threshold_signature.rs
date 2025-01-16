use crate::simple_aggregate_signature::signature::Signature;
use crate::Element;
use alba::centralized_telescope::params::Params;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::CentralizedTelescope;
use blst::min_sig::{PublicKey, Signature as BLSSignature};
use blst::BLST_ERROR;
use std::collections::HashMap;

pub(crate) struct ThresholdSignature {
    proof: Proof,
    key_list: Vec<PublicKey>,
}

impl ThresholdSignature {
    pub(crate) fn aggregate(
        alba_signatures: &[Signature],
        params: &Params,
        key_list: &HashMap<usize, PublicKey>,
    ) -> Self {
        let prover_set = alba_signatures
            .iter()
            .map(|s| s.signature.to_bytes())
            .collect::<Vec<Element>>();
        let alba = CentralizedTelescope::create(params);
        let proof = alba.prove(&prover_set).unwrap();
        let proof_elements = proof.element_sequence.clone();
        // let mut public_keys = Vec::with_capacity(proof_elements.len());

        let proof_signatures = proof_elements
            .iter()
            .map(|element| BLSSignature::from_bytes(element).unwrap())
            .collect::<Vec<BLSSignature>>();

        // Resolve public keys corresponding to each signature
        let mut public_keys = Vec::with_capacity(proof_signatures.len());
        for sig in &proof_signatures {
            // Find the index of the signature in `alba_signatures`
            if let Some(signature_entry) =
                alba_signatures.iter().find(|entry| entry.signature == *sig)
            {
                if let Some(public_key) = key_list.get(&signature_entry.index) {
                    public_keys.push(*public_key);
                } else {
                    panic!("Public key not found for index: {}", signature_entry.index);
                }
            } else {
                panic!("Signature not found in alba_signatures: {:?}", sig);
            }
        }
        Self {
            proof,
            key_list: public_keys,
        }
    }

    /// Validates individual signatures in the threshold signature
    /// This function verifies each individual signature separately.
    /// The verification could also be done by using aggregation from blst library as follows:
    ///     - Aggregate signatures with: `AggregateSignature::aggregate`,
    ///     - Aggregate public keys with: `AggregatePublicKey::aggregate`,
    ///     - Convert aggregate signature to a signature and aggregate public key to a public key
    ///     - Verify the signature with public key against given message.
    fn validate_signatures(&self, msg: &[u8]) -> bool {
        let mut signatures = Vec::with_capacity(self.proof.element_sequence.len());
        for sig_bytes in &self.proof.element_sequence {
            let Ok(signature) = BLSSignature::from_bytes(sig_bytes.as_slice()) else {
                return false;
            };
            signatures.push(signature);
        }
        for (i, signature) in signatures.iter().enumerate() {
            if signature.verify(false, msg, &[], &[], &self.key_list[i], false)
                != BLST_ERROR::BLST_SUCCESS
            {
                return false;
            }
        }
        true
    }

    pub(crate) fn verify(&self, msg: &[u8], params: &Params) -> bool {
        if self.validate_signatures(msg) {
            let alba = CentralizedTelescope::create(params);
            return alba.verify(&self.proof);
        }
        false
    }
}
