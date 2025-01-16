use crate::Element;
use alba::centralized_telescope::params::Params;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::CentralizedTelescope;
use blst::min_sig::{PublicKey, Signature};
use blst::BLST_ERROR;
use std::collections::HashMap;

pub(crate) struct ThresholdSignature {
    proof: Proof,
    key_list: Vec<PublicKey>,
}

impl ThresholdSignature {
    pub(crate) fn aggregate(
        alba_signatures: &HashMap<Element, usize>,
        params: &Params,
        key_list: &HashMap<usize, PublicKey>,
    ) -> Self {
        let prover_set: Vec<Element> = alba_signatures.keys().copied().collect();
        let alba = CentralizedTelescope::create(params);
        let proof = alba.prove(&prover_set).unwrap();
        let signatures = proof.element_sequence.clone();
        let mut public_keys = Vec::with_capacity(signatures.len());

        for sig in signatures {
            public_keys.push(
                *key_list
                    .get(alba_signatures.get(sig.as_slice()).unwrap())
                    .unwrap(),
            );
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
            let Ok(signature) = Signature::from_bytes(sig_bytes.as_slice()) else {
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
        return true;
    }

    pub(crate) fn verify(&self, msg: &[u8], params: &Params) -> bool {
        if self.validate_signatures(msg) {
            let alba = CentralizedTelescope::create(params);
            return alba.verify(&self.proof);
        }
        false
    }
}
