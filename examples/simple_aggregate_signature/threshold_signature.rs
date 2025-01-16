use crate::Element;
use alba::centralized_telescope::params::Params;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::CentralizedTelescope;
use blst::min_sig::{AggregatePublicKey, AggregateSignature, PublicKey, Signature};
use blst::BLST_ERROR;
use std::collections::HashMap;

pub(crate) struct ThresholdSignature {
    proof: Proof,
    key_list: Vec<PublicKey>,
}

impl ThresholdSignature {
    pub(crate) fn aggregate<const N: usize>(
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
    fn validate_signatures(&self, msg: &[u8]) -> bool {
        let mut signatures = Vec::with_capacity(self.proof.element_sequence.len());
        for sig_bytes in &self.proof.element_sequence {
            let Ok(signature) = Signature::from_bytes(sig_bytes.as_slice()) else {
                return false;
            };
            signatures.push(signature);
        }
        let signature_refs: Vec<&Signature> = signatures.iter().collect();
        let Ok(aggregate_signature) =
            AggregateSignature::aggregate(signature_refs.as_slice(), false)
        else {
            return false;
        };
        let final_signature = aggregate_signature.to_signature();

        let public_key_refs: Vec<&PublicKey> = self.key_list.iter().collect();
        let Ok(aggregate_verification_key) =
            AggregatePublicKey::aggregate(public_key_refs.as_slice(), false)
        else {
            return false;
        };
        let final_verification_key = aggregate_verification_key.to_public_key();

        let result = final_signature.verify(false, msg, &[], &[], &final_verification_key, false);
        result == BLST_ERROR::BLST_SUCCESS
    }

    pub(crate) fn verify(&self, msg: &[u8], params: &Params) -> bool {
        if self.validate_signatures(msg) {
            let alba = CentralizedTelescope::create(params);
            return alba.verify(&self.proof);
        }
        false
    }
}
