use crate::simple_threshold_signature::signature::Signature;
use crate::Element;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::Telescope;
use blst::min_sig::{PublicKey, Signature as BLSSignature};
use blst::BLST_ERROR;

pub(crate) struct ThresholdSignature {
    proof: Proof,
}

impl ThresholdSignature {
    pub(crate) fn aggregate(
        signatures: &[Signature],
        alba: &Telescope,
        public_key_list: &[(usize, PublicKey)],
    ) -> (Self, Vec<usize>) {
        let prover_set = signatures
            .iter()
            .map(|s| s.signature.to_bytes())
            .collect::<Vec<Element>>();
        println!("-- Creating alba proof. ");
        let proof = alba.prove(&prover_set).unwrap();
        println!("-- Alba proof created: ");
        println!(
            " - Numbers of retries done to find the proof: {}",
            proof.retry_counter
        );
        println!(
            " - Index of the searched subtree to find the proof: {}",
            proof.search_counter
        );
        println!(
            " - Number of elements in the proof sequence: {}",
            proof.element_sequence.len()
        );

        let proof_signatures: Vec<BLSSignature> = proof
            .element_sequence
            .iter()
            .filter_map(|element| BLSSignature::from_bytes(element).ok())
            .collect();

        let mut indices = Vec::with_capacity(proof_signatures.len());
        for sig in &proof_signatures {
            if let Some(signature_entry) = signatures.iter().find(|entry| entry.signature == *sig) {
                if public_key_list
                    .iter()
                    .any(|entry| entry.0 == signature_entry.index)
                {
                    indices.push(signature_entry.index);
                }
            }
        }
        (Self { proof }, indices)
    }

    /// Validates individual signatures in the threshold signature
    /// This function verifies each individual signature separately.
    /// The verification could also be done by using aggregation from blst library as follows:
    ///     - Aggregate signatures with: `AggregateSignature::aggregate`,
    ///     - Aggregate public keys with: `AggregatePublicKey::aggregate`,
    ///     - Convert aggregate signature to a signature and aggregate public key to a public key
    ///     - Verify the signature with public key against given message.
    fn validate_signatures(
        &self,
        msg: &[u8],
        public_key_list: &[(usize, PublicKey)],
        indices: &[usize],
    ) -> bool {
        let mut signatures = Vec::with_capacity(self.proof.element_sequence.len());
        for sig_bytes in &self.proof.element_sequence {
            let Ok(signature) = BLSSignature::from_bytes(sig_bytes.as_slice()) else {
                return false;
            };
            signatures.push(signature);
        }

        for (signature, &index) in signatures.iter().zip(indices.iter()) {
            if let Some((_, public_key)) = public_key_list.iter().find(|(idx, _)| *idx == index) {
                if signature.verify(false, msg, &[], &[], public_key, false)
                    != BLST_ERROR::BLST_SUCCESS
                {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }

    pub(crate) fn verify(
        &self,
        msg: &[u8],
        alba: &Telescope,
        public_key_list: &[(usize, PublicKey)],
        indices: &[usize],
    ) -> bool {
        self.validate_signatures(msg, public_key_list, indices) && alba.verify(&self.proof)
    }
}
