use crate::simple_threshold_signature::signature::Signature;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::Telescope;
use blst::{
    min_sig::{PublicKey, Signature as BlsSignature},
    BLST_ERROR,
};
use digest::{Digest, FixedOutput};

const SIG_LENGTH: usize = 48;
pub(crate) type SigBytes = [u8; SIG_LENGTH];

pub(crate) struct ThresholdSignature<H: Digest + FixedOutput> {
    proof: Proof<SigBytes, H>,
}

impl<H: Digest + FixedOutput> ThresholdSignature<H> {
    // Create the Alba proof. Convert signatures to bytes and use them as the prover set. Create Alba proof with the
    // prover set. Collect signatures by converting proof elements to bls signatures. Find each signature's index and
    // collect them in a list. Return alba proof and the indices.
    pub(crate) fn aggregate(
        signatures: &[Signature],
        alba: &Telescope,
        public_key_list: &[(usize, PublicKey)],
    ) -> (Self, Vec<usize>) {
        // Convert signatures to bytes and collect as the prover set.
        let prover_set: Vec<SigBytes> = signatures.iter().map(|s| s.signature.to_bytes()).collect();

        println!("-- Creating alba proof. ");
        // Create alba proof with the prover set
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

        // Convert proof elements to signatures to obtain their indexes
        let proof_signatures: Vec<BlsSignature> = proof
            .element_sequence
            .iter()
            .filter_map(|element| BlsSignature::from_bytes(element).ok())
            .collect();

        // Collect the indices of the signatures that create alba proof.
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
        // Get the bls signatures from byte representation
        for sig_bytes in &self.proof.element_sequence {
            let Ok(signature) = BlsSignature::from_bytes(sig_bytes.as_slice()) else {
                return false;
            };
            signatures.push(signature);
        }

        // Find the public key from the public key lest for the corresponding index (and signature)
        // Verify the signature with this public key against given message.
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

    /// Verify `ThresholdSignature` by validating the signatures included in alba proof and verifying the alba proof.
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
