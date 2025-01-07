use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::{IndividualSignature, Signature};
use crate::aggregate_signature::signer::VerificationKey;
use crate::Element;
use blst::BLST_ERROR;
use std::collections::HashSet;

/// Aggregate signature storing the list of valid signatures and the hash of commitment with the message to be signed.
#[derive(Debug, Clone)]
pub(crate) struct AggregateSignature {
    /// A list of verified signatures
    pub(crate) valid_signatures: Vec<IndividualSignature>,
    /// The commitment = Hash(checksum||msg) that is used by signing
    pub(crate) commitment: Vec<u8>,
}

impl AggregateSignature {
    /// Verify the aggregate
    /// Check whether the verification key for each signature is registered.
    /// Collect signatures and verification keys into vectors
    /// Verify the signatures with verification keys against the message with `verify_aggregate`
    pub fn verify<const N: usize>(&self, registration: &Registration) -> bool {
        for sig in &self.valid_signatures {
            if !sig.verification_key.is_registered(registration) {
                return false;
            }
        }

        let (signatures, verification_keys) = self.extract_signatures_and_keys();
        let result = Signature::verify_aggregate(
            signatures.as_slice(),
            verification_keys.as_slice(),
            &self.commitment,
        );
        result == BLST_ERROR::BLST_SUCCESS
    }

    /// Return a vector of signatures and a vector of verification keys of the valid signatures
    fn extract_signatures_and_keys(&self) -> (Vec<Signature>, Vec<VerificationKey>) {
        let signatures = self
            .valid_signatures
            .iter()
            .map(|ind_sig| ind_sig.signature)
            .collect();

        let verification_keys = self
            .valid_signatures
            .iter()
            .map(|ind_sig| ind_sig.verification_key)
            .collect();

        (signatures, verification_keys)
    }

    /// Collect the individual signatures whose element versions are contained in the alba proof
    pub(crate) fn aggregate(
        signatures: &[IndividualSignature],
        prover_set: &[Element],
        element_sequence: &[Element],
        commitment: &[u8],
    ) -> Self {
        let proof_elements: HashSet<&Element> = element_sequence.iter().collect();
        let proof_signatures = prover_set
            .iter()
            .zip(signatures.iter())
            .filter(|(element, _)| proof_elements.contains(element))
            .map(|(_, signature)| signature.clone())
            .collect();
        Self {
            valid_signatures: proof_signatures,
            commitment: commitment.to_vec(),
        }
    }
}
