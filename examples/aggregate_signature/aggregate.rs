use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::{IndividualSignature, Signature};
use crate::aggregate_signature::signer::VerificationKey;
use crate::Element;
use blst::BLST_ERROR;

/// Aggregate signature storing the list of valid signatures and the hash of commitment
/// with the message to be signed.
#[derive(Debug, Clone)]
pub(crate) struct AggregateSignature {
    /// A list of verified signatures
    pub(crate) valid_signatures: Vec<IndividualSignature>,
    /// The commitment = Hash(checksum||msg) that is used by signing
    pub(crate) commitment: Vec<u8>,
}

impl AggregateSignature {
    /// Create the aggregate signature.
    /// First, create the commitment by hashing the checksum of the closed registration and the message.
    /// Verify all individual signatures
    /// Return the aggregate if all checks pass.
    pub fn aggregate<const N: usize>(
        signatures: &[IndividualSignature],
        registration: &Registration,
        msg: &[u8],
        _set_size: u64,
    ) -> Option<Self> {
        let commitment: [u8; N] = registration.get_commitment(msg)?;

        let valid_signatures = signatures
            .iter()
            .filter(|sig| sig.verify::<N>(registration, msg))
            .cloned()
            .collect();

        Some(Self {
            valid_signatures,
            commitment: commitment.to_vec(),
        })
    }

    /// Verify the aggregate
    /// Check whether the verification key for each signature is registered.
    /// Collect signatures and verification keys into vectors
    /// Verify the signatures with verification keys against
    /// the message with `verify_aggregate`
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

    /// Return a vector of signatures and a vector of verification keys of
    /// the valid signatures
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

    /// Create the prover set by running `to_element` function for each valid signature
    pub fn create_prover_set<const N: usize>(&self) -> Vec<Element> {
        self.valid_signatures
            .iter()
            .map(IndividualSignature::to_element)
            .collect()
    }
}
