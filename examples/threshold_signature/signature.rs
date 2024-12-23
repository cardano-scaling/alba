use crate::threshold_signature::registration::ClosedRegistration;
use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use blst::min_sig::{PublicKey, Signature};
use blst::BLST_ERROR;

/// Individual Signature.
/// It includes a blst signature, its verification key, and the registration index of the signer.
#[derive(Debug, Clone)]
pub(crate) struct IndividualSignature {
    pub(crate) signature: Signature,
    pub(crate) verification_key: PublicKey,
    pub(crate) signer_index: usize,
}

impl IndividualSignature {
    /// Verify a signature
    /// First, validate that the signer's verification key is actually registered.
    /// Then, verify the blst signature.
    pub fn verify<const N: usize>(
        &self,
        commitment: &[u8],
        closed_registration: &ClosedRegistration,
    ) -> bool {
        if closed_registration
            .registered_keys
            .is_key_at_index::<N>(&self.verification_key, self.signer_index)
        {
            let result =
                self.signature
                    .verify(false, commitment, &[], &[], &self.verification_key, false);
            return result == BLST_ERROR::BLST_SUCCESS;
        };
        false
    }

    /// Return the hash of the signature and its public key
    /// This function is used to create the `prover_set` of Alba protocol.
    pub fn to_element<const N: usize>(&self) -> [u8; N] {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut element = [0u8; N];

        hasher.update(&self.signature.to_bytes());
        hasher.update(&self.verification_key.to_bytes());
        hasher.finalize_variable(&mut element).unwrap();

        element
    }
}
