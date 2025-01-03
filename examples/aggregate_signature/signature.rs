use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signer::VerificationKey;
use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use blst::min_sig::Signature as BlstSignature;
use blst::BLST_ERROR;

/// Signature, which is a wrapper over the `BlstSignature` type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Signature(pub BlstSignature);

/// Individual Signature.
/// It includes a BLS signature, using the blst library, its verification key, and the registration index of the signer.
#[derive(Debug, Clone)]
pub(crate) struct IndividualSignature {
    /// Individual signature of type blst `Signature`
    pub(crate) signature: Signature,
    /// Verification key (wrapper over the blst `PublicKey`) of the individual signature.
    pub(crate) verification_key: VerificationKey,
}


impl Signature {
    pub fn verify(&self, msg: &[u8], verification_key: &VerificationKey) -> BLST_ERROR {
        self.0.verify(false, msg, &[], &[], &verification_key.0, false)
    }
    /// Convert an `Signature` to its byte representation.
    pub fn to_bytes(self) -> [u8; 48] {
        self.0.to_bytes()
    }
    pub fn verify_aggregate(signatures: &[Signature], verification_keys: &[VerificationKey], msg: &[u8])  -> BLST_ERROR {
        signatures[0].verify(msg, &verification_keys[0])
    }
}

impl IndividualSignature {
    /// Verify a signature
    /// First, validate that the signer's verification key is actually registered.
    /// Then, verify the blst signature against the given `commitment` (Hash(checksum||msg)).
    pub fn verify<const N: usize>(&self, registration: &Registration, msg: &[u8]) -> bool {
        let commitment: [u8; N] = match registration.get_commitment::<N>(msg) {
            Some(commitment) => commitment,
            None => return false,
        };
        if self.verification_key.is_registered(registration) {
            let result = self.signature.verify(
                &commitment, &self.verification_key);
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
