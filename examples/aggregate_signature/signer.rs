use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use blst::min_sig::{PublicKey, SecretKey};
use rand_core::{CryptoRng, RngCore};
use std::cmp::Ordering;

/// Signature verification key, which is a wrapper over the blst `PublicKey`.
#[derive(Debug, Clone, Copy, Default)]
pub struct VerificationKey(pub(crate) PublicKey);

/// Threshold signature candidate signer
#[derive(Debug)]
pub struct Signer {
    /// Signing key of the type blst `SecretKey`
    signing_key: SecretKey,
    /// Signature verification key of the type blst `PublicKey`
    pub verification_key: VerificationKey,
}

/// Registered Threshold signature signer.
/// It includes signer's keypair and the registration check-sum.
#[derive(Debug)]
pub struct RegisteredSigner {
    signing_key: SecretKey,
    verification_key: VerificationKey,
    registration_check_sum: Vec<u8>,
}

impl VerificationKey {
    /// Convert a `VerificationKey` to its byte representation.
    pub fn to_bytes(self) -> [u8; 96] {
        self.0.to_bytes()
    }

    /// Compare `self` with the given `VerificationKey`.
    fn cmp_vk(&self, other: &VerificationKey) -> Ordering {
        let self_bytes = self.to_bytes();
        let other_bytes = other.to_bytes();
        let mut result = Ordering::Equal;

        for (i, j) in self_bytes.iter().zip(other_bytes.iter()) {
            result = i.cmp(j);
            if result != Ordering::Equal {
                return result;
            }
        }
        result
    }
}

impl PartialEq for VerificationKey {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for VerificationKey {}

impl PartialOrd for VerificationKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl Ord for VerificationKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_vk(other)
    }
}

impl Signer {
    /// Generate a new candidate, i.e., a bls signer
    pub fn new(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        let sk = SecretKey::key_gen(&ikm, &[])
            .expect("Error occurs when the length of ikm < 32. This will not happen here.");
        let vk = sk.sk_to_pk();
        Self {
            signing_key: sk,
            verification_key: VerificationKey(vk),
        }
    }

    /// Create a new signer from candidate and closed registration
    pub fn new_signer<const N: usize>(
        &self,
        registration: &Registration,
    ) -> Option<RegisteredSigner> {
        if registration.is_registered(&self.verification_key) {
            Some(RegisteredSigner {
                signing_key: self.signing_key.clone(),
                verification_key: self.verification_key,
                registration_check_sum: registration.check_sum.clone(),
            })
        } else {
            None
        }
    }
}

impl RegisteredSigner {
    /// Create a bls signature for given message.
    /// Signer creates a commitment by hashing the checksum of the closed registration and the message.
    /// The signature is generated by signing the commitment.
    pub fn sign<const N: usize>(&self, msg: &[u8]) -> IndividualSignature {
        let commitment: [u8; N] =
            Registration::get_commitment::<N>(&self.registration_check_sum, msg);

        IndividualSignature {
            signature: self.signing_key.sign(&commitment, &[], &[]),
            verification_key: self.verification_key,
        }
    }
}
