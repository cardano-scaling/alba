use crate::aggregate_signature::helpers::get_commitment;
use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use blst::min_sig::{PublicKey, SecretKey};
use rand_core::{CryptoRng, RngCore};

/// Threshold signature candidate signer
#[derive(Debug, Clone)]
pub struct Signer {
    /// Signature generation key
    signing_key: SecretKey,
    /// Signature verification key
    pub(crate) verification_key: PublicKey,
}

/// Registered threshold signature signer
#[derive(Debug, Clone)]
pub(crate) struct RegisteredSigner {
    /// Signature generation key
    signing_key: SecretKey,
    /// Registration index of the signer
    pub(crate) index: usize,
    /// Closed registration checksum
    checksum: Vec<u8>,
}

impl Signer {
    /// Create signing key and verification key of a signer
    pub(crate) fn init(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        let sk = SecretKey::key_gen(&ikm, &[])
            .expect("Error occurs when the length of ikm < 32. This will not happen here.");
        let vk = sk.sk_to_pk();
        Self {
            signing_key: sk,
            verification_key: vk,
        }
    }

    /// Create a new registered signer if signer's key exists in closed registration
    pub(crate) fn new_signer<const N: usize>(
        &self,
        registration: &Registration,
    ) -> Option<RegisteredSigner> {
        if let Some(index) = registration.get_index_of_key(&self.verification_key) {
            Some(RegisteredSigner {
                signing_key: self.signing_key.clone(),
                index,
                checksum: registration.checksum.clone().unwrap(),
            })
        } else {
            None
        }
    }
}

impl RegisteredSigner {
    /// Create an individual signature by signing `commitment = Hash(checksum || msg)`
    pub(crate) fn sign<const N: usize>(&self, msg: &[u8]) -> IndividualSignature {
        let commitment = get_commitment::<N>(&self.checksum, msg).to_vec();

        IndividualSignature {
            signature: self.signing_key.sign(&commitment, &[], &[]),
            index: self.index,
        }
    }
}
