use crate::aggregate_signature::helpers::get_commitment;
use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use blst::min_sig::{PublicKey, SecretKey};
use rand_core::{CryptoRng, RngCore};

/// Threshold signature signer
#[derive(Debug, Clone)]
pub(crate) struct Signer {
    /// Signature generation key
    signing_key: SecretKey,
    /// Signature verification key
    pub(crate) verification_key: PublicKey,
    /// Registration index of the signer
    pub(crate) index: usize,
    /// Closed registration checksum
    pub(crate) checksum: Option<Vec<u8>>,
}

impl Signer {
    /// Create signing key and verification key for a signer
    pub(crate) fn init(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);

        let sk = SecretKey::key_gen(&ikm, &[])
            .expect("Error: Key generation failed due to insufficient IKM length. This should never happen.");
        let vk = sk.sk_to_pk();

        Self {
            signing_key: sk,
            verification_key: vk,
            index: 0,
            checksum: None,
        }
    }

    /// Register signer's verification key. If the registration is not closed, i.e., no checksum found and the
    /// signer's verification key is already registered return `None`. Otherwise, insert new verification key with a
    /// new index. Return the `RegisteredSigner`.
    pub(crate) fn register(&mut self, registration: &mut Registration) -> bool {
        if registration.checksum.is_none() {
            if registration
                .registered_keys
                .values()
                .any(|v| *v == self.verification_key)
            {
                println!("Error: Key already registered!");
                return false;
            }
            let index = registration.registered_keys.len().saturating_add(1);
            registration
                .registered_keys
                .insert(index, self.verification_key);
            self.index = index;
            true
        } else {
            println!("Error: Cannot register, registration is closed!");
            false
        }
    }

    /// Get closed registration. Update the registered signer with the checksum of registration.
    pub(crate) fn get_closed_registration(&mut self, registration: &Registration) {
        match &registration.checksum {
            Some(checksum) => {
                if registration.registered_keys.contains_key(&self.index) {
                    self.checksum = Some(checksum.clone());
                }
            }
            None => {
                println!("Error: Registration is not closed.");
            }
        }
    }

    /// Create an individual signature by signing `commitment = Hash(checksum || msg)`
    pub(crate) fn sign<const N: usize>(&self, msg: &[u8]) -> Option<IndividualSignature> {
        let commitment = self
            .checksum
            .as_ref()
            .map(|checksum| get_commitment::<N>(checksum, msg));

        commitment.map(|commitment| IndividualSignature {
            signature: self.signing_key.sign(&commitment, &[], &[]),
            index: self.index,
        })
    }
}
