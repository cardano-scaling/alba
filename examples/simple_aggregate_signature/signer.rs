use blst::min_sig::{PublicKey, SecretKey};
use rand_core::{CryptoRng, RngCore};

pub(crate) struct Signer {
    signing_key: SecretKey,
    pub(crate) verification_key: PublicKey,
}

impl Signer {
    pub(crate) fn new(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        let sk = SecretKey::key_gen(&ikm, &[])
            .expect("Error occurs when the length of ikm < 32. This will not happen here.");
        let pk: PublicKey = sk.sk_to_pk();
        Self {
            signing_key: sk,
            verification_key: pk,
        }
    }

    pub(crate) fn sign(&self, msg: &[u8]) -> [u8; 48] {
        let mut signature_to_byte = [0u8; 48];
        signature_to_byte.copy_from_slice(&self.signing_key.sign(msg, &[], &[]).to_bytes());
        signature_to_byte
    }
}
