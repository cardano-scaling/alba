use crate::simple_threshold_signature::signature::Signature;
use blst::min_sig::{PublicKey, SecretKey};
use rand_core::CryptoRng;

pub(crate) struct Signer {
    signing_key: SecretKey,
    pub(crate) verification_key: PublicKey,
}

impl Signer {
    /// Create a pair of bls signing key and verification key
    pub(crate) fn new(rng: &mut impl CryptoRng) -> Self {
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

    /// Sign given message. Return the signature and given signer index.
    pub(crate) fn sign(&self, msg: &[u8], index: usize) -> Signature {
        Signature {
            signature: self.signing_key.sign(msg, &[], &[]),
            index,
        }
    }
}
