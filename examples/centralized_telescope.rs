//! Use case for ALBA Telescope centralized setup.
//! BLS signatures are the elements to create an ALBA proof.

use blst::BLST_ERROR;
use blst::min_sig::{
    PublicKey, SecretKey,
    Signature,
};
use rand_chacha::ChaCha20Rng;
use rand_core::{CryptoRng, RngCore, SeedableRng};


/// Key pair including blst `SecretKey` and blst `PublicKey`
#[derive(Debug)]
pub struct KEYPAIR{
    sk: SecretKey,
    /// Verification key
    pub vk: PublicKey,
}

/// A blst signature and its verification key of the type blst `PublicKey`
#[derive(Debug)]
pub struct SIGNATURE {
    /// Blst signature
    pub signature: Signature,
    /// Blst public key to verify the signature
    pub vk: PublicKey,
}

impl KEYPAIR {
    /// Generate a `KEYPAIR`
    pub fn new(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        let sk = SecretKey::key_gen(&ikm, &[])
                .expect("Error occurs when the length of ikm < 32. This will not happen here.");
        let vk = sk.sk_to_pk();
        Self {
            sk,
            vk,
        }
    }
    /// Sign a message with the given secret key
    pub fn sign(&self, msg: &[u8]) -> SIGNATURE {
        SIGNATURE{
            signature: self.sk.sign(msg, &[], &[]),
            vk: self.vk,
        }

    }
}

impl SIGNATURE{
    /// Verify a signature against its verification key for the given message.
    pub fn verify(&self, msg: &[u8]) -> bool {
        let result = self.signature.verify(false, msg, &[], &[], &self.vk, false);
        result == BLST_ERROR::BLST_SUCCESS
    }
}



fn main() {
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]); // create and initialize rng
    let mut msg = [0u8; 16]; // setting an arbitrary message
    rng.fill_bytes(&mut msg);

    let key_pair = KEYPAIR::new(&mut rng);
    let sig = key_pair.sign(&msg);
    // let result = sig.verify(&msg);

    let element = sig.signature.clone();
    println!("{ }", element.to_bytes().len());

}

