//! Use case for ALBA Telescope centralized setup.
//! BLS signatures are the elements to create an ALBA proof.

use blst::min_sig::{PublicKey, SecretKey, Signature};
use blst::BLST_ERROR;
use rand_chacha::ChaCha20Rng;
use rand_core::{CryptoRng, RngCore, SeedableRng};

// const DATA_LENGTH: usize = 48;

/// Key pair including blst `SecretKey` and blst `PublicKey`
#[derive(Debug)]
pub struct SIGNER {
    signing_key: SecretKey,
    /// Verification key
    pub verification_key: PublicKey,
}

/// A blst signature and its verification key of the type blst `PublicKey`
#[derive(Debug)]
pub struct BLS {
    /// Blst signature
    pub signature: Signature,
    /// Blst public key to verify the signature
    pub verification_key: PublicKey,
}

impl SIGNER {
    /// Generate a `KEYPAIR`
    pub fn new(rng: &mut (impl RngCore + CryptoRng)) -> Self {
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
    /// Sign the given message with the secret key of keypair
    pub fn sign(&self, msg: &[u8]) -> BLS {
        BLS {
            signature: self.signing_key.sign(msg, &[], &[]),
            verification_key: self.verification_key,
        }
    }
}

impl BLS {
    /// Verify a signature against its verification key for the given message.
    pub fn verify(&self, msg: &[u8]) -> bool {
        let result = self
            .signature
            .verify(false, msg, &[], &[], &self.verification_key, false);
        result == BLST_ERROR::BLST_SUCCESS
    }
    /// Converts the `BLS.signature` into a fixed-size byte array `[u8; N]`
    pub fn to_bytes<const N: usize>(&self) -> [u8; N] {
        let bytes = self.signature.to_bytes();
        let mut array = [0u8; N];

        for (i, &byte) in bytes.iter().take(N).enumerate() {
            array[i] = byte;
        }
        array
    }
}

/// Generate signatures
pub fn generate_signatures(
    rng: &mut (impl RngCore + CryptoRng),
    msg: &[u8],
    set_size: u64,
) -> Vec<BLS> {
    (0..set_size)
        .map(|_| {
            let signer = SIGNER::new(rng);
            signer.sign(msg)
        })
        .collect::<Vec<BLS>>()
}

/// Collect the valid signatures as byte arrays
pub fn collect_set_elements<const N: usize>(
    msg: &[u8],
    set_size: u64,
    signatures: Vec<BLS>,
) -> Option<Vec<[u8; N]>> {
    let prover_set = signatures
        .iter()
        .filter_map(|sig| {
            if sig.verify(msg) {
                Some(sig.to_bytes())
            } else {
                None
            }
        })
        .collect::<Vec<[u8; N]>>();
    if prover_set.len() < set_size as usize {
        None
    } else {
        Some(prover_set)
    }
}

fn main() {
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]); // create and initialize rng
    let mut msg = [0u8; 16]; // setting an arbitrary message
    rng.fill_bytes(&mut msg);

    let key_pair = SIGNER::new(&mut rng);
    let sig = key_pair.sign(&msg);
    let result = sig.verify(&msg);

    println!("{:?}", result);
}
