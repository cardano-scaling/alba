//! Example

// REMOVE!!!!!!!!!!!!!!!!
#![allow(dead_code)]

use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use blst::min_sig::{PublicKey, SecretKey, Signature};
use blst::BLST_ERROR;
use rand_chacha::ChaCha20Rng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use std::collections::HashMap;

const DATA_LENGTH: usize = 32;

/// A hash table for storing BLS public keys with their indices
#[derive(Debug, Clone)]
pub struct RegisteredKeys {
    keys: HashMap<Vec<u8>, usize>,
}

/// Closed registration including registered keys and the commitment
#[derive(Debug, Clone)]
pub struct ClosedRegistration {
    registered_keys: RegisteredKeys,
    commitment: Vec<u8>,
}

/// Key pair including blst `SecretKey` and blst `PublicKey`
#[derive(Debug)]
pub struct Candidate {
    signing_key: SecretKey,
    /// Verification key
    pub verification_key: PublicKey,
}

/// Registered Alba signer. It includes signer's keypair, registration index and the commitment.
#[derive(Debug)]
pub struct Signer {
    signing_key: SecretKey,
    verification_key: PublicKey,
    signer_index: usize,
    commitment: Vec<u8>,
}

/// Individual Signature including blst `PublicKey` and blst `Signature`
#[derive(Debug, Clone)]
pub struct IndividualSignature {
    signature: Signature,
    verification_key: PublicKey,
}

/// Aggregate signature storing the list of valid signatures and the hash of commitment with the message to be signed.
#[derive(Debug, Clone)]
pub struct AggregateSignature {
    valid_signatures: Vec<IndividualSignature>,
    commitment_with_msg: Vec<u8>,
}

impl Candidate {
    /// Generate a new candidate
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

    /// Extract the verification key.
    pub fn verification_key(&self) -> PublicKey {
        self.verification_key
    }

    /// Create a new signer from candidate and closed registration
    pub fn new_signer(&self, closed_registration: &ClosedRegistration) -> Option<Signer> {
        closed_registration
            .registered_keys
            .find_key(&self.verification_key())
            .map(|index| Signer {
                signing_key: self.signing_key.clone(),
                verification_key: self.verification_key,
                signer_index: index,
                commitment: closed_registration.commitment.clone(),
            })
    }
}

impl Default for RegisteredKeys {
    fn default() -> Self {
        Self::new()
    }
}

impl RegisteredKeys {
    /// Creates a new hash table with a specified hash size
    pub fn new() -> Self {
        Self {
            keys: HashMap::new(),
        }
    }

    /// Inserts a list of public keys into the hash table
    pub fn insert_keys(&mut self, keys: &[PublicKey]) {
        for (index, key) in keys.iter().enumerate() {
            let key_hash = RegisteredKeys::hash_key(key);
            self.keys.insert(key_hash, index);
        }
    }

    /// Searches for a public key and returns its index if found
    pub fn find_key(&self, key: &PublicKey) -> Option<usize> {
        let key_hash = RegisteredKeys::hash_key(key);
        self.keys.get(&key_hash).copied()
    }

    /// Hashes a public key using Blake2b
    fn hash_key(key: &PublicKey) -> Vec<u8> {
        let mut hasher = Blake2bVar::new(DATA_LENGTH).expect("Invalid hash size");
        hasher.update(&key.to_bytes());
        let mut hash_output = vec![0u8; DATA_LENGTH];
        hasher.finalize_variable(&mut hash_output).unwrap();
        hash_output
    }
}

impl ClosedRegistration {
    /// Close the registration and create the hash of all registration data.
    pub fn close(registration: &RegisteredKeys) -> Self {
        let mut hasher = Blake2bVar::new(DATA_LENGTH).expect("Invalid hash size");
        for reg in registration.clone().keys {
            hasher.update(&reg.0.clone());
        }
        let mut hash_output = vec![0u8; DATA_LENGTH];
        hasher.finalize_variable(&mut hash_output).unwrap();
        Self {
            registered_keys: registration.clone(),
            commitment: hash_output.clone(),
        }
    }
}

impl Signer {
    /// Create a bls signature for given message
    fn sign(&self, msg: &[u8]) -> IndividualSignature {
        let mut hasher = Blake2bVar::new(DATA_LENGTH).expect("Invalid hash size");
        hasher.update(&self.commitment.clone());
        hasher.update(&msg);

        let mut msg_to_sign = vec![0u8; DATA_LENGTH];
        hasher.finalize_variable(&mut msg_to_sign).unwrap();

        IndividualSignature {
            signature: self.signing_key.sign(&msg_to_sign, &[], &[]),
            verification_key: self.verification_key,
        }
    }
}

impl IndividualSignature {
    /// Verify a signature
    pub fn verify(&self, msg: &[u8], closed_registration: &ClosedRegistration) -> bool {
        if closed_registration
            .registered_keys
            .find_key(&self.verification_key)
            .is_some()
        {
            let mut signed_msg = vec![0u8; DATA_LENGTH];
            let mut hasher = Blake2bVar::new(DATA_LENGTH).expect("Invalid hash size");
            hasher.update(&closed_registration.commitment.clone());
            hasher.update(&msg);
            hasher.finalize_variable(&mut signed_msg).unwrap();

            let result =
                self.signature
                    .verify(false, &signed_msg, &[], &[], &self.verification_key, false);
            return result == BLST_ERROR::BLST_SUCCESS;
        };
        false
    }

    /// Return the hash of the signature and its public key
    pub fn to_element<const N: usize>(&self) -> [u8; N] {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut element = [0u8; N];
        hasher.update(&self.signature.to_bytes());
        hasher.update(&self.verification_key.to_bytes());
        hasher.finalize_variable(&mut element).unwrap();
        element
    }
}

impl AggregateSignature {
    /// Collect valid individual signatures and create the commitment with message
    pub fn collect_valid_signatures<const N: usize>(
        signatures: Vec<IndividualSignature>,
        closed_registration: &ClosedRegistration,
        msg: &[u8],
        set_size: u64,
    ) -> Option<Self> {
        let valid_signatures: Vec<IndividualSignature> = signatures
            .iter()
            .filter(|sig| sig.verify(msg, closed_registration))
            .cloned()
            .collect();
        if valid_signatures.len() < set_size as usize {
            None
        } else {
            let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
            hasher.update(&closed_registration.commitment.clone());
            hasher.update(&msg);

            let mut commitment_with_msg = vec![0u8; N];
            hasher.finalize_variable(&mut commitment_with_msg).unwrap();
            Some(Self {
                valid_signatures,
                commitment_with_msg,
            })
        }
    }

    /// Create the prover set by running `to_element` function for each valid signature
    pub fn create_prover_set<const N: usize>(&self) -> Vec<[u8; N]> {
        self.valid_signatures
            .iter()
            .map(IndividualSignature::to_element)
            .collect()
    }
}

fn main() {
    let mut rng = ChaCha20Rng::from_seed(Default::default());
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);
    let set_size = 1_000;

    let mut candidates: Vec<Candidate> = Vec::with_capacity(set_size as usize);

    for _ in 0..set_size {
        candidates.push(Candidate::new(&mut rng));
    }

    let public_keys = candidates
        .iter()
        .map(|candidate| candidate.verification_key)
        .collect::<Vec<PublicKey>>();

    let mut registration = RegisteredKeys::new();
    registration.insert_keys(&public_keys);

    let closed_registration = ClosedRegistration::close(&registration);

    let mut signers: Vec<Signer> = Vec::new();

    for candidate in candidates {
        match candidate.new_signer(&closed_registration) {
            Some(signer) => signers.push(signer),
            None => continue,
        }
    }
    let signatures = signers
        .iter()
        .map(|signer| signer.sign(&msg))
        .collect::<Vec<IndividualSignature>>();

    let aggregate = AggregateSignature::collect_valid_signatures::<DATA_LENGTH>(
        signatures,
        &closed_registration,
        &msg,
        set_size,
    )
    .unwrap();
    let s_p: Vec<[u8; DATA_LENGTH]> = aggregate.create_prover_set();
    println!("{}", s_p.len());
}
