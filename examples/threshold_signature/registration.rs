use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use blst::min_sig::PublicKey;
use std::collections::HashMap;

/// A hash table for storing BLS public keys with their indices
#[derive(Debug, Clone)]
pub(crate) struct RegisteredKeys {
    keys: HashMap<usize, Vec<u8>>,
}

/// Closed registration including registered keys and the checksum of the registered keys
#[derive(Debug, Clone)]
pub(crate) struct ClosedRegistration {
    pub(crate) registered_keys: RegisteredKeys,
    pub(crate) check_sum: Vec<u8>,
}

impl RegisteredKeys {
    /// Creates a new hash table with a specified hash size
    pub fn new() -> Self {
        Self {
            keys: HashMap::new(),
        }
    }

    /// Inserts the given list of public keys into the hash table
    pub fn insert_keys<const N: usize>(&mut self, keys: &[PublicKey]) {
        for (index, key) in keys.iter().enumerate() {
            let key_hash = RegisteredKeys::hash_key::<N>(key);
            self.keys.insert(index, key_hash);
        }
    }

    /// Insert the given public key into the hash table
    pub fn insert_key<const N: usize>(&mut self, key: &PublicKey, index: usize) {
        let key_hash = RegisteredKeys::hash_key::<N>(key);
        self.keys.insert(index, key_hash);
    }

    /// Finds the index of the given public key, if it exists in the registered keys
    pub fn get_index<const N: usize>(&self, key: &PublicKey) -> Option<usize> {
        let key_hash = RegisteredKeys::hash_key::<N>(key);
        self.keys.iter().find_map(|(&index, stored_hash)| {
            if *stored_hash == key_hash {
                Some(index)
            } else {
                None
            }
        })
    }

    /// Checks if the hash of the given `PublicKey` matches the hash stored at the specified index
    pub fn is_key_at_index<const N: usize>(&self, key: &PublicKey, index: usize) -> bool {
        match self.keys.get(&index) {
            Some(stored_hash) => &RegisteredKeys::hash_key::<N>(key) == stored_hash,
            None => false,
        }
    }

    /// Hashes a public key using Blake2b
    fn hash_key<const N: usize>(key: &PublicKey) -> Vec<u8> {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut hash_output = vec![0u8; N];

        hasher.update(&key.to_bytes());
        hasher.finalize_variable(&mut hash_output).unwrap();

        hash_output
    }
}

impl Default for RegisteredKeys {
    fn default() -> Self {
        Self::new()
    }
}

impl ClosedRegistration {
    /// Close the registration and create the hash of all registered keys.
    pub fn close<const N: usize>(registration: &RegisteredKeys) -> Self {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut hash_output = vec![0u8; N];

        for reg in registration.clone().keys {
            hasher.update(&reg.1.clone());
        }
        hasher.finalize_variable(&mut hash_output).unwrap();

        Self {
            registered_keys: registration.clone(),
            check_sum: hash_output.clone(),
        }
    }

    /// Compute the commitment by hashing check sum of closed registration and the message
    pub fn get_commitment<const N: usize>(check_sum: &[u8], msg: &[u8]) -> [u8; N] {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut commitment: [u8; N] = [0u8; N];

        hasher.update(check_sum);
        hasher.update(msg);
        hasher.finalize_variable(&mut commitment).unwrap();
        commitment
    }
}
