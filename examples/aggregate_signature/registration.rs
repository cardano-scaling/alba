use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use blst::min_sig::PublicKey;
use std::collections::BTreeMap;

type Keys = BTreeMap<usize, PublicKey>;

/// Structure for registration functionality.
#[derive(Debug, Clone)]
pub(crate) struct Registration {
    /// Registered keys
    pub(crate) registered_keys: Keys,
    /// Checksum of registry data
    pub(crate) checksum: Option<Vec<u8>>,
}

impl Registration {
    /// Initialize key registration
    pub(crate) fn new() -> Self {
        Self {
            registered_keys: BTreeMap::new(),
            checksum: None,
        }
    }

    /// Register new key if the registration is not closed and the key is not already registered
    pub(crate) fn register(&mut self, key: PublicKey, index: usize) -> bool {
        if self.checksum.is_none() {
            return if self.registered_keys.values().any(|v| *v == key) {
                println!("Key already registered!");
                false
            } else {
                self.registered_keys.insert(index, key);
                true
            };
        }
        println!("Registration is closed!");
        return false;
    }

    /// Get the index of given key if the registration is closed
    pub(crate) fn get_index_of_key(&self, verification_key: &PublicKey) -> Option<usize> {
        if self.checksum.is_some() {
            for (index, key) in &self.registered_keys {
                if key == verification_key {
                    return Some(*index);
                }
            }
            println!("Signer is not registered.");
            return None;
        }
        println!("Registration is not closed.");
        return None;
    }

    /// Close key registration if it is not already closed
    pub(crate) fn close<const N: usize>(&mut self) {
        if self.checksum.is_none() {
            let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
            let mut hash_output = vec![0u8; N];

            self.registered_keys.iter().for_each(|participant| {
                hasher.update(participant.1.to_bytes().as_slice());
            });

            hasher.finalize_variable(&mut hash_output).unwrap();
            self.checksum = Some(hash_output);
        } else {
            println!("Registration is already closed.");
        }
    }
}
