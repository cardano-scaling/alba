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

    /// Register a new key if registration is open and the key is not already registered.
    pub(crate) fn register(&mut self, key: PublicKey, index: usize) -> bool {
        if self.checksum.is_some() {
            println!("Registration is closed!");
            return false;
        }

        if self.registered_keys.values().any(|v| *v == key) {
            println!("Key already registered!");
            return false;
        }

        self.registered_keys.insert(index, key);
        true
    }

    /// Get the index of a given key if registration is closed.
    pub(crate) fn get_index_of_key(&self, verification_key: &PublicKey) -> Option<usize> {
        if self.checksum.is_none() {
            println!("Registration is not closed.");
            return None;
        }

        self.registered_keys
            .iter()
            .find_map(|(index, key)| if key == verification_key { Some(*index) } else { None })
            .or_else(|| {
                println!("Signer is not registered.");
                None
            })
    }

    /// Close the registration by computing a checksum if it is not already closed.
    pub(crate) fn close<const N: usize>(&mut self) {
        if self.checksum.is_some() {
            println!("Registration is already closed.");
            return;
        }

        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut hash_output = vec![0u8; N];

        for key in self.registered_keys.values() {
            hasher.update(key.to_bytes().as_slice());
        }

        hasher.finalize_variable(&mut hash_output).expect("Hash finalization failed");
        self.checksum = Some(hash_output);
    }
}