use crate::threshold_signature::signer::VerificationKey;
use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use std::collections::BTreeSet;

type Keys = BTreeSet<VerificationKey>;

/// Structure for registration functionality. It hold a `BTreeSet` of registered keys and the check
/// sum of all registry data.
#[derive(Debug, Clone)]
pub(crate) struct Registration {
    pub(crate) registered_keys: Keys,
    pub(crate) check_sum: Vec<u8>,
}

impl Registration {
    /// Initialize the key registration.
    pub fn new() -> Self {
        Self {
            registered_keys: BTreeSet::new(),
            check_sum: Vec::new(),
        }
    }

    /// Register the given `VerificationKey` if it is not registered already.
    /// Returns true if registration succeeds.
    pub fn register(&mut self, key: VerificationKey) -> bool {
        if self.registered_keys.insert(key) {
            true
        } else {
            println!("Key already registered!");
            false
        }
    }

    /// Close the registration and create the hash of all registered keys.
    pub fn close<const N: usize>(&mut self) {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut hash_output = vec![0u8; N];

        for key in &self.registered_keys {
            hasher.update(key.to_bytes().as_slice());
        }

        hasher.finalize_variable(&mut hash_output).unwrap();
        self.check_sum.clone_from(&hash_output);
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

    /// Return `true` if given `VerificationKey` is registered.
    pub fn is_registered(&self, verification_key: &VerificationKey) -> bool {
        self.registered_keys.contains(verification_key)
    }
}
