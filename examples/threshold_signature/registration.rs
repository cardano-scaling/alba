use crate::threshold_signature::signer::VerificationKey;
use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use std::collections::BTreeSet;

type Keys = BTreeSet<VerificationKey>;

#[derive(Debug, Clone)]
pub(crate) struct Registration {
    pub(crate) registered_keys: Keys,
    pub(crate) check_sum: Vec<u8>,
}

impl Registration {
    pub fn new() -> Self {
        Self {
            registered_keys: BTreeSet::new(),
            check_sum: Vec::new(),
        }
    }

    pub fn register(&mut self, key: VerificationKey) -> Option<&Self> {
        if self.registered_keys.insert(key) {
            Some(self)
        } else {
            println!("Key already registered!");
            None
        }
    }

    /// Close the registration and create the hash of all registered keys.
    pub fn close<const N: usize>(&mut self) {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut hash_output = vec![0u8; N];

        for key in &self.registered_keys {
            hasher.update(key.to_bytes().as_slice())
        }

        hasher.finalize_variable(&mut hash_output).unwrap();
        self.check_sum = hash_output.clone();
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

    pub fn is_registered(&self, verification_key: &VerificationKey) -> bool {
        if self.registered_keys.contains(verification_key) {
            return true;
        }
        return false;
    }
}
