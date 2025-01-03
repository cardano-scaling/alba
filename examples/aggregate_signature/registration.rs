use crate::aggregate_signature::signer::VerificationKey;
use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use std::collections::BTreeSet;

type Keys = BTreeSet<VerificationKey>;

/// Structure for registration functionality. It hold a `BTreeSet` of registered keys and the check
/// sum of all registry data.
#[derive(Debug, Clone)]
pub(crate) struct Registration {
    pub(crate) registered_keys: Keys,
    pub(crate) check_sum: Option<Vec<u8>>,
}

impl Registration {
    /// Initialize the key registration.
    pub fn new() -> Self {
        Self {
            registered_keys: BTreeSet::new(),
            check_sum: None,
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
        if self.check_sum.is_none() {
            let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
            let mut hash_output = vec![0u8; N];

            self.registered_keys.iter().for_each(|key| {
                hasher.update(key.to_bytes().as_slice());
            });

            hasher.finalize_variable(&mut hash_output).unwrap();
            self.check_sum = Some(hash_output);
        } else {
            println!("Registration is already closed.");
        }
    }

    /// Compute the commitment by hashing the checksum of the closed registration and the message.
    /// Returns `None` if the registration is not closed.
    pub fn get_commitment<const N: usize>(&self, msg: &[u8]) -> Option<[u8; N]> {
        self.check_sum.as_ref().map(|check_sum| {
            let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
            let mut commitment = [0u8; N];

            hasher.update(check_sum);
            hasher.update(msg);
            hasher.finalize_variable(&mut commitment).unwrap();
            commitment
        })
    }
}
