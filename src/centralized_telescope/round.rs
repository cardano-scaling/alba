//! ALBA's Round structure and associated functions

#![doc = include_str!("../../docs/centralized_telescope/round.md")]

use super::types::Hash;
use crate::utils::sample;
use crate::utils::types::Element;
use blake2::{Blake2s256, Digest};

/// Round parameters
#[derive(Debug, Clone)]
pub struct Round {
    /// Numbers of retries done so far
    pub retry_counter: u64,
    /// Index of the current subtree being searched
    pub search_counter: u64,
    /// Candidate element sequence
    pub element_sequence: Vec<Element>,
    /// Candidate round hash
    pub hash: Hash,
    /// Candidate round id, i.e. round hash mapped to [1, set_size]
    pub id: u64,
    /// Approximate size of prover set to lower bound
    pub set_size: u64,
}

impl Round {
    /// Output a round from retry and search counters as well as set_size
    /// Initilialises the hash with round_hash(retry_counter || search_bytes)
    /// and random value as oracle(round_hash(retry_counter || search_bytes), set_size)
    pub(super) fn new(retry_counter: u64, search_counter: u64, set_size: u64) -> Option<Self> {
        let retry_bytes: [u8; 8] = retry_counter.to_be_bytes();
        let search_bytes: [u8; 8] = search_counter.to_be_bytes();
        let (hash, id_opt) = Self::round_hash(&retry_bytes, &search_bytes, set_size);
        id_opt.map(|id| Self {
            retry_counter,
            search_counter,
            element_sequence: Vec::new(),
            hash,
            id,
            set_size,
        })
    }

    /// Updates a round with an element
    /// Replaces the hash $h$ with $h' = round_hash(h, s)$ and the random value as oracle(h', set_size)
    pub(super) fn update(r: &Self, element: Element) -> Option<Self> {
        let mut element_sequence = r.element_sequence.clone();
        element_sequence.push(element);
        let (hash, id_opt) = Self::round_hash(&r.hash, &element, r.set_size);
        id_opt.map(|id| Self {
            retry_counter: r.retry_counter,
            search_counter: r.search_counter,
            element_sequence,
            hash,
            id,
            set_size: r.set_size,
        })
    }

    /// Oracle producing a uniformly random value in [0, set_size[ used for round candidates
    /// We also return hash(data) to follow the optimization presented in Section 3.3
    fn round_hash(first_input: &[u8], second_input: &[u8], set_size: u64) -> (Hash, Option<u64>) {
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-round_hash");
        hasher.update(first_input);
        hasher.update(second_input);
        let digest: Hash = hasher.finalize().into();
        (digest, sample::sample_uniform(&digest, set_size))
    }
}
