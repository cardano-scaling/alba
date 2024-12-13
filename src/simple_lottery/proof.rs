//! Simple lottery Proof structure

use super::params::Params;
use crate::utils::{
    sample,
    types::{Element, Hash},
};
use blake2::{Blake2s256, Digest};

/// Simple lottery proof
#[derive(Debug, Clone)]
pub struct Proof {
    /// Sequence of elements from prover's set
    pub element_sequence: Vec<Element>,
}

fn lottery_hash(lottery_probability: f64, element: Element) -> bool {
    let mut hasher = Blake2s256::new();
    hasher.update(element);
    let digest: Hash = hasher.finalize().into();
    sample::sample_bernoulli(&digest, lottery_probability)
}

impl Proof {
    /// Simple Lottery proving algorithm
    pub fn new(params: &Params, prover_set: &[Element]) -> Option<Self> {
        debug_assert!(crate::utils::misc::check_distinct(prover_set));

        let mut element_sequence = Vec::with_capacity(params.proof_size as usize);
        for &element in prover_set {
            if lottery_hash(params.lottery_probability, element) {
                element_sequence.push(element);
            }
            if prover_set.len() as u64 >= params.proof_size {
                element_sequence.sort_unstable();
                return Some(Proof { element_sequence });
            }
        }
        None
    }

    /// Simple Lottery verifying algorithm
    pub fn verify(&self, params: &Params) -> bool {
        (self.element_sequence.len() as u64 == params.proof_size)
            && self.element_sequence.is_sorted_by(|a, b| a < b)
            && self
                .element_sequence
                .iter()
                .all(|&element| lottery_hash(params.lottery_probability, element))
    }
}
