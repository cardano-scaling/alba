//! Simple lottery prove and verify functions

use super::proof::Proof;
use super::params::Setup;
use crate::utils::{
    sample,
    types::{Element, Hash},
};
use blake2::{Blake2s256, Digest};

pub(super) fn prove(setup: &Setup, prover_set: &[Element]) -> Option<Proof> {
    debug_assert!(crate::utils::misc::check_distinct(prover_set));

    let mut element_sequence = Vec::with_capacity(setup.proof_size as usize);
    for &element in prover_set {
        if lottery_hash(setup, element) {
            element_sequence.push(element);
        }
        if prover_set.len() as u64 >= setup.proof_size {
            element_sequence.sort_unstable();
            return Some(Proof { element_sequence });
        }
    }

    None
}

pub(super) fn verify(setup: &Setup, proof: &Proof) -> bool {
    (proof.element_sequence.len() as u64 == setup.proof_size)
        && proof.element_sequence.is_sorted_by(|a, b| a < b)
        && proof
            .element_sequence
            .iter()
            .all(|&element| lottery_hash(setup, element))
}

fn lottery_hash(setup: &Setup, element: Element) -> bool {
    let mut hasher = Blake2s256::new();
    hasher.update(element);
    let digest: Hash = hasher.finalize().into();
    sample::sample_bernoulli(&digest, setup.lottery_probability)
}
