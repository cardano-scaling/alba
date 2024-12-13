//! Simple lottery prove and verify functions

use super::params::Params;
use super::proof::Proof;
use crate::utils::{
    sample,
    types::{Element, Hash},
};
use blake2::{Blake2s256, Digest};

pub(super) fn prove(params: &Params, prover_set: &[Element]) -> Option<Proof> {
    debug_assert!(crate::utils::misc::check_distinct(prover_set));

    let mut element_sequence = Vec::with_capacity(params.proof_size as usize);
    for &element in prover_set {
        if lottery_hash(params, element) {
            element_sequence.push(element);
        }
        if prover_set.len() as u64 >= params.proof_size {
            element_sequence.sort_unstable();
            return Some(Proof { element_sequence });
        }
    }

    None
}

pub(super) fn verify(params: &Params, proof: &Proof) -> bool {
    (proof.element_sequence.len() as u64 == params.proof_size)
        && proof.element_sequence.is_sorted_by(|a, b| a < b)
        && proof
            .element_sequence
            .iter()
            .all(|&element| lottery_hash(params, element))
}

fn lottery_hash(params: &Params, element: Element) -> bool {
    let mut hasher = Blake2s256::new();
    hasher.update(element);
    let digest: Hash = hasher.finalize().into();
    sample::sample_bernoulli(&digest, params.lottery_probability)
}
