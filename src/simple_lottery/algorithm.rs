//! Simple lottery prove and verify functions

use super::proof::Proof;
use super::setup::Setup;
use super::types::Hash;
use crate::utils::sample;
use crate::utils::types::Element;
use blake2::{Blake2s256, Digest};

pub(super) fn prove(setup: &Setup, prover_set: &[Element]) -> Option<Proof> {
    let mut element_sequence = Vec::with_capacity(setup.proof_size as usize);
    for &element in prover_set {
        if lottery_hash(setup, element) {
            element_sequence.push(element);
        }
        if prover_set.len() as u64 >= setup.proof_size {
            return Some(Proof { element_sequence });
        }
    }

    None
}

pub(super) fn verify(setup: &Setup, proof: &Proof) -> bool {
    (proof.element_sequence.len() as u64 == setup.proof_size)
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
