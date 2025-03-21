//! Centralized Telescope integration tests

use alba::centralized_telescope::Telescope;
use alba::centralized_telescope::{params::Params, proof::Proof};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

mod common;
use common::gen_items;

use alba::utils::types::Element;
use sha2::Sha256;

const DATA_LENGTH: usize = 48;
type P = Proof<[u8; DATA_LENGTH], Sha256>;

fn test(created_with_params: bool) {
    let mut rng = ChaCha20Rng::from_seed(Default::default());
    let nb_tests = 1_000;
    let nb_elements: u64 = 1_000;
    let soundness_param = 10.0;
    let completeness_param = 10.0;
    let set_size = nb_elements.saturating_mul(80).div_ceil(100);
    let lower_bound = nb_elements.saturating_mul(20).div_ceil(100);
    for _t in 0..nb_tests {
        let seed = rng.next_u32().to_be_bytes().to_vec();
        let s_p: Vec<Element<[u8; DATA_LENGTH]>> = gen_items::<DATA_LENGTH>(&seed, nb_elements);
        let alba = if created_with_params {
            Telescope::create(soundness_param, completeness_param, set_size, lower_bound)
        } else {
            let setup = Params::new(soundness_param, completeness_param, set_size, lower_bound);
            Telescope::setup_unsafe(set_size, &setup)
        };
        let proof = alba.prove::<[u8; DATA_LENGTH], Sha256>(&s_p).unwrap();
        assert!(alba.verify(&proof));
        // Checking that the proof fails if proof.search_counter is erroneous
        let proof_t = P::from(
            proof.retry_counter,
            proof.search_counter.wrapping_add(1),
            proof.element_sequence.clone(),
        );
        assert!(!alba.verify(&proof_t));
        // Checking that the proof fails if proof.retry_counter is erroneous
        let proof_v = P::from(
            proof.retry_counter.wrapping_add(1),
            proof.search_counter,
            proof.element_sequence.clone(),
        );
        assert!(!alba.verify(&proof_v));
        // Checking that the proof fails when no elements are included
        let proof_item = P::from(proof.retry_counter, proof.search_counter, Vec::new());
        assert!(!alba.verify(&proof_item));
        // Checking that the proof fails when wrong elements are included
        // We are trying to trigger proof_hash
        let mut wrong_items = proof.element_sequence.clone();
        let last_item = wrong_items.pop().unwrap();
        let mut penultimate_item = wrong_items.pop().unwrap();
        let proof_itembis = P::from(
            proof.retry_counter,
            proof.search_counter,
            wrong_items.clone(),
        );
        assert!(!alba.verify(&proof_itembis));
        // Checking that the proof fails when wrong elements are included
        // We are trying to trigger round_hash
        penultimate_item.data[0] = penultimate_item.data[0].wrapping_add(42u8);
        wrong_items.push(penultimate_item);
        wrong_items.push(last_item);
        let proof_itembis = P::from(
            proof.retry_counter,
            proof.search_counter,
            wrong_items.clone(),
        );
        assert!(!alba.verify(&proof_itembis));
    }
}

#[test]
fn created_with_params() {
    test(true);
}

#[test]
fn created_with_setup() {
    test(false);
}
