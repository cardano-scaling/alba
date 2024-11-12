//! Customer facing Telescope structure

use super::algorithm::{bin_hash, proof_hash, prove_index};
use super::init::make_setup;
use super::params::Params;
use super::proof::Proof;
use super::round::Round;
use super::setup::Setup;

/// Structure wrapping input and internal parameters to generate a proof from
/// and verify it
#[derive(Debug, Clone, Copy)]
pub struct Telescope {
    /// Input parameters
    pub params: Params,
    /// Internal parameters
    pub setup: Setup,
}

impl Telescope {
    /// Returns a Telescope structure from input parameters
    pub fn new(params: Params) -> Self {
        let setup = make_setup(&params);
        Telescope { params, setup }
    }

    /// Returns a Telescope structure from a set_size and Setup
    pub fn from(set_size: u64, setup: Setup) -> Self {
        let params = Params {
            soundness_param: 0.0,
            completeness_param: 0.0,
            set_size,
            lower_bound: 0,
        };
        Telescope { params, setup }
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Calls up to setup.max_retries times the prove_index function and returns an empty
    /// proof if no suitable candidate is found.
    pub fn prove(self, prover_set: &[crate::utils::types::Element]) -> Option<Proof> {
        // Run prove_index up to max_retries times
        (0..self.setup.max_retries)
            .find_map(|retry_counter| prove_index(&self.setup, prover_set, retry_counter).1)
    }

    /// Alba's verification algorithm, returns true if the proof is
    /// successfully verified, following the DFS verification, false otherwise.
    pub fn verify(self, proof: &Proof) -> bool {
        if proof.search_counter >= self.setup.search_width
            || proof.retry_counter >= self.setup.max_retries
            || proof.element_sequence.len() as u64 != self.setup.proof_size
        {
            return false;
        }

        // Initialise a round with given retry and search counters
        let Some(mut round) = Round::new(
            proof.retry_counter,
            proof.search_counter,
            self.setup.set_size,
        ) else {
            return false;
        };

        // For each element in the proof's sequence
        for &element in &proof.element_sequence {
            // Retrieve the bin id associated to this new element
            let Some(bin_id) = bin_hash(&self.setup, proof.retry_counter, element) else {
                return false;
            };
            // Check that the new element was chosen correctly
            // i.e. that we chose the new element such that its bin id equals the round id
            if round.id == bin_id {
                match Round::update(&round, element) {
                    Some(r) => round = r,
                    None => return false,
                }
            } else {
                return false;
            }
        }
        proof_hash(&self.setup, &round)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::test_utils::gen_items;
    use crate::utils::types::DATA_LENGTH;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    #[test]
    fn test_verify() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let nb_tests = 1_000;
        let set_size = 1_000;
        let params = Params {
            soundness_param: 10.0,
            completeness_param: 10.0,
            set_size: 80 * set_size / 100,
            lower_bound: 20 * set_size / 100,
        };
        for _t in 0..nb_tests {
            let seed = rng.next_u32().to_be_bytes().to_vec();
            let s_p = gen_items::<DATA_LENGTH>(&seed, set_size);
            let telescope = Telescope::new(params);
            let proof = telescope.prove(&s_p).unwrap();
            assert!(telescope.clone().verify(&proof.clone()));
            // Checking that the proof fails if proof.search_counter is erroneous
            let proof_t = Proof {
                retry_counter: proof.retry_counter,
                search_counter: proof.search_counter.wrapping_add(1),
                element_sequence: proof.element_sequence.clone(),
            };
            assert!(!telescope.verify(&proof_t));
            // Checking that the proof fails if proof.retry_counter is erroneous
            let proof_v = Proof {
                retry_counter: proof.retry_counter.wrapping_add(1),
                search_counter: proof.search_counter,
                element_sequence: proof.element_sequence.clone(),
            };
            assert!(!telescope.verify(&proof_v));
            // Checking that the proof fails when no elements are included
            let proof_item = Proof {
                retry_counter: proof.retry_counter,
                search_counter: proof.search_counter,
                element_sequence: Vec::new(),
            };
            assert!(!telescope.verify(&proof_item));
            // Checking that the proof fails when wrong elements are included
            // We are trying to trigger proof_hash
            let mut wrong_items = proof.element_sequence.clone();
            let last_item = wrong_items.pop().unwrap();
            let mut penultimate_item = wrong_items.pop().unwrap();
            let proof_itembis = Proof {
                retry_counter: proof.retry_counter,
                search_counter: proof.search_counter,
                element_sequence: wrong_items.clone(),
            };
            assert!(!telescope.verify(&proof_itembis));
            // Checking that the proof fails when wrong elements are included
            // We are trying to trigger round_hash
            penultimate_item[0] = penultimate_item[0].wrapping_add(42u8);
            wrong_items.push(penultimate_item);
            wrong_items.push(last_item);
            let proof_itembis = Proof {
                retry_counter: proof.retry_counter,
                search_counter: proof.search_counter,
                element_sequence: wrong_items.clone(),
            };
            assert!(!telescope.verify(&proof_itembis));
        }
    }
}
