//! Customer facing Telescope structure

use super::params::Params;
use super::proof::Proof;
use super::round::Round;

/// Structure wrapping input and internal parameters to generate a proof from
/// and verify it
#[derive(Debug, Clone, Copy)]
pub struct Telescope {
    /// Soundness security parameter
    pub soundness_param: f64,
    /// Completeness security parameter
    pub completeness_param: f64,
    /// Approximate size of the prover set to lower bound
    pub set_size: u64,
    /// Lower bound to prove on prover set
    pub lower_bound: u64,
    /// Internal parameters
    pub params: Params,
}

impl Telescope {
    /// Returns a Telescope structure from input parameters
    pub fn new(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
    ) -> Self {
        let params = Params::new(soundness_param, completeness_param, set_size, lower_bound);
        Telescope {
            soundness_param,
            completeness_param,
            set_size,
            lower_bound,
            params,
        }
    }

    /// Returns a Telescope structure from a set_size and Params
    pub fn from(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
        params: Params,
    ) -> Self {
        Telescope {
            soundness_param,
            completeness_param,
            set_size,
            lower_bound,
            params,
        }
    }

    /// Generates a Centralized Telescope proof.
    ///
    /// # Arguments
    ///
    /// * `self` - the current Telescope structure
    /// * `prover_set` - an array of elements to generate an Alba proof on
    ///
    /// # Returns
    ///
    /// A proof if found, None otherwise
    ///
    /// # Example
    ///
    /// ```
    /// let telescope = Telescope::new(128, 128, 1_000, 750);
    /// let proof = telescope.prove(prover_set)?;
    /// ```

    pub fn prove(&self, prover_set: &[crate::utils::types::Element]) -> Option<Proof> {
        // Run prove_index up to max_retries times
        (0..self.params.max_retries).find_map(|retry_counter| {
            Proof::prove_index(self.set_size, &self.params, prover_set, retry_counter).1
        })
    }

    /// Verifies a Centralized Telescope proof.
    ///
    /// # Arguments
    ///
    /// * `self` - the current Telescope structure
    /// * `proof` - a centralized Telescope proof
    ///
    /// # Returns
    ///
    /// True if the verification is successful, false otherwise
    ///
    /// # Example
    ///
    /// ```
    /// let telescope = Telescope::new(128, 128, 1_000, 750);
    /// let proof = telescope.prove(prover_set)?;
    /// assert!(telescope.verify(proof));
    /// ```

    pub fn verify(&self, proof: &Proof) -> bool {
        if proof.search_counter >= self.params.search_width
            || proof.retry_counter >= self.params.max_retries
            || proof.element_sequence.len() as u64 != self.params.proof_size
        {
            return false;
        }

        // Initialize a round with given retry and search counters
        let Some(mut round) = Round::new(proof.retry_counter, proof.search_counter, self.set_size)
        else {
            return false;
        };

        // For each element in the proof's sequence
        for &element in &proof.element_sequence {
            // Retrieve the bin id associated to this new element
            let Some(bin_id) = Proof::bin_hash(self.set_size, proof.retry_counter, element) else {
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
        Proof::proof_hash(self.params.valid_proof_probability, &round)
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
        let soundness_param = 10.0;
        let completeness_param = 10.0;
        let set_size = 80 * set_size / 100;
        let lower_bound = 20 * set_size / 100;
        for _t in 0..nb_tests {
            let seed = rng.next_u32().to_be_bytes().to_vec();
            let s_p = gen_items::<DATA_LENGTH>(&seed, set_size);
            let telescope =
                Telescope::new(soundness_param, completeness_param, set_size, lower_bound);
            let proof = telescope.prove(&s_p).unwrap();
            assert!(telescope.verify(&proof.clone()));
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
