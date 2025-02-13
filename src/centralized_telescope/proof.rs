//! Centralized Telescope's `Proof` structure

#![doc = include_str!("../../docs/rustdoc/centralized_telescope/proof.md")]

use super::params::Params;
use super::round::Round;
use crate::utils::{
    sample,
    types::{Element, Hash},
};
use blake2::{Blake2s256, Digest};
use rayon::prelude::*;
use std::sync::{Arc, Mutex};

/// Centralized Telescope proof
#[derive(Debug, Clone)]
pub struct Proof {
    /// Numbers of retries done to find the proof
    pub retry_counter: u64,
    /// Index of the searched subtree to find the proof
    pub search_counter: u64,
    /// Sequence of elements from prover's set
    pub element_sequence: Vec<Element>,
}

impl Proof {
    /// Centralized Telescope's proving algorithm, based on a DFS algorithm.
    /// Calls up to `params.max_retries` times the prove_index function and
    /// returns a `Proof` if a suitable candidate tuple is found.
    ///
    /// # Arguments
    ///
    /// * `set_size` - the size of the prover set to lower bound
    /// * `params` - the internal parameters to generate a proof from
    /// * `prover_set` - the dataset to generate a proof from
    ///
    /// # Returns
    ///
    /// A `Proof` structure
    pub(super) fn new(set_size: u64, params: &Params, prover_set: &[Element]) -> Option<Self> {
        debug_assert!(crate::utils::misc::check_distinct(prover_set));

        Self::prove_routine(set_size, params, prover_set).1
    }

    /// Alba's proving algorithm used for benchmarking, returning a proof if
    /// found as well as the number of steps done when generating it. Only for
    ///  internal usage. Do not use.
    ///
    /// # Arguments
    ///
    /// * `set_size` - the size of the prover set to lower bound
    /// * `params` - the internal parameters to generate a proof from
    /// * `prover_set` - the dataset to generate a proof from
    ///
    /// # Returns
    ///
    /// The number of steps, and `Some(Proof)` structure
    pub fn bench(set_size: u64, params: &Params, prover_set: &[Element]) -> (u64, Option<Proof>) {
        Self::prove_routine(set_size, params, prover_set)
    }

    /// Alba's prove routine.
    /// Calls up to `params.max_retries` times the prove_index function and
    /// returns the number of steps done when searching a proof as well as a
    /// `Proof` if a suitable candidate tuple is found, otherwise `None`.
    fn prove_routine(
        set_size: u64,
        params: &Params,
        prover_set: &[Element],
    ) -> (u64, Option<Proof>) {
        let mut steps: u64 = 0;

        // Run prove_index up to max_retries times
        for retry_counter in 0..params.max_retries {
            let (dfs_calls, proof_opt) = Self::prove_index(
                set_size,
                params,
                prover_set,
                retry_counter.saturating_add(1),
            );
            steps = steps.saturating_add(dfs_calls);
            if proof_opt.is_some() {
                return (steps, proof_opt);
            }
        }
        (steps, None)
    }

    /// Centralized Telescope's verification algorithm, returns true if the
    /// proof is successfully verified, following the DFS verification, false
    /// otherwise.
    ///
    /// # Arguments
    ///
    /// * `self` - the proof to verify
    /// * `set_size` - the size of the prover set to lower bound
    /// * `params` - the internal parameters to generate a proof from
    ///
    /// # Returns
    ///
    /// A boolean, true if the proof verifies successfully otherwise false
    pub(super) fn verify(&self, set_size: u64, params: &Params) -> bool {
        if self.search_counter >= params.search_width
            || self.retry_counter >= params.max_retries
            || self.element_sequence.len() as u64 != params.proof_size
        {
            return false;
        }

        // Initialise a round with given retry and search counters
        let Some(mut round) = Round::new(self.retry_counter, self.search_counter, set_size) else {
            return false;
        };

        // For each element in the proof's sequence
        for &element in &self.element_sequence {
            // Retrieve the bin id associated to this new element
            let Some(bin_id) = Proof::bin_hash(set_size, self.retry_counter, element) else {
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

        Proof::proof_hash(params.valid_proof_probability, &round)
    }

    /// Indexed proving algorithm, returns the total number of DFS calls done
    /// to find a proof and `Some(proof)` if found within `params.dfs_bound` calls
    /// of DFS, otherwise `None`
    fn prove_index(
        set_size: u64,
        params: &Params,
        prover_set: &[Element],
        retry_counter: u64,
    ) -> (u64, Option<Self>) {
        // Initialize set_size bins
        let mut bins: Vec<Vec<Element>> = Vec::with_capacity(set_size as usize);
        for _ in 0..set_size {
            bins.push(Vec::new());
        }

        // Take only up to 2*set_size elements for efficiency and fill the bins
        // with them
        for &element in prover_set.iter().take(set_size.saturating_mul(2) as usize) {
            if let Some(bin_index) = Proof::bin_hash(set_size, retry_counter, element) {
                bins[bin_index as usize].push(element);
            }
        }

        // Initialize shared variables used to check when to stop the threads
        let total_step = Arc::new(Mutex::new(0u64));
        let found = Arc::new(Mutex::new(false));

        // Run the DFS algorithm on up to params.search_width different trees
        let proof_opt = (0..params.search_width)
            .into_par_iter()
            .find_map_any(|search_counter| {
                // If DFS was called more than params.dfs_bound times,or a proof was
                // already found abort this retru
                if check_step(params, &total_step) || check_proof(&found) {
                    return None;
                }

                // Initialize new round
                Round::new(retry_counter, search_counter, set_size).and_then(|r| {
                    // Initializing thread dependent steps
                    let step = Arc::new(Mutex::new(0u64));

                    // Call DFS recursion
                    let dfs_result = Self::dfs(params, &bins, &r, &step, &found);

                    // Updating global number of steps with the ones done in thread
                    update_step_lock(&total_step, *step.lock().unwrap());

                    dfs_result
                })
            });

        let total_steps = total_step.lock().unwrap();
        (*total_steps, proof_opt)
    }

    /// Depth-First Search (DFS) algorithm which goes through all potential
    /// round candidates and returns the total number of recursive DFS calls
    /// done and, if not found under params.dfs_bound calls, None otherwise
    /// `Some(Proof)`, that is the first "round", i.e. the first proof candidate,
    /// Round{retry_counter, search_counter, x_1, ..., x_u)} such that:
    /// - ∀i ∈ [0, u-1], bin_hash(x_i+1) ∈ bins[round_hash(...round_hash(round_hash(v, t), x_1), ..., x_i)]
    /// - proof_hash(round_hash(... round_hash((round_hash(v, t), x_1), ..., x_u)) = true
    fn dfs(
        params: &Params,
        bins: &[Vec<Element>],
        round: &Round,
        step: &Arc<Mutex<u64>>,
        found: &Arc<Mutex<bool>>,
    ) -> Option<Self> {
        // If the current round comprises `params.proof_size elements` and
        // passes the `proof_hash` check, then update the `found` boolean
        // and return the round cast as a `Proof`
        if round.element_sequence.len() as u64 == params.proof_size {
            let proof_found = Proof::proof_hash(params.valid_proof_probability, round);
            if proof_found {
                update_proof_lock(found);
            }
            return proof_found.then(|| Self {
                retry_counter: round.retry_counter,
                search_counter: round.search_counter,
                element_sequence: round.element_sequence.clone(),
            });
        }

        // Otherwise, try to recursively update the current round with each
        // element contained in the bin referenced by the round's id
        bins[round.id as usize].par_iter().find_map_any(|&element| {
            // If a proof has already been found, abort
            if check_proof(found) {
                return None;
            }

            // Otherwise, update the current round with the new element
            Round::update(round, element).and_then(|r| {
                // Run DFS on updated round, incrementing step
                update_step_lock(step, 1u64);
                Self::dfs(params, bins, &r, step, found)
            })
        })
    }

    /// Oracle producing a uniformly random value in [0, set_size[ used for
    /// prehashing S_p
    fn bin_hash(set_size: u64, retry_counter: u64, element: Element) -> Option<u64> {
        let retry_bytes: [u8; 8] = retry_counter.to_be_bytes();
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-bin_hash");
        hasher.update(retry_bytes);
        hasher.update(element);
        let digest: Hash = hasher.finalize().into();
        sample::sample_uniform(&digest, set_size)
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0
    /// otherwise
    fn proof_hash(valid_proof_probability: f64, r: &Round) -> bool {
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-proof_hash");
        hasher.update(r.hash);
        let digest: Hash = hasher.finalize().into();
        sample::sample_bernoulli(&digest, valid_proof_probability)
    }
}

fn check_step(params: &Params, step_lock: &Arc<Mutex<u64>>) -> bool {
    *step_lock.lock().unwrap() >= params.dfs_bound
}

fn check_proof(proof_lock: &Arc<Mutex<bool>>) -> bool {
    *proof_lock.lock().unwrap()
}

fn update_step_lock(step_lock: &Arc<Mutex<u64>>, to_add: u64) {
    let mut step_guard = step_lock.lock().unwrap();
    *step_guard = (*step_guard).wrapping_add(to_add);
    drop(step_guard);
}

fn update_proof_lock(found_lock: &Arc<Mutex<bool>>) {
    let mut proof_guard = found_lock.lock().unwrap();
    *proof_guard = true;
    drop(proof_guard);
}
