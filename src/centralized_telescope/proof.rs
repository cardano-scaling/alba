//! Centralized Telescope's `Proof` structure

#![doc = include_str!("../../docs/rustdoc/centralized_telescope/proof.md")]

use super::params::Params;
use super::round::Round;
use crate::utils::{sample, types::truncate};
use digest::{Digest, FixedOutput};
use std::marker::PhantomData;

use rayon::prelude::*;
use std::sync::{Arc, Mutex};

/// Centralized Telescope proof
#[derive(Debug, Clone)]
pub struct Proof<E, H> {
    /// Numbers of retries done to find the proof
    pub retry_counter: u64,
    /// Index of the searched subtree to find the proof
    pub search_counter: u64,
    /// Sequence of elements from prover's set
    pub element_sequence: Vec<E>,
    // Phantom type to link the tree with its hasher
    hasher: PhantomData<fn(H)>,
}

impl<E: AsRef<[u8]> + Clone + Send + Sync, H: Digest + FixedOutput> Proof<E, H> {
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
    pub(super) fn new(set_size: u64, params: &Params, prover_set: &[E]) -> Option<Self> {
        debug_assert!(crate::utils::misc::check_distinct(prover_set));

        Self::prove_routine(set_size, params, prover_set).1
    }

    /// Constructs a `Proof` from a retry counter, search_counter and sequence
    /// of elemenst.
    ///
    /// # Arguments
    ///
    /// * `retry_counter` - the numbers of retries done to find the proof
    /// * `search_counter` - the index of the searched subtree to find the proof
    /// * `element_sequence` - the sequence of elements from prover's set
    ///
    /// # Returns
    ///
    /// A `Proof` structure
    pub fn from(retry_counter: u64, search_counter: u64, element_sequence: Vec<E>) -> Self {
        Self {
            retry_counter,
            search_counter,
            element_sequence,
            hasher: PhantomData::<fn(H)>,
        }
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
    pub fn bench(set_size: u64, params: &Params, prover_set: &[E]) -> (u64, Option<Self>) {
        Self::prove_routine(set_size, params, prover_set)
    }

    /// Alba's prove routine.
    /// Calls up to `params.max_retries` times the prove_index function and
    /// returns the number of steps done when searching a proof as well as a
    /// `Proof` if a suitable candidate tuple is found, otherwise `None`.
    fn prove_routine(set_size: u64, params: &Params, prover_set: &[E]) -> (u64, Option<Self>) {
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
        let Some(mut round) = Round::<E, H>::new(self.retry_counter, self.search_counter, set_size)
        else {
            return false;
        };

        // For each element in the proof's sequence
        for element in &self.element_sequence {
            // Retrieve the bin id associated to this new element
            let Some(bin_id) = Self::bin_hash(set_size, self.retry_counter, element) else {
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
        Self::proof_hash(params.valid_proof_probability, &round)
    }

    /// Indexed proving algorithm, returns the total number of DFS calls done
    /// to find a proof and `Some(proof)` if found within `params.dfs_bound` calls
    /// of DFS, otherwise `None`
    fn prove_index(
        set_size: u64,
        params: &Params,
        prover_set: &[E],
        retry_counter: u64,
    ) -> (u64, Option<Self>) {
        // Initialize set_size bins
        let mut bins = Vec::with_capacity(set_size as usize);
        for _ in 0..set_size {
            bins.push(Vec::new());
        }

        // Take only up to 2*set_size elements for efficiency and fill the bins
        // with them
        for element in prover_set.iter().take(set_size.saturating_mul(2) as usize) {
            if let Some(bin_index) = Self::bin_hash(set_size, retry_counter, element) {
                bins[bin_index as usize].push(element.clone());
            }
        }

        // Run the DFS algorithm on up to params.search_width different trees
        let step = Arc::new(Mutex::new(0u64));
        let proof_found = Arc::new(Mutex::new(false));
        let proof_opt = (0..params.search_width)
            .into_par_iter()
            .find_map_first(|search_counter| {
                // If DFS was called more than params.dfs_bound times,or a proof was
                // already found abort this retru
                if Self::check_locks(params, step.clone(), proof_found.clone()) {
                    return None;
                }
                // Initialize new round
                if let Some(r) = Round::new(retry_counter, search_counter, set_size) {
                    // Run DFS on such round, incrementing step
                    Self::update_step_lock(step.clone());
                    let dfs_result =
                        Self::dfs(params, &bins, &r, step.clone(), proof_found.clone());
                    if dfs_result.is_some() {
                        Self::update_proof_lock(proof_found.clone());
                    }
                    dfs_result
                } else {
                    None
                }
            });

        let total_steps = step.lock().unwrap();
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
        bins: &[Vec<E>],
        round: &Round<E, H>,
        step: Arc<Mutex<u64>>,
        proof_found: Arc<Mutex<bool>>,
    ) -> Option<Self> {
        // If DFS was called more than params.dfs_bound times,or a proof was
        // already found abort this round
        if Self::check_locks(params, step.clone(), proof_found.clone()) {
            return None;
        }

        // If current round comprises params.proof_size elements and satisfies
        // the proof_hash check, return it cast as a Proof
        if round.element_sequence.len() as u64 == params.proof_size {
            return Self::proof_hash(params.valid_proof_probability, round).then_some(Self {
                retry_counter: round.retry_counter,
                search_counter: round.search_counter,
                element_sequence: round.element_sequence.clone(),
                hasher: PhantomData::<fn(H)>,
            });
        }
        // For each element in bin numbered id
        bins[round.id as usize]
            .par_iter()
            .find_map_first(|element| {
                // If DFS was called more than params.dfs_bound times,or a
                // proof was already found abort this round
                if Self::check_locks(params, step.clone(), proof_found.clone()) {
                    return None;
                }
                // Update round with such element
                if let Some(r) = Round::update(round, element) {
                    // Run DFS on updated round, incrementing step
                    Self::update_step_lock(step.clone());
                    Self::dfs(params, bins, &r, step.clone(), proof_found.clone())
                } else {
                    None
                }
            })
    }

    /// Oracle producing a uniformly random value in [0, set_size[ used for
    /// prehashing S_p
    fn bin_hash(set_size: u64, retry_counter: u64, element: &E) -> Option<u64> {
        let retry_bytes: [u8; 8] = retry_counter.to_be_bytes();
        let digest = H::new()
            .chain_update(b"Telescope-bin_hash")
            .chain_update(retry_bytes)
            .chain_update(element.as_ref())
            .finalize();
        let hash = truncate(digest.as_slice());
        sample::sample_uniform(&hash, set_size)
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0
    /// otherwise
    fn proof_hash(valid_proof_probability: f64, r: &Round<E, H>) -> bool {
        let digest = H::new()
            .chain_update(b"Telescope-proof_hash")
            .chain_update(r.hash)
            .finalize();
        let hash = truncate(digest.as_slice());
        sample::sample_bernoulli(&hash, valid_proof_probability)
    }

    fn check_locks(
        params: &Params,
        step_lock: Arc<Mutex<u64>>,
        proof_lock: Arc<Mutex<bool>>,
    ) -> bool {
        *step_lock.lock().unwrap() >= params.dfs_bound || *proof_lock.lock().unwrap() == true
    }

    fn update_step_lock(step_lock: Arc<Mutex<u64>>) -> () {
        let mut step_guard = step_lock.lock().unwrap();
        *step_guard += 1;
        drop(step_guard);
    }

    fn update_proof_lock(proof_found_lock: Arc<Mutex<bool>>) -> () {
        let mut proof_guard = proof_found_lock.lock().unwrap();
        *proof_guard = true;
        drop(proof_guard);
    }
}
