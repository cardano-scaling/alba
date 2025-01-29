//! Centralized Telescope's `Proof` structure

#![doc = include_str!("../../docs/rustdoc/centralized_telescope/proof.md")]

use super::params::Params;
use super::round::Round;
use crate::utils::{
    sample,
    types::{Hash, ToBytes},
};
use blake2::{Blake2s256, Digest};

/// Centralized Telescope proof
#[derive(Debug, Clone)]
pub struct Proof<E: ToBytes + Clone + Sized + Ord> {
    /// Numbers of retries done to find the proof
    pub retry_counter: u64,
    /// Index of the searched subtree to find the proof
    pub search_counter: u64,
    /// Sequence of elements from prover's set
    pub element_sequence: Vec<E>,
}

impl<E: ToBytes + Clone + Sized + Ord> Proof<E> {
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
    ///
    /// # Example
    ///
    /// ```
    /// use alba::centralized_telescope::params::Params;
    /// use alba::centralized_telescope::proof::Proof;
    /// let set_size = 200;
    /// let params = Params::new(128.0, 128.0, 1_000, 750);
    /// let mut prover_set = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push([(i % 256) as u8 ; 48]);
    /// }
    /// let (setps, proof_opt) = Proof::bench(set_size, &params, &prover_set);
    /// ```
    pub fn bench(set_size: u64, params: &Params, prover_set: &[E]) -> (u64, Option<Self>) {
        Self::prove_routine(set_size, params, prover_set)
    }

    /// Alba's prove routine.
    /// Calls up to `params.max_retries` times the prove_index function and
    /// returns the number of steps done when searching a proof as well as a
    /// `Proof` if a suitable candidate tuple is found, otherwise `None`.
    fn prove_routine(set_size: u64, params: &Params, prover_set: &[E]) -> (u64, Option<Proof<E>>) {
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
        let Some(mut round) = Round::<E>::new(self.retry_counter, self.search_counter, set_size)
        else {
            return false;
        };

        // For each element in the proof's sequence
        for element in &self.element_sequence {
            // Retrieve the bin id associated to this new element
            let Some(bin_id) = Self::bin_hash(set_size, self.retry_counter, element.clone()) else {
                return false;
            };
            // Check that the new element was chosen correctly
            // i.e. that we chose the new element such that its bin id equals the round id
            if round.id == bin_id {
                match Round::update(&round, element.clone()) {
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
            match Self::bin_hash(set_size, retry_counter, element.clone()) {
                Some(bin_index) => {
                    bins[bin_index as usize].push(element.clone());
                }
                None => return (0, None),
            }
        }

        // Run the DFS algorithm on up to params.search_width different trees
        let mut step = 0;
        for search_counter in 0..params.search_width {
            // If DFS was called more than dfs_bound times, abort this retry
            if step >= params.dfs_bound {
                return (step, None);
            }
            // Initialize new round
            if let Some(r) = Round::new(retry_counter, search_counter, set_size) {
                // Run DFS on such round, incrementing step
                let (dfs_calls, proof_opt) = Self::dfs(params, &bins, &r, step.saturating_add(1));
                // Return proof if found
                if proof_opt.is_some() {
                    return (dfs_calls, proof_opt);
                }
                // Update step, that is the number of DFS calls
                step = dfs_calls;
            }
        }
        (step, None)
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
        round: &Round<E>,
        mut step: u64,
    ) -> (u64, Option<Self>) {
        // If current round comprises params.proof_size elements and satisfies
        // the proof_hash check, return it cast as a Proof
        if round.element_sequence.len() as u64 == params.proof_size {
            let proof_opt = if Proof::proof_hash(params.valid_proof_probability, round) {
                Some(Self {
                    retry_counter: round.retry_counter,
                    search_counter: round.search_counter,
                    element_sequence: round.element_sequence.clone(),
                })
            } else {
                None
            };
            return (step, proof_opt);
        }

        // For each element in bin numbered id
        for element in &bins[round.id as usize] {
            // If DFS was called more than params.dfs_bound times, abort this
            // round
            if step == params.dfs_bound {
                return (step, None);
            }
            // Update round with such element
            if let Some(r) = Round::update(round, element.clone()) {
                // Run DFS on updated round, incrementing step
                let (dfs_calls, proof_opt) = Self::dfs(params, bins, &r, step.saturating_add(1));
                // Return proof if found
                if proof_opt.is_some() {
                    return (dfs_calls, proof_opt);
                }
                // Update step, i.e. is the number of DFS calls
                step = dfs_calls;
            }
        }
        // If no proof was found, return number of steps and None
        (step, None)
    }

    /// Oracle producing a uniformly random value in [0, set_size[ used for
    /// prehashing S_p
    fn bin_hash(set_size: u64, retry_counter: u64, element: E) -> Option<u64> {
        let retry_bytes: [u8; 8] = retry_counter.to_be_bytes();
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-bin_hash");
        hasher.update(retry_bytes);
        hasher.update(element.to_be_bytes());
        let digest: Hash = hasher.finalize().into();
        sample::sample_uniform(&digest, set_size)
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0
    /// otherwise
    fn proof_hash(valid_proof_probability: f64, r: &Round<E>) -> bool {
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-proof_hash");
        hasher.update(r.hash);
        let digest: Hash = hasher.finalize().into();
        sample::sample_bernoulli(&digest, valid_proof_probability)
    }
}
