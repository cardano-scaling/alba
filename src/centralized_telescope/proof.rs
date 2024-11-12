//! ALBA's Proof structure

use super::params::Params;
use super::round::Round;
use super::types::Hash;
use crate::utils::sample;
use crate::utils::types::Element;
use blake2::{Blake2s256, Digest};

/// Alba proof
#[derive(Debug, Clone)]
pub struct Proof {
    /// Numbers of retries done to find the proof
    pub retry_counter: u64,
    /// Index of the searched subtree to find the proof
    pub search_counter: u64,
    /// Sequence of elements from prover set
    pub element_sequence: Vec<Element>,
}

impl Proof {
    /// Indexed proving algorithm, returns the total number of DFS calls done
    /// to find a proof and Some(proof) if found within params.dfs_bound calls of DFS,
    /// otherwise None
    pub(super) fn prove_index(
        set_size: u64,
        params: &Params,
        prover_set: &[Element],
        retry_counter: u64,
    ) -> (u64, Option<Proof>) {
        // Initialise set_size bins
        let mut bins: Vec<Vec<Element>> = Vec::with_capacity(set_size as usize);
        for _ in 0..set_size {
            bins.push(Vec::new());
        }

        // Take only up to 2*set_size elements for efficiency and fill the bins with them
        for &element in prover_set.iter().take(set_size.saturating_mul(2) as usize) {
            match Proof::bin_hash(set_size, retry_counter, element) {
                Some(bin_index) => {
                    bins[bin_index as usize].push(element);
                }
                None => return (0, None),
            }
        }

        // Run the DFS algorithm on up to search_width different trees
        let mut step = 0;
        for search_counter in 0..params.search_width {
            // If DFS was called more than dfs_bound times, abort this retry
            if step >= params.dfs_bound {
                return (step, None);
            }
            // Initialise new round
            if let Some(r) = Round::new(retry_counter, search_counter, set_size) {
                // Run DFS on such round, incrementing step
                let (dfs_calls, proof_opt) = Proof::dfs(params, &bins, &r, step.saturating_add(1));
                // Returns proof if found
                if proof_opt.is_some() {
                    return (dfs_calls, proof_opt);
                }
                // Update step, that is the number of DFS calls
                step = dfs_calls;
            }
        }
        (step, None)
    }

    /// Depth-First Search which goes through all potential round candidates
    /// and returns the total number of recursive DFS calls done and, if not
    /// found under params.dfs_bound calls, returns None otherwise Some(Proof),
    /// that is the first round candidate Round{retry_counter, search_counter, x_1, ..., x_u)} such that:
    /// - ∀i ∈ [0, u-1], bin_hash(x_i+1) ∈ bins[round_hash(...round_hash(round_hash(v, t), x_1), ..., x_i)]
    /// - proof_hash(round_hash(... round_hash((round_hash(v, t), x_1), ..., x_u)) = true
    fn dfs(
        params: &Params,
        bins: &[Vec<Element>],
        round: &Round,
        mut step: u64,
    ) -> (u64, Option<Proof>) {
        // If current round comprises proof_size elements, returns it as Proof
        if round.element_sequence.len() as u64 == params.proof_size {
            let proof_opt = if Proof::proof_hash(params.valid_proof_probability, round) {
                Some(Proof {
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
        for &element in &bins[round.id as usize] {
            // If DFS was called more than dfs_bound times, abort this round
            if step == params.dfs_bound {
                return (step, None);
            }
            // Update round with such element
            if let Some(r) = Round::update(round, element) {
                // Run DFS on updated round, incrementing step
                let (dfs_calls, proof_opt) = Proof::dfs(params, bins, &r, step.saturating_add(1));
                // Returns proof if found
                if proof_opt.is_some() {
                    return (dfs_calls, proof_opt);
                }
                // Update step, that is the number of DFS calls
                step = dfs_calls;
            }
        }
        // If no proof was found, return number of steps and None
        (step, None)
    }

    /// Oracle producing a uniformly random value in [0, set_size[ used for prehashing S_p
    pub(super) fn bin_hash(set_size: u64, retry_counter: u64, element: Element) -> Option<u64> {
        let retry_bytes: [u8; 8] = retry_counter.to_be_bytes();
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-bin_hash");
        hasher.update(retry_bytes);
        hasher.update(element);
        let digest: Hash = hasher.finalize().into();
        sample::sample_uniform(&digest, set_size)
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
    pub(super) fn proof_hash(valid_proof_probability: f64, r: &Round) -> bool {
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-proof_hash");
        hasher.update(r.hash);
        let digest: Hash = hasher.finalize().into();
        sample::sample_bernoulli(&digest, valid_proof_probability)
    }
}
