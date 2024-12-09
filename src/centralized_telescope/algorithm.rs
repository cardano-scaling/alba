//! ALBA's bounded DFS scheme prove and verification functions

use super::proof::Proof;
use super::round::Round;
use super::setup::Setup;
use super::types::Hash;
use crate::utils::sample;
use crate::utils::types::Element;
use blake2::{Blake2s256, Digest};

/// Alba's proving algorithm, based on a depth-first search algorithm.
/// Calls up to setup.max_retries times the prove_index function and returns an empty
/// proof if no suitable candidate is found.
pub(super) fn prove(setup: &Setup, prover_set: &[Element]) -> Option<Proof> {
    // Run prove_index up to max_retries times
    (0..setup.max_retries).find_map(|retry_counter| prove_index(setup, prover_set, retry_counter).1)
}

/// Alba's verification algorithm, returns true if the proof is
/// successfully verified, following the DFS verification, false otherwise.
pub(super) fn verify(setup: &Setup, proof: &Proof) -> bool {
    if proof.search_counter >= setup.search_width
        || proof.retry_counter >= setup.max_retries
        || proof.element_sequence.len() as u64 != setup.proof_size
    {
        return false;
    }

    // Initialise a round with given retry and search counters
    let Some(mut round) = Round::new(proof.retry_counter, proof.search_counter, setup.set_size)
    else {
        return false;
    };

    // For each element in the proof's sequence
    for &element in &proof.element_sequence {
        // Retrieve the bin id associated to this new element
        let Some(bin_id) = bin_hash(setup, proof.retry_counter, element) else {
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
    proof_hash(setup, &round)
}

/// Indexed proving algorithm, returns the total number of DFS calls done
/// to find a proof and Some(proof) if found within setup.dfs_bound calls of DFS,
/// otherwise None
fn prove_index(setup: &Setup, prover_set: &[Element], retry_counter: u64) -> (u64, Option<Proof>) {
    // Initialise set_size bins
    let mut bins: Vec<Vec<Element>> = Vec::with_capacity(setup.set_size as usize);
    for _ in 0..setup.set_size {
        bins.push(Vec::new());
    }

    // Take only up to 2*set_size elements for efficiency and fill the bins with them
    for &element in prover_set
        .iter()
        .take(setup.set_size.saturating_mul(2) as usize)
    {
        match bin_hash(setup, retry_counter, element) {
            Some(bin_index) => {
                bins[bin_index as usize].push(element);
            }
            None => return (0, None),
        }
    }

    // Run the DFS algorithm on up to search_width different trees
    let mut step = 0;
    for search_counter in 0..setup.search_width {
        // If DFS was called more than dfs_bound times, abort this retry
        if step >= setup.dfs_bound {
            return (step, None);
        }
        // Initialise new round
        if let Some(r) = Round::new(retry_counter, search_counter, setup.set_size) {
            // Run DFS on such round, incrementing step
            let (dfs_calls, proof_opt) = dfs(setup, &bins, &r, step.saturating_add(1));
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
/// found under setup.dfs_bound calls, returns None otherwise Some(Proof),
/// that is the first round candidate Round{retry_counter, search_counter, x_1, ..., x_u)} such that:
/// - ∀i ∈ [0, u-1], bin_hash(x_i+1) ∈ bins[round_hash(...round_hash(round_hash(v, t), x_1), ..., x_i)]
/// - proof_hash(round_hash(... round_hash((round_hash(v, t), x_1), ..., x_u)) = true
fn dfs(setup: &Setup, bins: &[Vec<Element>], round: &Round, mut step: u64) -> (u64, Option<Proof>) {
    // If current round comprises proof_size elements, returns it as Proof
    if round.element_sequence.len() as u64 == setup.proof_size {
        let proof_opt = if proof_hash(setup, round) {
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
        if step == setup.dfs_bound {
            return (step, None);
        }
        // Update round with such element
        if let Some(r) = Round::update(round, element) {
            // Run DFS on updated round, incrementing step
            let (dfs_calls, proof_opt) = dfs(setup, bins, &r, step.saturating_add(1));
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
fn bin_hash(setup: &Setup, retry_counter: u64, element: Element) -> Option<u64> {
    let retry_bytes: [u8; 8] = retry_counter.to_be_bytes();
    let mut hasher = Blake2s256::new();
    hasher.update(b"Telescope-bin_hash");
    hasher.update(retry_bytes);
    hasher.update(element);
    let digest: Hash = hasher.finalize().into();
    sample::sample_uniform(&digest, setup.set_size)
}

/// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
fn proof_hash(setup: &Setup, r: &Round) -> bool {
    let mut hasher = Blake2s256::new();
    hasher.update(b"Telescope-proof_hash");
    hasher.update(r.hash);
    let digest: Hash = hasher.finalize().into();
    sample::sample_bernoulli(&digest, setup.valid_proof_probability)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::centralized_telescope::{init, params::Params};
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
            let setup = init::make_setup(&params);
            let proof = prove(&setup, &s_p).unwrap();
            assert!(verify(&setup, &proof.clone()));
            // Checking that the proof fails if proof.search_counter is erroneous
            let proof_t = Proof {
                retry_counter: proof.retry_counter,
                search_counter: proof.search_counter.wrapping_add(1),
                element_sequence: proof.element_sequence.clone(),
            };
            assert!(!verify(&setup, &proof_t));
            // Checking that the proof fails if proof.retry_counter is erroneous
            let proof_v = Proof {
                retry_counter: proof.retry_counter.wrapping_add(1),
                search_counter: proof.search_counter,
                element_sequence: proof.element_sequence.clone(),
            };
            assert!(!verify(&setup, &proof_v));
            // Checking that the proof fails when no elements are included
            let proof_item = Proof {
                retry_counter: proof.retry_counter,
                search_counter: proof.search_counter,
                element_sequence: Vec::new(),
            };
            assert!(!verify(&setup, &proof_item));
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
            assert!(!verify(&setup, &proof_itembis));
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
            assert!(!verify(&setup, &proof_itembis));
        }
    }
}
