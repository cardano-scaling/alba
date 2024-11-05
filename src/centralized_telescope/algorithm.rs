//! ALBA's bounded DFS scheme prove and verification functions

use super::proof::Proof;
use super::round::Round;
use super::setup::Setup;
use super::types::Hash;
use crate::utils::sample;
use crate::utils::types::Element;
use blake2::{Blake2s256, Digest};

/// Alba's proving algorithm, based on a depth-first search algorithm.
/// Calls up to setup.r times the prove_index function and returns an empty
/// proof if no suitable candidate is found.
pub fn prove(setup: &Setup, set: &[Element]) -> Option<Proof> {
    (0..setup.r).find_map(|v| Proof::prove_index(setup, set, v).1)
}

/// Alba's verification algorithm, returns true if the proof is
/// successfully verified, following the DFS verification, false otherwise.
pub fn verify(setup: &Setup, proof: &Proof) -> bool {
    if proof.t >= setup.d || proof.v >= setup.r || proof.items.len() as u64 != setup.u {
        return false;
    }
    let Some(mut round) = Round::new(proof.v, proof.t, setup.n_p) else {
        return false;
    };
    for &element in &proof.items {
        let Some(h) = Proof::h0(setup, proof.v, element) else {
            return false;
        };
        if round.h_u64 == h {
            match Round::update(&round, element) {
                Some(r) => round = r,
                None => return false,
            }
        } else {
            return false;
        }
    }
    Proof::h2(setup, &round)
}

impl Proof {
    /// Indexed proving algorithm, returns the total number of DFS calls done
    /// to find a proof and Some(proof) if found within setup.b calls of DFS,
    /// otherwise None
    pub(super) fn prove_index(setup: &Setup, set: &[Element], v: u64) -> (u64, Option<Self>) {
        let mut bins: Vec<Vec<Element>> = Vec::with_capacity(setup.n_p as usize);
        for _ in 0..setup.n_p {
            bins.push(Vec::new());
        }
        // Take only up to 2*np elements for efficiency
        for &s in set.iter().take(setup.n_p.saturating_mul(2) as usize) {
            match Self::h0(setup, v, s) {
                Some(h) => {
                    bins[h as usize].push(s);
                }
                None => return (0, None),
            }
        }

        let mut limit = 0;
        for t in 0..setup.d {
            if limit == setup.b {
                return (limit, None);
            }
            if let Some(r) = Round::new(v, t, setup.n_p) {
                let (l, proof_opt) = Self::dfs(setup, &bins, &r, limit.saturating_add(1));
                if proof_opt.is_some() {
                    return (l, proof_opt);
                }
                limit = l;
            }
        }
        (limit, None)
    }

    /// Depth-First Search which goes through all potential round candidates
    /// and returns the total number of recursive DFS calls done and, if not
    /// found under setup.b calls, returns None otherwise Some(Proof), that is
    /// the first round candidate Round{v, t, x_1, ..., x_u)} such that:
    /// - ∀i ∈ [0, u-1], H0(x_i+1) ∈ bins[H1(...H1(H1(v, t), x_1), ..., x_i)]
    /// - H2(H1(... H1((H1(v, t), x_1), ..., x_u)) = true
    fn dfs(
        setup: &Setup,
        bins: &[Vec<Element>],
        round: &Round,
        mut limit: u64,
    ) -> (u64, Option<Self>) {
        if round.s_list.len() as u64 == setup.u {
            let proof_opt = if Self::h2(setup, round) {
                Some(Self {
                    v: round.v,
                    t: round.t,
                    items: round.s_list.clone(),
                })
            } else {
                None
            };
            return (limit, proof_opt);
        }

        for &s in &bins[round.h_u64 as usize] {
            if limit == setup.b {
                return (limit, None);
            }
            if let Some(r) = Round::update(round, s) {
                let (l, proof_opt) = Self::dfs(setup, bins, &r, limit.saturating_add(1));
                if proof_opt.is_some() {
                    return (l, proof_opt);
                }
                limit = l;
            }
        }
        (limit, None)
    }

    /// Oracle producing a uniformly random value in [0, n_p[ used for prehashing S_p
    pub(super) fn h0(setup: &Setup, v: u64, s: Element) -> Option<u64> {
        let v_bytes: [u8; 8] = v.to_be_bytes();
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-H0");
        hasher.update(v_bytes);
        hasher.update(s);
        let digest: Hash = hasher.finalize().into();
        sample::sample_uniform(&digest, setup.n_p)
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
    pub(super) fn h2(setup: &Setup, r: &Round) -> bool {
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-H2");
        hasher.update(r.h);
        let digest: Hash = hasher.finalize().into();
        sample::sample_bernoulli(&digest, setup.q)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::centralized_telescope::params::Params;
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
            lambda_sec: 10.0,
            lambda_rel: 10.0,
            n_p: 80 * set_size / 100,
            n_f: 20 * set_size / 100,
        };
        for _t in 0..nb_tests {
            let seed = rng.next_u32().to_be_bytes().to_vec();
            let s_p = gen_items::<DATA_LENGTH>(&seed, set_size);
            let setup = Setup::new(&params);
            let proof = prove(&setup, &s_p).unwrap();
            assert!(verify(&setup, &proof.clone()));
            // Checking that the proof fails if proof.t is erroneous
            let proof_t = Proof {
                v: proof.v,
                t: proof.t.wrapping_add(1),
                items: proof.items.clone(),
            };
            assert!(!verify(&setup, &proof_t));
            // Checking that the proof fails if proof.v is erroneous
            let proof_v = Proof {
                v: proof.v.wrapping_add(1),
                t: proof.t,
                items: proof.items.clone(),
            };
            assert!(!verify(&setup, &proof_v));
            // Checking that the proof fails when no elements are included
            let proof_item = Proof {
                v: proof.v,
                t: proof.t,
                items: Vec::new(),
            };
            assert!(!verify(&setup, &proof_item));
            // Checking that the proof fails when wrong elements are included
            // We are trying to trigger H2
            let mut wrong_items = proof.items.clone();
            let last_item = wrong_items.pop().unwrap();
            let mut penultimate_item = wrong_items.pop().unwrap();
            let proof_itembis = Proof {
                v: proof.v,
                t: proof.t,
                items: wrong_items.clone(),
            };
            assert!(!verify(&setup, &proof_itembis));
            // Checking that the proof fails when wrong elements are included
            // We are trying to trigger H1
            penultimate_item[0] = penultimate_item[0].wrapping_add(42u8);
            wrong_items.push(penultimate_item);
            wrong_items.push(last_item);
            let proof_itembis = Proof {
                v: proof.v,
                t: proof.t,
                items: wrong_items.clone(),
            };
            assert!(!verify(&setup, &proof_itembis));
        }
    }
}
