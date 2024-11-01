use super::round::Round;
use super::setup::Setup;
use super::types::Hash;
use super::utils;

use crate::utils::types::Element;
use blake2::{Blake2s256, Digest};

#[derive(Debug, Clone)]
/// Alba proof
pub struct Proof {
    /// Proof counter
    pub(super) v: u64,
    /// Proof 2nd counter
    pub(super) t: u64,
    /// Proof tuple
    pub(super) items: Vec<Element>,
}

impl Proof {
    /// Oracle producing a uniformly random value in [0, n_p[ used for prehashing S_p
    pub(super) fn h0(setup: &Setup, v: u64, s: Element) -> Option<u64> {
        let v_bytes: [u8; 8] = v.to_be_bytes();
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-H0");
        hasher.update(v_bytes);
        hasher.update(s);
        let digest: Hash = hasher.finalize().into();
        utils::sample_uniform(&digest, setup.n_p)
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
    pub(super) fn h2(setup: &Setup, r: &Round) -> bool {
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-H2");
        hasher.update(r.h);
        let digest: Hash = hasher.finalize().into();
        utils::sample_bernoulli(&digest, setup.q)
    }

    /// Depth-First Search which goes through all potential round candidates
    /// and returns the total number of recursive DFS calls done and, if found
    /// under setup.b calls, Some(Proof), that is the first round candidate
    /// Round{v, t, x_1, ..., x_u)} such that:
    /// - ∀i ∈ [0, u-1], H0(x_i+1) ∈ bins[H1(...H1(H1(v, t), x_1), ..., x_i)]
    /// - H2(H1(... H1((H1(v, t), x_1), ..., x_u)) = true
    /// otherwise None
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
}
