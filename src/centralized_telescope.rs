//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

use crate::utils;
use blake2::{Blake2s256, Digest};
use std::f64::consts::LOG2_E;

const DATA_LENGTH: usize = 32;
const DIGEST_SIZE: usize = 32;

type Element = [u8; DATA_LENGTH];
type Hash = [u8; DIGEST_SIZE];

/// Setup input parameters
#[derive(Debug)]
pub struct Params {
    /// Soundness security parameter
    pub lambda_sec: f64,
    /// Completeness security parameter
    pub lambda_rel: f64,
    /// Approximate size of set Sp to lower bound
    pub n_p: u64,
    /// Target lower bound
    pub n_f: u64,
}

/// Setup output parameters
#[derive(Debug, Clone)]
pub struct Setup {
    /// Approximate size of set Sp to lower bound
    pub n_p: u64,
    /// Proof size (in Sp elements)
    pub u: u64,
    /// Proof max counter
    pub r: u64,
    /// Proof max 2nd counter
    pub d: u64,
    /// Probability q
    pub q: f64,
    /// Computation bound
    pub b: u64,
}
impl Setup {
    fn param_small_case(params: &Params, u_f64: f64) -> Self {
        let ln12 = (12f64).ln();
        let d = (32.0 * ln12 * u_f64).ceil();
        Self {
            n_p: params.n_p,
            u: u_f64 as u64,
            r: params.lambda_rel as u64,
            d: d as u64,
            q: 2.0 * ln12 / d,
            b: (8.0 * (u_f64 + 1.0) * d / ln12).floor() as u64,
        }
    }

    fn param_high_case(params: &Params, u_f64: f64, lambda_rel2: f64) -> Self {
        let l2 = lambda_rel2 + 2.0;
        let d = (16.0 * u_f64 * l2 / LOG2_E).ceil();
        debug_assert!(params.n_p as f64 >= d * d * LOG2_E / (9.0 * l2));
        Self {
            n_p: params.n_p,
            u: u_f64 as u64,
            r: (params.lambda_rel / lambda_rel2).ceil() as u64,
            d: d as u64,
            q: 2.0 * l2 / (d * LOG2_E),
            b: (((l2 + u_f64.log2()) / l2) * (3.0 * u_f64 * d / 4.0) + d + u_f64).floor() as u64,
        }
    }

    fn param_mid_case(params: &Params, u_f64: f64, s1: f64) -> Self {
        fn compute_w(u: f64, l: f64) -> f64 {
            fn factorial_check(w: f64, l: f64) -> bool {
                let bound = (-l).exp2();
                let mut factor = (w.ceil() as u64).saturating_add(1);
                let w_2 = w + 2.0;
                let exp_1_over_w = w.recip().exp();
                let mut ratio = (14.0 * w * w * w_2 * exp_1_over_w) / (w_2 - exp_1_over_w);
                while factor != 0 {
                    ratio /= factor as f64;
                    if ratio <= bound {
                        return true;
                    }
                    factor = factor.saturating_sub(1);
                }
                false
            }
            let mut w = u;
            while !factorial_check(w, l) {
                w += 1.0;
            }
            w
        }
        let lambda_rel1 = params.lambda_rel.min(s1);
        let lbar = (lambda_rel1 + 7.0) / LOG2_E;
        let d = (16.0 * u_f64 * lbar).ceil();
        let lbar_over_d = lbar / d;
        debug_assert!(params.n_p as f64 >= d * d / (9.0 * lbar));

        let w = compute_w(u_f64, lambda_rel1);
        let exponential = (2.0 * u_f64 * w * lbar / params.n_p as f64 + 7.0 * u_f64 / w).exp();
        Self {
            n_p: params.n_p,
            u: u_f64 as u64,
            r: (params.lambda_rel / lambda_rel1).ceil() as u64,
            d: d as u64,
            q: 2.0 * lbar_over_d,
            b: ((w * lbar_over_d + 1.0) * exponential * d * u_f64 + d).floor() as u64,
        }
    }

    /// Setup algorithm taking a Params as input and returning setup parameters (u,d,q)
    pub fn new(params: &Params) -> Self {
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;

        let u_f64 = {
            let numerator = params.lambda_sec + params.lambda_rel.log2() + 5.0 - LOG2_E.log2();
            let denominator = (n_p_f64 / n_f_f64).log2();
            (numerator / denominator).ceil()
        };

        let ratio = 9.0 * n_p_f64 * LOG2_E / ((17.0 * u_f64).powi(2));
        let s1 = ratio - 7.0;
        let s2 = ratio - 2.0;

        if s1 < 1.0 || s2 < 1.0 {
            // Small case, i.e. n_p <= λ^2
            Self::param_small_case(params, u_f64)
        } else {
            let lambda_rel2 = params.lambda_rel.min(s2);
            if u_f64 < lambda_rel2 {
                // Case 3, Theorem 14, i.e.  n_p >= λ^3
                Self::param_high_case(params, u_f64, lambda_rel2)
            } else {
                // Case 2, Theorem 13, i.e. λ^3 > n_p > λ^2
                Self::param_mid_case(params, u_f64, s1)
            }
        }
    }
}

/// Round parameters
#[derive(Debug, Clone)]
struct Round {
    /// Proof counter
    v: u64,
    /// Proof 2nd counter
    t: u64,
    // Round candidate tuple
    s_list: Vec<Element>,
    /// Round candidate hash
    h: Hash,
    /// Round candidate hash mapped to [1, n_p]
    h_u64: u64,
    /// Approximate size of set Sp to lower bound
    n_p: u64,
}

impl Round {
    /// Oracle producing a uniformly random value in [0, n_p[ used for round candidates
    /// We also return hash(data) to follow the optimization presented in Section 3.3
    fn h1(first_input: &[u8], second_input: &[u8], n_p: u64) -> (Hash, Option<u64>) {
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-H1");
        hasher.update(first_input);
        hasher.update(second_input);
        let digest: Hash = hasher.finalize().into();
        (digest, utils::sample_uniform(&digest, n_p))
    }

    /// Output a round from a proof counter and n_p
    /// Initilialises the hash with H1(t) and random value as oracle(H1(t), n_p)
    fn new(v: u64, t: u64, n_p: u64) -> Option<Self> {
        let v_bytes: [u8; 8] = v.to_be_bytes();
        let t_bytes: [u8; 8] = t.to_be_bytes();
        let (h, h_u64_opt) = Self::h1(&v_bytes, &t_bytes, n_p);
        h_u64_opt.map(|h_u64| Self {
            v,
            t,
            s_list: Vec::new(),
            h,
            h_u64,
            n_p,
        })
    }

    /// Updates a round with an element of S_p
    /// Replaces the hash $h$ with $h' = H1(h, s)$ and the random value as oracle(h', n_p)
    fn update(r: &Self, s: Element) -> Option<Self> {
        let mut s_list = r.s_list.clone();
        s_list.push(s);
        let (h, h_u64_opt) = Self::h1(&r.h, &s, r.n_p);
        h_u64_opt.map(|h_u64| Self {
            v: r.v,
            t: r.t,
            s_list,
            h,
            h_u64,
            n_p: r.n_p,
        })
    }
}

#[derive(Debug, Clone)]
/// Alba proof
pub struct Proof {
    /// Proof counter
    v: u64,
    /// Proof 2nd counter
    t: u64,
    /// Proof tuple
    items: Vec<Element>,
}

impl Proof {
    /// Oracle producing a uniformly random value in [0, n_p[ used for prehashing S_p
    fn h0(setup: &Setup, v: u64, s: Element) -> Option<u64> {
        let v_bytes: [u8; 8] = v.to_be_bytes();
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-H0");
        hasher.update(v_bytes);
        hasher.update(s);
        let digest: Hash = hasher.finalize().into();
        utils::sample_uniform(&digest, setup.n_p)
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
    fn h2(setup: &Setup, r: &Round) -> bool {
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
    fn prove_index(setup: &Setup, set: &[Element], v: u64) -> (u64, Option<Self>) {
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

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Calls up to setup.r times the prove_index function and returns an empty
    /// proof if no suitable candidate is found.
    pub fn prove(setup: &Setup, set: &[Element]) -> Option<Self> {
        (0..setup.r).find_map(|v| Self::prove_index(setup, set, v).1)
    }

    /// Alba's verification algorithm, returns true if the proof is
    /// successfully verified, following the DFS verification, false otherwise.
    pub fn verify(setup: &Setup, proof: &Self) -> bool {
        if proof.t >= setup.d || proof.v >= setup.r || proof.items.len() as u64 != setup.u {
            return false;
        }
        let Some(mut round) = Round::new(proof.v, proof.t, setup.n_p) else {
            return false;
        };
        for &element in &proof.items {
            let Some(h) = Self::h0(setup, proof.v, element) else {
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
        Self::h2(setup, &round)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::gen_items;
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
            let proof = Proof::prove(&setup, &s_p).unwrap();
            assert!(Proof::verify(&setup, &proof.clone()));
            // Checking that the proof fails if proof.t is erroneous
            let proof_t = Proof {
                v: proof.v,
                t: proof.t.wrapping_add(1),
                items: proof.items.clone(),
            };
            assert!(!Proof::verify(&setup, &proof_t));
            // Checking that the proof fails if proof.v is erroneous
            let proof_v = Proof {
                v: proof.v.wrapping_add(1),
                t: proof.t,
                items: proof.items.clone(),
            };
            assert!(!Proof::verify(&setup, &proof_v));
            // Checking that the proof fails when no elements are included
            let proof_item = Proof {
                v: proof.v,
                t: proof.t,
                items: Vec::new(),
            };
            assert!(!Proof::verify(&setup, &proof_item));
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
            assert!(!Proof::verify(&setup, &proof_itembis));
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
            assert!(!Proof::verify(&setup, &proof_itembis));
        }
    }
}