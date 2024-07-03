//! Rust implementation of ALBA's bounded DFS scheme using Blake2b as hash
//! function.

extern crate core;
extern crate utils;

use std::f64::consts::E;

/// Setup input parameters
#[derive(Debug, Clone)]
pub struct Params {
    /// Soundness security parameter
    pub lambda_sec: usize,
    /// Completeness security parameter
    pub lambda_rel: usize,
    /// Approximate size of set Sp to lower bound
    pub n_p: usize,
    /// Target lower bound
    pub n_f: usize,
}
/// Setup output parameters
#[derive(Debug, Clone)]
pub struct Setup {
    /// Approximate size of set Sp to lower bound
    pub n_p: usize,
    /// Proof size (in Sp elements)
    pub u: usize,
    /// Proof max counter
    pub r: usize,
    /// Proof max 2nd counter
    pub d: usize,
    /// Inverse of probability p_q
    pub q: usize,
    /// Computation bound
    pub b: usize,
}
impl Setup {
    /// Setup algorithm taking a Params as input and returning setup parameters (u,d,q)
    pub fn new(params: &Params) -> Self {
        // Misc values
        let e = E;
        let log_2 = |x: f64| x.log2();
        let loge = log_2(e);
        let logloge = log_2(loge);
        let log3 = log_2(3.0);
        let log12 = log_2(12.0);

        // Converting params to f64
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;
        let lognpnf = log_2(n_p_f64 / n_f_f64);
        let lambda_rel = params.lambda_rel as f64;
        let lambda_sec = (params.lambda_sec as f64) + log_2(lambda_rel) as f64;

        let l2 = (lambda_rel * lambda_rel).ceil() as usize; // Is it the right lambda?
        let l3 = l2 * params.lambda_rel;

        // Initialising output params
        let u;
        let r;
        let d;
        let q;
        let b;

        // If $n_p$ < λ^3, we define the parameters accoding to Section 3.2.2, Theorem 12 and Corollary 3
        if n_p_f64 < (l3 as f64) {
            let u_f64 = (lambda_sec + log_2(lambda_rel) + 5.0 - logloge) / lognpnf;
            u = u_f64.ceil() as usize;
            r = params.lambda_rel;
            b = 1000 * l2; // What about the epsilon
            let d_f64 = 32.0 * log12 * u_f64;
            d = d_f64.ceil() as usize;
            q = ((2.0 * log12) / d_f64).recip().ceil() as usize;
        } else {
            // Otherwise, according to Section 3.2, Corrollary 2
            let u_f64 = (lambda_sec + log_2(lambda_rel + log3) + 1.0 - logloge) / lognpnf;
            u = u_f64.ceil() as usize;
            r = 1;
            b = 1000 * l3;
            let d_f64 = 16.0 * u_f64 * (lambda_rel + log3) / loge;
            d = d_f64.ceil() as usize;
            q = (2.0 * (lambda_rel + log3) / (d_f64 * loge)).recip().ceil() as usize;

            let check = ((d_f64 * d_f64 * loge) / (9.0 * (lambda_rel + log3))).ceil() as usize;
            assert!(params.n_p >= check);
        }

        Setup {
            n_p: params.n_p,
            u,
            r,
            d,
            q,
            b,
        }
    }
}

/// Round parameters
#[derive(Debug, Clone)]
pub struct Round {
    /// Proof counter
    v: usize,
    /// Proof 2nd counter
    t: usize,
    // Round candidate tuple
    s_list: Vec<[u8; 32]>,
    /// Round candidate hash
    h: Vec<u8>,
    /// Round candidate hash mapped to [1, n_p]
    h_usize: usize,
    /// Approximate size of set Sp to lower bound
    n_p: usize,
}

impl Round {
    /// Oracle producing a uniformly random value in [1, n_p] used for round candidates
    /// We also return hash(data) to follow the optimization presented in Section 3.3
    fn h1(data: Vec<Vec<u8>>, n_p: usize) -> ([u8; 32], usize) {
        let digest = utils::combine_hashes(data);
        return (digest, utils::oracle(&digest, n_p));
    }

    /// Output a round from a proof counter and n_p
    /// Initilialises the hash with H1(t) and random value as oracle(H1(t), n_p)
    pub fn new(v: usize, t: usize, n_p: usize) -> Round {
        let mut data = Vec::new();
        data.push(v.to_ne_bytes().to_vec());
        data.push(t.to_ne_bytes().to_vec());
        let (h, h_usize) = Round::h1(data, n_p);
        Round {
            v,
            t,
            s_list: Vec::new(),
            h: h.to_vec(),
            h_usize,
            n_p,
        }
    }

    /// Updates a round with an element of S_p
    /// Replaces the hash $h$ with $h' = H1(h, s)$ and the random value as oracle(h', n_p)
    pub fn update(r: &Round, s: [u8; 32]) -> Round {
        let mut s_list = r.s_list.clone();
        s_list.push(s);
        let mut data = Vec::new();
        data.push(r.h.clone());
        data.push(s.to_vec());
        let (h, h_usize) = Round::h1(data, r.n_p);
        Round {
            v: r.v,
            t: r.t,
            s_list,
            h: h.to_vec(),
            h_usize,
            n_p: r.n_p,
        }
    }
}

#[derive(Debug, Clone)]
/// Alba proof
pub struct Proof {
    /// Proof counter
    r: usize,
    /// Proof 2nd counter
    d: usize,
    /// Proof tuple
    items: Vec<[u8; 32]>,
}

impl Proof {
    /// Returns an empty proof
    fn empty() -> Self {
        Proof {
            r: 0,
            d: 0,
            items: Vec::new(),
        }
    }

    /// Oracle producing a uniformly random value in [1, n_p] used for prehashing S_p
    // TODO: We also return hash(data) to follow the optimization presented in Section 3.3
    fn h0(setup: &Setup, v: usize, s: [u8; 32]) -> usize {
        let mut data = Vec::new();
        data.push(v.to_ne_bytes().to_vec());
        data.push(s.to_vec());
        let digest = utils::combine_hashes(data);
        // return (digest, utils::oracle(&digest, setup.n_p));
        return utils::oracle(&digest, setup.n_p);
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
    fn h2(setup: &Setup, r: &Round) -> bool {
        let mut data = Vec::new();
        data.push(r.v.to_ne_bytes().to_vec());
        data.push(r.t.to_ne_bytes().to_vec());
        for s in &r.s_list {
            data.push(s.clone().to_vec());
        }
        let digest = utils::combine_hashes(data);
        return utils::oracle(&digest, setup.q) == 0;
    }

    /// Depth-first search which goes through all potential round candidates
    /// and returns first round candidate Round{t, x_1, ..., x_u)} such that:
    /// - for all i ∈ [0, u-1], H0(x_i+1) ∈ bins[H1(t, x_1, ..., x_i)]
    /// - H2(t, x_0, ..., x_u) = true
    fn dfs(
        setup: &Setup,
        bins: &Vec<Vec<[u8; 32]>>,
        round: &Round,
        limit: &mut usize,
    ) -> Option<Proof> {
        // If the round candidate is at the last iteration (len(round) == u)
        if round.s_list.len() == setup.u {
            // if H2(t, x_0, ..., x_u) = true, return candidate as proof
            if Proof::h2(setup, round) {
                let r = round.v;
                let d = round.t;
                let items = round.s_list.clone();
                return Some(Proof { r, d, items });
            } else {
                return None;
            }
        }
        // Otherwise, update round candidate with all s in bins[H1(t, x_1, ..., x_i)] and continue
        let result = bins[round.h_usize].iter().find_map(|&s| {
            if *limit == 0 {
                return None;
            }
            *limit -= 1;
            Self::dfs(setup, bins, &Round::update(round, s), limit)
        });
        return result;
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Returns an empty proof if no suitable candidate is found.
    fn prove_index(setup: &Setup, set: &Vec<[u8; 32]>, v: usize) -> (usize, Option<Proof>) {
        // Initialising our n_p bins with all s ∈ Sp
        let mut bins: Vec<Vec<[u8; 32]>> = Vec::new();
        for _ in 1..(setup.n_p + 1) {
            bins.push(Vec::new());
        }
        for &s in set.iter() {
            // TODO: add H1(s),
            // let (hs, index_s) = Proof::h0(setup, s);
            bins[Proof::h0(setup, v, s)].push(s);
        }
        // Initialising max number of steps
        let mut limit = setup.b;
        // Attempting to generate a proof for $d$ rounds
        for t in 1..(setup.d + 1) {
            if limit == 0 {
                return (0, None);
            }
            limit = limit - 1;
            let round = Round::new(v, t, setup.n_p);
            let res = Proof::dfs(setup, &bins, &round, &mut limit);
            if res.is_some() {
                return (limit, res);
            }
        }
        // If no proof found, return None
        return (limit, None);
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Returns an empty proof if no suitable candidate is found.
    pub fn prove(setup: &Setup, set: &Vec<[u8; 32]>) -> Self {
        // Initialising our n_p bins with all s ∈ Sp
        for v in 0..setup.r {
            if let (_, Some(proof)) = Proof::prove_index(setup, set, v) {
                return proof;
            }
        }
        // If no proof found, return empty proof
        return Proof::empty();
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Returns an empty proof if no suitable candidate is found.
    pub fn bench(setup: &Setup, set: &Vec<[u8; 32]>) -> (usize, Self) {
        // Initialising our n_p bins with all s ∈ Sp
        let mut nb_calls = 0;
        for v in 0..setup.r {
            let (steps, opt) = Proof::prove_index(setup, set, v);
            // Adding nb of steps done in prove_index
            nb_calls += setup.b - steps;
            if let Some(proof) = opt {
                return (nb_calls, proof);
            }
        }
        // If no proof found, return empty proof
        return (nb_calls, Proof::empty());
    }

    /// Alba's verification algorithm, follows proving algorithm by running the
    /// same depth-first search algorithm.
    pub fn verify(setup: &Setup, proof: Proof) -> bool {
        if proof.d == 0 || proof.d > setup.d || proof.r > setup.r || proof.items.len() != setup.u {
            return false;
        }
        let r0 = Round::new(proof.r, proof.d, setup.n_p);
        let (b, round) = proof.items.iter().fold((true, r0), |(b, r), &s| {
            (
                b && r.h_usize == Proof::h0(setup, proof.r, s),
                Round::update(&r, s),
            )
        });
        return b && Proof::h2(setup, &round);
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    #[test]
    fn test_verify() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let nb_tests = 1_000;
        for _t in 0..nb_tests {
            let seed = rng.next_u32().to_ne_bytes().to_vec();
            let s_p = utils::gen_items(seed, 100);
            let params = Params {
                lambda_sec: 10,
                lambda_rel: 10,
                n_p: 80,
                n_f: 20,
            };
            let setup = Setup::new(&params);
            let proof = Proof::prove(&setup, &s_p);
            assert!(Proof::verify(&setup, proof.clone()));
            let proof_0 = Proof {
                r: proof.r,
                d: 0,
                items: proof.items.clone(),
            };
            assert!(!Proof::verify(&setup, proof_0));
            let proof_d = Proof {
                r: proof.r,
                d: proof.d.wrapping_add(1),
                items: proof.items.clone(),
            };
            assert!(!Proof::verify(&setup, proof_d));
            let proof_r = Proof {
                r: proof.r.wrapping_add(1),
                d: proof.d,
                items: proof.items.clone(),
            };
            assert!(!Proof::verify(&setup, proof_r));
            let proof_item = Proof {
                r: proof.r,
                d: proof.d,
                items: Vec::new(),
            };
            assert!(!Proof::verify(&setup, proof_item));
            let mut wrong_items = proof.items.clone();
            let last_item = wrong_items.pop().unwrap();
            let mut penultimate_item = wrong_items.pop().unwrap();
            let proof_itembis = Proof {
                r: proof.r,
                d: proof.d,
                items: wrong_items.clone(),
            };
            assert!(!Proof::verify(&setup, proof_itembis));
            // Modifying the penultimate item to check correctness of H1 check and not H2
            penultimate_item[0] = penultimate_item[0].wrapping_add(42u8);
            wrong_items.push(penultimate_item);
            wrong_items.push(last_item);
            let proof_itembis = Proof {
                r: proof.r,
                d: proof.d,
                items: wrong_items.clone(),
            };
            assert!(!Proof::verify(&setup, proof_itembis));
        }
    }
}
