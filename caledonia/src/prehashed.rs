//! Rust implementation of ALBA's prehashed scheme using Blake2b as hash
//! function, working for big sets.

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
    pub d: usize,
    /// Inverse of probability p_q
    pub q: usize,
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

        // Converting params to f64
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;
        let lognpnf = log_2(n_p_f64 / n_f_f64);
        let lambda_sec = params.lambda_sec as f64;
        let lambda_rel = params.lambda_rel as f64;

        // We define the parameters according to Section 3.2, Corrollary 2
        let u_f64 = (lambda_sec + log_2(lambda_rel + log3) + 1.0 - logloge) / lognpnf;
        let u = u_f64.ceil() as usize;
        let d_f64 = 16.0 * u_f64 * (lambda_rel + log3) / loge;
        let d = d_f64.ceil() as usize;
        let q = (2.0 * (lambda_rel + log3) / (d_f64 * loge)).recip().ceil() as usize;

        let check = ((d_f64 * d_f64 * loge) / (9.0 * (lambda_rel + log3))).ceil() as usize;
        assert!(params.n_p >= check);

        Setup {
            n_p: params.n_p,
            u,
            d,
            q,
        }
    }
}

/// Round parameters
#[derive(Debug, Clone)]
pub struct Round {
    /// Proof counter
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
    pub fn new(t: usize, n_p: usize) -> Round {
        let data = [t.to_ne_bytes().to_vec()].to_vec();
        let (h, h_usize) = Round::h1(data, n_p);
        Round {
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
    d: usize,
    /// Proof tuple
    items: Vec<[u8; 32]>,
}

impl Proof {
    /// Oracle producing a uniformly random value in [1, n_p] used for prehashing S_p
    // TODO: We also return hash(data) to follow the optimization presented in Section 3.3
    fn h0(setup: &Setup, s: [u8; 32]) -> usize {
        let mut data = Vec::new();
        data.push(s.to_vec());
        let digest = utils::combine_hashes(data);
        // return (digest, utils::oracle(&digest, setup.n_p));
        return utils::oracle(&digest, setup.n_p);
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
    fn h2(setup: &Setup, r: &Round) -> bool {
        let mut data = Vec::new();
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
    fn dfs(setup: &Setup, bins: &Vec<Vec<[u8; 32]>>, round: &Round) -> Option<Proof> {
        // If the round candidate is at the last iteration (len(round) == u)
        if round.s_list.len() == setup.u {
            // if H2(t, x_0, ..., x_u) = true, return candidate as proof
            if Proof::h2(setup, round) {
                let d = round.t;
                let items = round.s_list.clone();
                return Some(Proof { d, items });
            } else {
                return None;
            }
        }
        // Otherwise, update round candidate with all s in bins[H1(t, x_1, ..., x_i)] and continue
        let result = bins[round.h_usize]
            .iter()
            .find_map(|&s| Self::dfs(setup, bins, &Round::update(round, s)));
        return result;
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Returns an empty proof if no suitable candidate is found.
    pub fn prove(setup: &Setup, set: &Vec<[u8; 32]>) -> Self {
        // Initialising our n_p bins with all s ∈ Sp
        let mut bins: Vec<Vec<[u8; 32]>> = Vec::new();
        for _ in 1..(setup.n_p + 1) {
            bins.push(Vec::new());
        }
        for &s in set.iter() {
            // TODO: add H1(s)
            // let (hs, index_s) = Proof::h0(setup, s);
            bins[Proof::h0(setup, s)].push(s);
        }

        // Attempting to generate a proof for $d$ rounds
        for t in 1..(setup.d + 1) {
            let round = Round::new(t, setup.n_p);
            if let Some(proof) = Proof::dfs(setup, &bins, &round) {
                return proof;
            };
        }

        // If no proof found, return empty proof
        return Proof {
            d: 0,
            items: Vec::new(),
        };
    }

    /// Alba's verification algorithm, follows proving algorithm by running the
    /// same depth-first search algorithm.
    pub fn verify(setup: &Setup, proof: Proof) -> bool {
        if proof.d == 0 || proof.d > setup.d || proof.items.len() != setup.u {
            return false;
        }
        let r0 = Round::new(proof.d, setup.n_p);
        let (b, round) = proof.items.iter().fold((true, r0), |(b, r), &s| {
            (b && r.h_usize == Proof::h0(setup, s), Round::update(&r, s))
        });
        return b && Proof::h2(setup, &round);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verify() {
        let s_p = utils::gen_items(1_000);
        let params = Params {
            lambda_sec: 10,
            lambda_rel: 10,
            n_p: 800,
            n_f: 2,
        };
        let setup = Setup::new(&params);
        let proof = Proof::prove(&setup, &s_p);
        assert!(Proof::verify(&setup, proof.clone()));
        let proof_0 = Proof {
            d: 0,
            items: proof.items.clone(),
        };
        assert!(!Proof::verify(&setup, proof_0));
        let proof_d = Proof {
            d: 1_000_000,
            items: proof.items.clone(),
        };
        assert!(!Proof::verify(&setup, proof_d));
        let proof_item = Proof {
            d: proof.d,
            items: Vec::new(),
        };
        assert!(!Proof::verify(&setup, proof_item));
        let mut wrong_items = proof.items.clone();
        let _ = wrong_items.pop();
        let proof_itembis = Proof {
            d: proof.d,
            items: wrong_items.clone(),
        };
        assert!(!Proof::verify(&setup, proof_itembis));
        wrong_items.push([0u8; 32]);
        let proof_itembis = Proof {
            d: proof.d,
            items: wrong_items,
        };
        assert!(!Proof::verify(&setup, proof_itembis));
    }

    #[test]
    fn test_prove() {
        use std::time::Instant;
        let npnf = [(1_000, 8)];
        // Other working tests: (10_000, 2_000), (100_000, 60_000)
        let lambdas = [5, 10];
        let nb_tests = 10;
        for (n_p, n_f) in npnf {
            let mut u = 0;
            let mut time_setup = 0;
            let mut time_prove = 0;
            let mut time_verify = 0;
            for lambda in lambdas {
                let s_p: Vec<[u8; 32]> = utils::gen_items(n_p);
                for _t in 0..nb_tests {
                    let params = Params {
                        lambda_sec: lambda,
                        lambda_rel: lambda,
                        n_p,
                        n_f,
                    };
                    // Setup
                    let start_setup = Instant::now();
                    let setup = Setup::new(&params);
                    let end_setup = start_setup.elapsed();
                    time_setup += end_setup.as_nanos();
                    u = setup.u;
                    // Prove
                    let start_prove = Instant::now();
                    let proof = Proof::prove(&setup, &s_p);
                    let end_prove = start_prove.elapsed();
                    time_prove += end_prove.as_nanos();
                    // Verify
                    let start_verify = Instant::now();
                    let b = Proof::verify(&setup, proof.clone());
                    let end_verify = start_verify.elapsed();
                    time_verify += end_verify.as_nanos();
                    assert!(b);
                }

                println!(
                    "(n_p={}, n_f={}, λ={}): \t u={}, \t setup:{}, \t prove:{}, \t verify:{}",
                    utils::format_nb(n_p),
                    utils::format_nb(n_f),
                    lambda,
                    utils::format_nb(u),
                    utils::format_time(time_setup / nb_tests),
                    utils::format_time(time_prove / nb_tests),
                    utils::format_time(time_verify / nb_tests)
                );
            }
        }
    }
}
