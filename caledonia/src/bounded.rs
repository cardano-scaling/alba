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
pub enum Cases {
    /// Case where u =< λ^2
    Small,
    /// Case where λ^2 < u < λ^3
    Mid,
    /// Case where u >= λ^3
    High,
}

impl Params {
    /// Returns information on which case corresponds some parameter
    pub fn which_case(&self) -> (Cases, usize) {
        let lsec = self.lambda_sec as f64;
        let lrel = self.lambda_rel as f64;
        let np = self.n_p as f64;
        let nf = self.n_f as f64;
        let loge = E.log2();

        let lognpnf = (np / nf).log2();
        let u_f64 = (lsec + lrel.log2() + 5.0 - loge.log2()) / lognpnf;
        let u = u_f64.ceil() as u64;

        let ratio = 9.0 * np * loge / ((17 * u).pow(2) as f64);
        let s1 = ratio - 7.0;
        let s2 = ratio - 2.0;

        if s1 < 1.0 || s1 > lrel || s2 < 1.0 || s2 > lrel {
            return (Cases::Small, u as usize);
        }

        let lrel2 = lrel.min(s2);
        if (u as f64) < lrel2 {
            return (Cases::Mid, u as usize);
        } else {
            return (Cases::High, u as usize);
        }
    }
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
        // Misc values and functions
        let loge = E.log2();
        fn compute_w(u: f64, l: f64) -> f64 {
            fn factorial_check(w: f64, l: f64) -> bool {
                let bound = 0.5f64.powf(l);
                let factors: Vec<u64> = (1..=(w as u64 + 1)).rev().collect();
                let mut ratio = (14.0 * w * w * (w + 2.0) * E.powf((w + 1.0) / w))
                    / (E * (w + 2.0 - E.powf(1.0 / w)));

                for f in factors {
                    ratio /= f as f64;
                    if ratio <= bound {
                        return true;
                    }
                }
                return false;
            }
            let mut w: f64 = u;
            while !factorial_check(w, l) {
                w += 1.0;
            }
            w
        }

        // Converting params to f64
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;
        let lognpnf = (n_p_f64 / n_f_f64).log2();
        let lambda_rel = params.lambda_rel as f64;
        let logrel = lambda_rel.log2();
        let lambda_sec = (params.lambda_sec as f64) + logrel;

        // Computing the proof size u
        let u_f64 = ((lambda_sec + logrel + 5.0 - loge.log2()) / lognpnf).ceil();
        let u = u_f64 as usize;

        // Computing param depending on case
        let ratio = 9.0 * n_p_f64 * loge / ((17 * u).pow(2) as f64);
        let s1 = ratio - 7.0;
        let s2 = ratio - 2.0;

        if s1 < 1.0 || s1 > lambda_rel || s2 < 1.0 || s2 > lambda_rel {
            // Small case, ie n_p <= λ^2
            let ln12 = (12f64).ln();
            let d = (32.0 * ln12 * u_f64).ceil();
            return Setup {
                n_p: params.n_p,
                u,
                r: params.lambda_rel,
                d: d as usize,
                q: (2.0 * ln12 / d).recip().ceil() as usize,
                b: (8.0 * (u_f64 + 1.0) * d / ln12).floor() as usize,
            };
        }
        let lambda_rel2 = lambda_rel.min(s2);
        if u_f64 < lambda_rel2 {
            // Case 3, Theorem 14, ie  n_p >= λ^3
            let d = (16.0 * u_f64 * (lambda_rel2 + 2.0) / loge).ceil();
            assert!(n_p_f64 >= d * d * loge / (9.0 * (lambda_rel2 + 2.0)));
            return Setup {
                n_p: params.n_p,
                u,
                r: (lambda_rel / lambda_rel2).ceil() as usize,
                d: d as usize,
                q: (2.0 * (lambda_rel2 + 2.0) / (d * loge)).recip().ceil() as usize,
                b: (((lambda_rel2 + 2.0 + u_f64.log2()) / (lambda_rel2 + 2.0))
                    * (3.0 * u_f64 * d / 4.0)
                    + d
                    + u_f64)
                    .floor() as usize,
            };
        } else {
            // Case 2, Theorem 13, ie λ^3 > n_p > λ^2
            let lambda_rel1 = lambda_rel.min(s1);
            let lbar = (lambda_rel1 + 7.0) / loge;
            let d = (16.0 * u_f64 * lbar).ceil();
            assert!(n_p_f64 >= d * d / (9.0 * lbar));

            let w = compute_w(u_f64, lambda_rel1);
            return Setup {
            n_p: params.n_p,
            u,
                r: (lambda_rel / lambda_rel1).ceil() as usize,
                d: d as usize,
                q: (2.0 * lbar / d).recip().ceil() as usize,
                b: (((w * lbar) / d + 1.0)
                    * E.powf(2.0 * u_f64 * w * lbar / n_p_f64 + 7.0 * u_f64 / w)
                    * d
                    * u_f64
                    + d)
                    .floor() as usize,
            };
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
    fn test_params() {
        let lambdas = [10, 80, 100, 128];
        let pows: Vec<u32> = (2..10).collect();
        let sps: Vec<usize> = pows.iter().map(|&i| 10_u32.pow(i) as usize).collect();
        let ratios = [60, 66, 80, 95, 99];
        let mut params = Vec::new();
        for l in lambdas {
            for &sp in &sps {
                for r in ratios {
                    params.push(Params {
                        lambda_sec: l,
                        lambda_rel: l,
                        n_p: (sp * r) / 100,
                        n_f: (sp * (100 - r)) / 100,
                    })
                }
            }
        }

        let mut smalls = Vec::new();
        let mut mids = Vec::new();
        let mut highs = Vec::new();
        for p in params {
            match Params::which_case(&p) {
                (Cases::Small, u) => smalls.push((p.clone(), u)),
                (Cases::Mid, u) => mids.push((p.clone(), u)),
                (Cases::High, u) => highs.push((p.clone(), u)),
            }
        }


        println!("------------ Small cases");
        for s in smalls {
            println!("{:?}", s);
        }
        println!("\n------------ Mid cases");
        for s in mids {
            println!("{:?}", s);
        }
        println!("\n------------ High cases");
        for s in highs {
            println!("{:?}", s);
        }
    }

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

    #[test]
    fn test_prove() {
        use std::time::Instant;
        let npnf = [(100, 20), (1_000, 8), (1_000, 200)];
        let lambdas = [10, 20];
        let nb_tests = 100;
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        for (n_p, n_f) in npnf {
            for lambda in lambdas {
                let mut u = 0;
                let mut time_setup = 0;
                let mut time_prove = 0;
                let mut time_verify = 0;
                let mut max_bench: usize = 0;
                let mut mean_bench: usize = 0;
                let mut max_retrial: usize = 0;
                let mut mean_retrial: usize = 0;
                for _t in 0..nb_tests {
                    let seed_u32 = rng.next_u32();
                    let seed = seed_u32.to_ne_bytes().to_vec();
                    let s_p: Vec<[u8; 32]> = utils::gen_items(seed, n_p);
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
                    let (steps, proof) = Proof::bench(&setup, &s_p);
                    let end_prove = start_prove.elapsed();
                    time_prove += end_prove.as_nanos();
                    max_bench = std::cmp::max(max_bench, steps);
                    mean_bench += steps;
                    max_retrial = std::cmp::max(max_retrial, proof.r);
                    mean_retrial += proof.r;
                    // Verify
                    let start_verify = Instant::now();
                    let b = Proof::verify(&setup, proof.clone());
                    let end_verify = start_verify.elapsed();
                    time_verify += end_verify.as_nanos();
                    assert!(b);
                }
                println!(
                    "(n_p={}, n_f={}, λ={}): \t u={}, \t setup:{}, \t prove:{}, \t verify:{}, \t max steps:{}, \t mean steps:{}, \t max retrial:{}, \t mean retrial:{}",
                    utils::format_nb(n_p),
                    utils::format_nb(n_f),
                    utils::format_nb(lambda),
                    utils::format_nb(u),
                    utils::format_time(time_setup / nb_tests),
                    utils::format_time(time_prove / nb_tests),
                    utils::format_time(time_verify / nb_tests),
                    utils::format_nb(max_bench),
                    utils::format_nb((mean_bench as u128 / nb_tests) as usize),
                    max_retrial,
                    mean_retrial as u128 / nb_tests,
                );
            }
        }
    }
}
