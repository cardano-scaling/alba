//! Rust implementation of ALBA's Telescope scheme using Blake2b as hash
//! function.

use crate::utils;
extern crate core;
use std::f64::consts::E;

const DATA_LENGTH: usize = 32;
const DIGEST_SIZE: usize = 32;

type Data = [u8; DATA_LENGTH];

type Hash = [u8; DIGEST_SIZE];

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
    /// Expected number of participants
    pub mu: usize,
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
    /// Returns the minimum mu, found by dichotomic search,
    /// so that the soundness error is <= 2^-lambda_sec
    pub fn min_mu(lambda_sec: usize, lambda_rel: usize, n_p: usize, n_f: usize) -> usize {
        fn compute_bounds(lsec: f64, lrel: f64, np: f64, nf: f64, mu: f64) -> bool {
            let npnf = np / nf;
            let lognpnf = npnf.log2();
            let loge = E.log2();
            let ln12 = (12f64).ln();

            let bound_completeness = 2f64 * (lrel + 1f64) / loge;

            let delta = (bound_completeness / mu).sqrt();
            let rho = ((1f64 - delta) * mu).ceil();
            let rhomu = rho / mu as f64;
            let logrhomu = rhomu.log2();
            let u_f64 = ((lsec + (lrel + 1f64).log2() + 1f64 + loge + ln12.log2())
                / (lognpnf + logrhomu))
                .ceil();

            // Soudness check
            let bound_soudness = npnf * u_f64 * u_f64;
            return (mu > bound_completeness) && (mu >= bound_soudness) && (npnf * rhomu > 1f64);
        }
        let (np, nf) = (n_p as f64, n_f as f64);
        let (mut lower, mut upper) = (0.0, np);
        let mut mu = 0.5 * np;
        while mu != np {
            let b = compute_bounds(lambda_sec as f64, lambda_rel as f64, np, nf, mu);
            if b {
                upper = mu;
            } else {
                lower = mu;
            }
            let new_mu = ((lower + upper) / 2.0).ceil();
            if mu == new_mu && b {
                return mu as usize;
            }
            mu = new_mu;
        }
        return mu as usize;
    }

    pub fn new(lambda_sec: usize, lambda_rel: usize, n_p: usize, n_f: usize) -> Params {
        let mu = Params::min_mu(lambda_sec, lambda_rel, n_p, n_f);
        return Params {
            lambda_sec,
            lambda_rel,
            n_p,
            n_f,
            mu,
        };
    }
}

/// Setup output parameters
#[derive(Debug, Clone)]
pub struct Setup {
    /// Approximate size of set Sp to lower bound
    pub n_p_lottery: usize,
    /// Post lottery Alba np
    pub n_p: usize,
    /// Expected number of participants
    pub mu: usize,
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
    /// Follows Theorem 16 and 17
    pub fn new(params: &Params) -> Self {
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;
        let npnf = n_p_f64 / n_f_f64;
        let lognpnf = npnf.log2();
        let lambda_rel = params.lambda_rel as f64;
        let lambda_sec = params.lambda_sec as f64;
        let mu = params.mu as f64;
        let loge = E.log2();
        let ln12 = (12f64).ln();

        // Completness check
        let mu_completeness = 2f64 * (lambda_rel + 1f64) / loge;
        assert!(mu == n_p_f64 || mu > mu_completeness);

        let delta = (mu_completeness / mu).sqrt();
        let rho = ((1f64 - delta) * mu).ceil();
        let rhomu = rho / mu as f64;
        let logrhomu = rhomu.log2();
        let u_f64 = (lambda_sec + (lambda_rel + 1f64).log2() + 1f64 + loge + ln12.log2())
            / (lognpnf + logrhomu);
        let d = (32.0 * ln12 * u_f64).ceil();
        let q = (2.0 * ln12 / d).recip().ceil();

        // Soudness check
        let mu_soundness = npnf * u_f64 * u_f64;
        assert!(mu == n_p_f64 || mu >= mu_soundness);
        assert!(mu == n_p_f64 || npnf * rhomu > 1f64);

        // let soundness_error = q
        //     * d
        //     * (lambda_rel + 1.0)
        //     * (npnf.recip() * rhomu.recip()).powf(u_f64)
        //     * E.powf(u_f64 * u_f64 * npnf / mu);
        // println!("Soundness error: {}", soundness_error);

        return Setup {
            n_p_lottery: params.n_p,
            n_p: rho as usize,
            mu: params.mu,
            u: u_f64 as usize,
            r: params.lambda_rel + 1,
            d: d as usize,
            q: q as usize,
            b: (8.0 * (u_f64 + 1.0) * d / ln12).floor() as usize,
        };
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
    s_list: Vec<Data>,
    /// Round candidate hash
    h: Hash,
    /// Round candidate hash mapped to [1, n_p]
    h_usize: usize,
    /// Approximate size of set Sp to lower bound
    n_p: usize,
}

impl Round {
    /// Oracle producing a uniformly random value in [1, n_p] used for round candidates
    /// We also return hash(data) to follow the optimization presented in Section 3.3
    fn h1(data: Vec<Vec<u8>>, n_p: usize) -> (Hash, usize) {
        let digest = utils::combine_hashes::<DIGEST_SIZE>(data);
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
            h: h,
            h_usize,
            n_p,
        }
    }

    /// Updates a round with an element of S_p
    /// Replaces the hash $h$ with $h' = H1(h, s)$ and the random value as oracle(h', n_p)
    pub fn update(r: &Round, s: Data) -> Round {
        let mut s_list = r.s_list.clone();
        s_list.push(s);
        let mut data = Vec::new();
        data.push(r.h.clone().to_vec());
        data.push(s.to_vec());
        let (h, h_usize) = Round::h1(data, r.n_p);
        Round {
            v: r.v,
            t: r.t,
            s_list,
            h: h,
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
    items: Vec<Data>,
}

impl Proof {
    /// Returns a new proof
    fn new() -> Self {
        Proof {
            r: 0,
            d: 0,
            items: Vec::new(),
        }
    }

    /// Lottery (Section 4.1 of ALBA paper) scheme using oracle outputing 1
    /// with probability p = mu / np where mu (resp. np) is the expected (resp.
    /// total) number of participants
    pub fn lottery(np: usize, mu: usize, s: Data) -> bool {
        let mut data = Vec::new();
        data.push(s.to_vec());
        let digest = utils::combine_hashes::<DIGEST_SIZE>(data);
        let proba: f64 = mu as f64 / np as f64;
        let inverse_proba = proba.recip().ceil() as usize;
        return utils::oracle(&digest, inverse_proba) == 0;
    }

    /// Oracle producing a uniformly random value in [1, n_p] used for prehashing S_p
    fn h0(setup: &Setup, v: usize, s: Data) -> usize {
        let mut data = Vec::new();
        data.push(v.to_ne_bytes().to_vec());
        data.push(s.to_vec());
        let digest = utils::combine_hashes::<DIGEST_SIZE>(data);
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
        let digest = utils::combine_hashes::<DIGEST_SIZE>(data);
        return utils::oracle(&digest, setup.q) == 0;
    }

    /// Depth-first search which goes through all potential round candidates
    /// and returns first round candidate Round{t, x_1, ..., x_u)} such that:
    /// - for all i ∈ [0, u-1], H0(x_i+1) ∈ bins[H1(t, x_1, ..., x_i)]
    /// - H2(t, x_0, ..., x_u) = true
    fn dfs(
        setup: &Setup,
        bins: &Vec<Vec<Data>>,
        round: &Round,
        nb_steps: &mut usize,
    ) -> Option<Proof> {
        if round.s_list.len() == setup.u {
            if Proof::h2(setup, round) {
                let r = round.v;
                let d = round.t;
                let items = round.s_list.clone();
                return Some(Proof { r, d, items });
            } else {
                return None;
            }
        }
        let result = bins[round.h_usize].iter().find_map(|&s| {
            if *nb_steps == setup.b {
                return None;
            }
            *nb_steps += 1;
            Self::dfs(setup, bins, &Round::update(round, s), nb_steps)
        });
        return result;
    }

    /// Indexed proving algorithm, returns an empty proof if no suitable
    /// candidate is found within the setup.b steps.
    fn prove_index(setup: &Setup, set: &Vec<Data>, v: usize) -> (usize, Option<Proof>) {
        let mut bins: Vec<Vec<Data>> = Vec::new();
        for _ in 1..(setup.n_p + 1) {
            bins.push(Vec::new());
        }
        for &s in set.iter() {
            bins[Proof::h0(setup, v, s)].push(s);
        }
        let mut nb_steps = 0;
        for t in 1..(setup.d + 1) {
            if nb_steps == setup.b {
                return (0, None);
            }
            nb_steps += 1;
            let round = Round::new(v, t, setup.n_p);
            let res = Proof::dfs(setup, &bins, &round, &mut nb_steps);
            if res.is_some() {
                return (nb_steps, res);
            }
        }
        return (nb_steps, None);
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Calls up to setup.r times the prove_index function and returns an empty
    /// proof if no suitable candidate is found.
    pub fn prove(setup: &Setup, set: &Vec<Data>) -> Self {
        // Lottery must be done by each participant on its own, we have here
        // the aggregator running it another time for robustness.
        let winner_set = set
            .iter()
            .filter_map(|&s| Proof::lottery(setup.n_p_lottery, setup.mu, s).then(|| s))
            .collect();

        for v in 0..setup.r {
            if let (_, Some(proof)) = Proof::prove_index(setup, &winner_set, v) {
                return proof;
            }
        }
        return Proof::new();
    }

    /// Alba's proving algorithm used for benchmarking, returning a proof as
    /// well as the number of  steps ran to find it.
    pub fn bench(setup: &Setup, set: &Vec<Data>) -> (usize, Self) {
        let mut nb_steps = 0;
        for v in 0..setup.r {
            let (steps, opt) = Proof::prove_index(setup, set, v);
            nb_steps += steps;
            if let Some(proof) = opt {
                return (nb_steps, proof);
            }
        }
        return (nb_steps, Proof::new());
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
                b && r.h_usize == Proof::h0(setup, proof.r, s)
                    && Proof::lottery(setup.n_p_lottery, setup.mu, s),
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
        let lambdas = [80, 100, 128];
        let pows: Vec<u32> = (2..10).collect();
        let sps: Vec<usize> = pows.iter().map(|&i| 10_u32.pow(i) as usize).collect();
        let ratios = [60, 66, 80, 95, 99];
        let mut params = Vec::new();
        for l in lambdas {
            for &sp in &sps {
                for r in ratios {
                    let n_p = (sp * r) / 100;
                    let n_f = (sp * (100 - r)) / 100;
                    let p = Params::new(l, l, n_p, n_f);
                    params.push(p);
                }
            }
        }
    }

    #[test]
    fn test_verify() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let nb_tests = 100;
        let set_size = 100_000;
        let lambda = 80;
        let np = 60;
        let nf = 100 - np;
        for _t in 0..nb_tests {
            let seed = rng.next_u32().to_ne_bytes().to_vec();
            let params = Params::new(lambda, lambda, set_size * np / 100, set_size * nf / 100);
            let s_p = utils::gen_items::<DATA_LENGTH>(seed, set_size)
                .iter()
                .filter_map(|&s| Proof::lottery(params.n_p, params.mu, s).then(|| s))
                .collect();
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
