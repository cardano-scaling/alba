//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

use crate::utils;

use std::{f32::consts::LOG2_E, f64::consts::E};

const DATA_LENGTH: usize = 32;
const DIGEST_SIZE: usize = 32;

type Element = [u8; DATA_LENGTH];
type Hash = [u8; DIGEST_SIZE];

/// Setup input parameters
#[derive(Debug)]
pub struct Params {
    /// Soundness security parameter
    pub lambda_sec: u32,
    /// Completeness security parameter
    pub lambda_rel: u32,
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
        let loge = LOG2_E as f64;

        let lognpnf = (np / nf).log2();
        let u_f64 = (lsec + lrel.log2() + 5.0 - loge.log2()) / lognpnf;
        let u = u_f64.ceil() as u64;

        let ratio = 9.0 * np * loge / ((17 * u).pow(2) as f64);
        let s1 = ratio - 7.0;
        let s2 = ratio - 2.0;

        if s1 < 1.0 || s2 < 1.0 {
            return (Cases::Small, u as usize);
        }

        let lrel2 = lrel.min(s2);
        if (u as f64) < lrel2 {
            (Cases::Mid, u as usize)
        } else {
            (Cases::High, u as usize)
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
    pub r: u32,
    /// Proof max 2nd counter
    pub d: usize,
    /// Probability q
    pub q: f64,
    /// Computation bound
    pub b: usize,
}
impl Setup {
    /// Setup algorithm taking a Params as input and returning setup parameters (u,d,q)
    pub fn new(params: &Params) -> Self {
        fn compute_w(u: f64, l: f64) -> f64 {
            fn factorial_check(w: f64, l: f64) -> bool {
                let bound = 0.5f64.powf(l);
                let factors: Vec<u64> = (1..=(w as u64 + 2)).rev().collect();
                let mut ratio = (14.0 * w * w * (w + 2.0) * E.powf((w + 1.0) / w))
                    / (E * (w + 2.0 - E.powf(1.0 / w)));

                for f in factors {
                    ratio /= f as f64;
                    if ratio <= bound {
                        return true;
                    }
                }
                false
            }
            let mut w: f64 = u;
            while !factorial_check(w, l) {
                w += 1.0;
            }
            w
        }

        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;
        let lognpnf = (n_p_f64 / n_f_f64).log2();
        let lambda_rel = params.lambda_rel as f64;
        let logrel = lambda_rel.log2();
        let lambda_sec = (params.lambda_sec as f64) + logrel;
        let loge = LOG2_E as f64;

        let u_f64 = ((lambda_sec + logrel + 5.0 - loge.log2()) / lognpnf).ceil();
        let u = u_f64 as usize;

        let ratio = 9.0 * n_p_f64 * loge / ((17 * u).pow(2) as f64);
        let s1 = ratio - 7.0;
        let s2 = ratio - 2.0;

        if s1 < 1.0 || s2 < 1.0 {
            // Small case, ie n_p <= λ^2
            let ln12 = (12f64).ln();
            let d = (32.0 * ln12 * u_f64).ceil();
            return Setup {
                n_p: params.n_p,
                u,
                r: params.lambda_rel,
                d: d as usize,
                q: (2.0 * ln12 / d),
                b: (8.0 * (u_f64 + 1.0) * d / ln12).floor() as usize,
            };
        }
        let lambda_rel2 = lambda_rel.min(s2);
        if u_f64 < lambda_rel2 {
            // Case 3, Theorem 14, ie  n_p >= λ^3
            let d = (16.0 * u_f64 * (lambda_rel2 + 2.0) / loge).ceil();
            assert!(n_p_f64 >= d * d * loge / (9.0 * (lambda_rel2 + 2.0)));
            Setup {
                n_p: params.n_p,
                u,
                r: (lambda_rel / lambda_rel2).ceil() as u32,
                d: d as usize,
                q: (2.0 * (lambda_rel2 + 2.0) / (d * loge)),
                b: (((lambda_rel2 + 2.0 + u_f64.log2()) / (lambda_rel2 + 2.0))
                    * (3.0 * u_f64 * d / 4.0)
                    + d
                    + u_f64)
                    .floor() as usize,
            }
        } else {
            // Case 2, Theorem 13, ie λ^3 > n_p > λ^2
            let lambda_rel1 = lambda_rel.min(s1);
            let lbar = (lambda_rel1 + 7.0) / loge;
            let d = (16.0 * u_f64 * lbar).ceil();
            assert!(n_p_f64 >= d * d / (9.0 * lbar));

            let w = compute_w(u_f64, lambda_rel1);
            Setup {
                n_p: params.n_p,
                u,
                r: (lambda_rel / lambda_rel1).ceil() as u32,
                d: d as usize,
                q: (2.0 * lbar / d).recip(),
                b: (((w * lbar) / d + 1.0)
                    * E.powf(2.0 * u_f64 * w * lbar / n_p_f64 + 7.0 * u_f64 / w)
                    * d
                    * u_f64
                    + d)
                    .floor() as usize,
            }
        }
    }
}

/// Round parameters
#[derive(Debug, Clone)]
pub struct Round {
    /// Proof counter
    v: u32,
    /// Proof 2nd counter
    t: usize,
    // Round candidate tuple
    s_list: Vec<Element>,
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
    fn h1(input: Vec<Vec<u8>>, n_p: usize) -> (Hash, usize) {
        let mut data = vec!["Telescope-H1".as_bytes().to_vec()];
        for i in input {
            data.push(i);
        }
        let digest = utils::combine_hashes::<DIGEST_SIZE>(data);
        (digest, utils::oracle_uniform(&digest, n_p))
    }

    /// Output a round from a proof counter and n_p
    /// Initilialises the hash with H1(t) and random value as oracle(H1(t), n_p)
    pub fn new(v: u32, t: usize, n_p: usize) -> Round {
        let mut data = vec![v.to_ne_bytes().to_vec()];
        data.push(t.to_ne_bytes().to_vec());
        let (h, h_usize) = Round::h1(data, n_p);
        Round {
            v,
            t,
            s_list: vec![],
            h,
            h_usize,
            n_p,
        }
    }

    /// Updates a round with an element of S_p
    /// Replaces the hash $h$ with $h' = H1(h, s)$ and the random value as oracle(h', n_p)
    pub fn update(r: &Round, s: Element) -> Round {
        let mut s_list = r.s_list.clone();
        s_list.push(s);
        let mut data = vec![r.h.clone().to_vec()];
        data.push(s.to_vec());
        let (h, h_usize) = Round::h1(data, r.n_p);
        Round {
            v: r.v,
            t: r.t,
            s_list,
            h,
            h_usize,
            n_p: r.n_p,
        }
    }
}

#[derive(Debug, Clone)]
/// Alba proof
pub struct Proof {
    /// Proof counter
    r: u32,
    /// Proof 2nd counter
    d: usize,
    /// Proof tuple
    items: Vec<Element>,
}

impl Proof {
    /// Oracle producing a uniformly random value in [1, n_p] used for prehashing S_p
    fn h0(setup: &Setup, v: u32, s: Element) -> usize {
        let mut data = vec!["Telescope-H0".as_bytes().to_vec()];
        data.push(v.to_ne_bytes().to_vec());
        data.push(s.to_vec());
        let digest = utils::combine_hashes::<DIGEST_SIZE>(data);
        utils::oracle_uniform(&digest, setup.n_p)
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
    fn h2(setup: &Setup, r: &Round) -> bool {
        let mut data = vec!["Telescope-H2".as_bytes().to_vec()];
        data.push(r.h.to_vec());
        let digest = utils::combine_hashes::<DIGEST_SIZE>(data);
        utils::oracle_bernouilli(&digest, setup.q)
    }

    /// Depth-first search which goes through all potential round candidates
    /// and returns first round candidate Round{t, x_1, ..., x_u)} such that:
    /// - for all i ∈ [0, u-1], H0(x_i+1) ∈ bins[H1(t, x_1, ..., x_i)]
    /// - H2(t, x_0, ..., x_u) = true
    fn dfs(
        setup: &Setup,
        bins: &Vec<Vec<Element>>,
        round: &Round,
        limit: usize,
    ) -> (usize, Option<Proof>) {
        if round.s_list.len() == setup.u {
            if Proof::h2(setup, round) {
                let r = round.v;
                let d = round.t;
                let items = round.s_list.clone();
                return (limit, Some(Proof { r, d, items }));
            } else {
                return (limit, None);
            }
        }

        bins[round.h_usize]
            .iter()
            .fold((limit, None), |(l, proof_opt), &s| {
                if proof_opt.is_some() || l == setup.b {
                    (l, proof_opt)
                } else {
                    Self::dfs(setup, bins, &Round::update(round, s), l + 1)
                }
            })
    }

    /// Indexed proving algorithm, returns an empty proof if no suitable
    /// candidate is found within the setup.b steps.
    fn prove_index(setup: &Setup, set: &[Element], v: u32) -> (usize, Option<Proof>) {
        let mut bins: Vec<Vec<Element>> = vec![vec![]; setup.n_p];
        for &s in set.iter() {
            bins[Proof::h0(setup, v, s)].push(s);
        }

        (0..setup.d).fold((0, None), |(limit, proof_opt), t| {
            if proof_opt.is_some() || limit == setup.b {
                (limit, proof_opt)
            } else {
                let round = Round::new(v, t, setup.n_p);
                Proof::dfs(setup, &bins, &round, limit + 1)
            }
        })
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Calls up to setup.r times the prove_index function and returns an empty
    /// proof if no suitable candidate is found.
    pub fn prove(setup: &Setup, set: &[Element]) -> Option<Self> {
        (0..setup.r).find_map(|v| Proof::prove_index(setup, set, v).1)
    }

    /// Alba's proving algorithm used for benchmarking, returning a proof as
    /// well as the number of  steps ran to find it.
    pub fn bench(setup: &Setup, set: &[Element]) -> (usize, u32, Option<Self>) {
        (0..setup.r).fold((0, setup.r, None), |(limit, r, proof_opt), v| {
            if proof_opt.is_some() {
                (limit, r, proof_opt)
            } else {
                let (l, opt) = Proof::prove_index(setup, set, v + 1);
                (limit + l, r, opt)
            }
        })
    }

    /// Alba's verification algorithm, follows proving algorithm by running the
    /// same depth-first search algorithm.
    pub fn verify(setup: &Setup, proof: Proof) -> bool {
        if proof.d >= setup.d || proof.r >= setup.r || proof.items.len() != setup.u {
            return false;
        }
        let r0 = Round::new(proof.r, proof.d, setup.n_p);
        let (b, round) = proof.items.iter().fold((true, r0), |(b, r), &s| {
            (
                b && r.h_usize == Proof::h0(setup, proof.r, s),
                Round::update(&r, s),
            )
        });
        b && Proof::h2(setup, &round)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};
    use crate::test_utils::gen_items;

    #[test]
    fn test_verify() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let nb_tests = 1_000;
        let set_size = 1_000;
        for _t in 0..nb_tests {
            let seed = rng.next_u32().to_ne_bytes().to_vec();
            let s_p = gen_items::<DATA_LENGTH>(seed, set_size);
            let params = Params {
                lambda_sec: 10,
                lambda_rel: 10,
                n_p: 80,
                n_f: 20,
            };
            let setup = Setup::new(&params);
            let proof = Proof::prove(&setup, &s_p).unwrap();
            assert!(Proof::verify(&setup, proof.clone()));
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
                items: vec![],
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
