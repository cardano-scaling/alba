//! Rust implementation of ALBA's Telescope weighted scheme using Blake2b as hash
//! function and Sortition as lottery.

use crate::utils;
extern crate core;
use std::f64::consts::E;
use vrf_dalek::vrf::{PublicKey, SecretKey, VrfProof};

const DATA_LENGTH: usize = 32;
const DIGEST_SIZE: usize = 32;

type Data = [u8; DATA_LENGTH];

#[derive(Clone, Copy, Debug)]
pub struct IndexedData {
    // Data, e.g. hash of a vote
    pub data: Data,
    // Lottery index of the data, must be strictly lower than the VotingWeight
    pub index: usize,
}

impl IndexedData {
    pub fn to_hash(self) -> Data {
        let mut data = Vec::new();
        data.push(self.data.to_vec());
        data.push(self.index.to_be_bytes().to_vec());
        return utils::combine_hashes::<DIGEST_SIZE>(data);
    }
}

#[derive(Clone, Copy, Debug)]
pub struct VerifiableData {
    // Data, e.g. hash of a vote
    pub indexed_data: IndexedData,
    pub pk: PublicKey,
    pub pi: VrfProof,
    pub stake: usize,
}

type Hash = [u8; DIGEST_SIZE];

type VotingWeight = usize;

/// Compute a vote's weight based on binomial distribution of the
/// possible weights.
//
// This function is directly stolen from Algorand's codebase
// (https://github.com/algorand/sortition/blob/main/sortition.cpp)
// It computes the actual voting weight through a dichotomial search in the
// binomial distribution $B(n,p)$ where $n$ is the voter's stake (in ADA) and
// $p$ is the ratio of target committee size over the total stake.  The value
// searched for is the given `ratio` which is compared to quantiles of the
// distribution.
// NOTE: While this is significantly faster, by several orders of magnitude,
// to the lottery drawing (see `isLotteryWinner`) process, the soundness of
// this process has not been researched so it should be considered with some
// caution.
pub fn sortition_binomial_cdf_walk(n: usize, p: f64, ratio: f64, weight: usize) -> VotingWeight {
    let mut bound = 0f64;
    for i in 0..weight {
        bound += utils::bin_pmf(n, i, p);

        if ratio <= bound {
            return i;
        }
    }
    return weight;
}

pub fn prove_sortition(
    public_key: &PublicKey,
    secret_key: &SecretKey,
    msg: &[u8],
    voter_stake: usize,
    total_stake: usize,
    expected_size: usize,
) -> (VotingWeight, VrfProof) {
    let proof = VrfProof::generate(public_key, secret_key, &msg);
    // We hash to 64 bits as the largest float is f64
    let hash = utils::hash_bytes::<8>(&proof.verify(public_key, &msg).unwrap());
    let i = sortition_binomial_cdf_walk(
        voter_stake,
        expected_size as f64 / total_stake as f64,
        u64::from_be_bytes(hash) as f64 / (u64::MAX as f64),
        voter_stake,
    );
    return (i, proof);
}

pub fn verify_sortition(
    public_key: &PublicKey,
    proof: VrfProof,
    msg: &[u8],
    voter_stake: usize,
    total_stake: usize,
    expected_size: usize,
) -> VotingWeight {
    let hash = match proof.verify(public_key, &msg) {
        Ok(h) => utils::hash_bytes::<8>(&h),
        Err(_) => return 0,
    };
    return sortition_binomial_cdf_walk(
        voter_stake,
        expected_size as f64 / total_stake as f64,
        u64::from_be_bytes(hash) as f64 / (u64::MAX as f64),
        voter_stake,
    );
}

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
    s_list: Vec<VerifiableData>,
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
    pub fn update(r: &Round, vd: VerifiableData) -> Round {
        let indexed_data = vd.indexed_data.clone();
        let mut s_list = r.s_list.clone();
        s_list.push(vd);
        let mut data = Vec::new();
        data.push(r.h.clone().to_vec());
        data.push(indexed_data.data.to_vec());
        data.push(indexed_data.index.to_be_bytes().to_vec());
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
    items: Vec<VerifiableData>,
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

    /// Sortition-based lottery proving scheme (Section 5 of ALBA paper) scheme using Binomial distribution
    /// with probability p = mu / np where mu (resp. np) is the expected (resp.
    /// total weight) number of participants
    pub fn prove_lottery(
        np: usize,
        mu: usize,
        s: Data,
        sk: &SecretKey,
        pk: &PublicKey,
        stake: usize,
    ) -> Vec<VerifiableData> {
        let (i, pi) = prove_sortition(pk, sk, &s, stake, np, mu);
        let mut winners = Vec::new();
        for j in 0..i {
            let indexed_data = IndexedData { data: s, index: j };
            winners.push(VerifiableData {
                indexed_data,
                pi: pi.clone(),
                pk: *pk,
                stake,
            });
        }
        return winners;
    }

    /// Sortition-based lottery verification scheme (Section 5 of ALBA paper) scheme using Binomial distribution
    /// with probability p = mu / np where mu (resp. np) is the expected (resp.
    /// total weight) number of participants
    pub fn verify_lottery(np_lottery: usize, mu: usize, vd: &VerifiableData) -> bool {
        let w = verify_sortition(
            &vd.pk,
            vd.pi.clone(),
            &vd.indexed_data.data,
            vd.stake,
            np_lottery,
            mu,
        );
        return vd.indexed_data.index < w;
    }

    /// Oracle producing a uniformly random value in [1, n_p] used for prehashing S_p
    fn h0(setup: &Setup, v: usize, is: IndexedData) -> usize {
        let mut data = Vec::new();
        data.push(v.to_ne_bytes().to_vec());
        data.push(is.data.to_vec());
        data.push(is.index.to_be_bytes().to_vec());
        let digest = utils::combine_hashes::<DIGEST_SIZE>(data);
        return utils::oracle(&digest, setup.n_p);
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0 otherwise
    fn h2(setup: &Setup, r: &Round) -> bool {
        let mut data = Vec::new();
        data.push(r.v.to_ne_bytes().to_vec());
        data.push(r.t.to_ne_bytes().to_vec());
        for vd in &r.s_list {
            data.push(vd.indexed_data.data.to_vec());
            data.push(vd.indexed_data.index.to_be_bytes().to_vec());
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
        bins: &Vec<Vec<VerifiableData>>,
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
        let result = bins[round.h_usize].iter().find_map(|&vd| {
            if *nb_steps == setup.b {
                return None;
            }
            *nb_steps += 1;
            Self::dfs(setup, bins, &Round::update(round, vd), nb_steps)
        });
        return result;
    }

    /// Indexed proving algorithm, returns an empty proof if no suitable
    /// candidate is found within the setup.b steps.
    fn prove_index(setup: &Setup, set: &Vec<VerifiableData>, v: usize) -> (usize, Option<Proof>) {
        let mut bins: Vec<Vec<VerifiableData>> = Vec::new();
        for _ in 1..(setup.n_p + 1) {
            bins.push(Vec::new());
        }
        for &vd in set.iter() {
            bins[Proof::h0(setup, v, vd.indexed_data)].push(vd);
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
    pub fn prove(setup: &Setup, set: &Vec<VerifiableData>) -> Self {
        // Lottery must be done by each participant on its own, we have here
        // the aggregator running it another time for robustness.
        let winner_set = set
            .iter()
            .filter_map(|&vd| Proof::verify_lottery(setup.n_p_lottery, setup.mu, &vd).then(|| vd))
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
    pub fn bench(setup: &Setup, set: &Vec<VerifiableData>) -> (usize, Self) {
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
        let (b, round) = proof.items.iter().fold((true, r0), |(b, r), &vd| {
            (
                b && r.h_usize == Proof::h0(setup, proof.r, vd.indexed_data)
                    && Proof::verify_lottery(setup.n_p_lottery, setup.mu, &vd),
                Round::update(&r, vd),
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
        let (total_weight, set_size) = (100_000, 1_000);
        let lambda = 80;
        let np = 60;
        let nf = 100 - np;
        for _t in 0..nb_tests {
            let seed = rng.next_u32().to_ne_bytes().to_vec();

            let s_p = utils::gen_weighted_items::<DATA_LENGTH>(seed, total_weight, set_size);
            let params = Params::new(
                lambda,
                lambda,
                total_weight * np / 100,
                total_weight * nf / 100,
            );
            let setup = Setup::new(&params);
            println!("\n{:?}", setup);

            let mut verifiable_set = Vec::new();
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            for spi in s_p {
                let ski = SecretKey::generate(&mut rng);
                let pki = PublicKey::from(&ski);
                let (data, stake) = spi;
                let votes =
                    Proof::prove_lottery(setup.n_p_lottery, setup.mu, data, &ski, &pki, stake);
                for v in votes {
                    verifiable_set.push(v);
                }
            }
            let proof = Proof::prove(&setup, &verifiable_set);
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
            penultimate_item.indexed_data.data[0] =
                penultimate_item.indexed_data.data[0].wrapping_add(42u8);
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