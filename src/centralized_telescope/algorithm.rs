//! ALBA's bounded DFS scheme prove and verification functions

use super::proof::Proof;
use super::round::Round;
use super::setup::Setup;
use crate::utils::types::Element;

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
