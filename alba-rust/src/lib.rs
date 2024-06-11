use blake2::digest::Update;
use blake2::digest::VariableOutput;
use blake2::Blake2bVar;
use more_asserts::assert_le;
use num_bigint::BigUint;
use num_traits::{One, ToPrimitive};
use std::f64::consts::LOG2_E;
use std::mem::size_of_val;
use std::ops::{Div, Shl};

#[derive(Debug, Clone)]
struct PreHash {
    h: Vec<u8>,
    s: [u8; 32],
}

#[derive(Debug, Clone)]
struct Round {
    t: usize,
    h: Vec<u8>,
    s_list: Vec<[u8; 32]>,
    n_pi: usize,
}

struct Cycle {
    k: usize,
    round: Round,
}

#[derive(Debug, Clone)]
struct Proof {
    d: usize,
    items: Vec<[u8; 32]>,
}

#[derive(Clone)]
pub struct Params {
    pub lambda_sec: f64,
    pub lambda_rel: f64,
    pub n_p: usize,
    pub n_f: usize,
}

pub struct AlbaSetup {
    pub n_p: usize,
    pub u: usize,
    pub d: usize,
    pub q: f64,
}

impl AlbaSetup {
    pub fn gen_setup(params: Params) -> Self {
        let temp = 3.0f64.log2() + params.lambda_rel;
        let uden = params.lambda_sec + temp.log2() + 1.0 - LOG2_E.log2();
        let udiv = (params.n_p.div(params.n_f) as f64).log2();
        let u = uden.div(udiv).ceil() as usize;
        let d = ((16 * u) as f64 * temp.div(LOG2_E)).ceil() as usize;
        let q = 2.0f64 * temp.div(d as f64 * LOG2_E);

        AlbaSetup {
            n_p: params.n_p,
            u,
            d,
            q,
        }
    }
}

fn prove(s_p: Vec<[u8; 32]>, alba_setup: AlbaSetup) -> Proof {
    let mut pre_hash: Vec<PreHash> = Vec::new();

    // Generate pre_hash list
    for s in s_p {
        let h = hash_bytes(s.as_slice()).to_vec();
        pre_hash.push(PreHash { h, s });
    }

    let mut round_stack: Vec<Round> = Vec::new();

    for hs in &pre_hash {
        for t in 1..&alba_setup.d + 1 {
            let mut new_h = hash_bytes(&[t as u8]).to_vec();
            new_h.extend(&hs.h.clone());
            let new_round = Round {
                t,
                h: new_h,
                s_list: [hs.s].to_vec(),
                n_pi: alba_setup.n_p,
            };
            round_stack.push(new_round);
        }
    }

    while !round_stack.is_empty() {
        let round = round_stack.first().unwrap();

        let result = go(&alba_setup.u - 2, pre_hash.clone(), round.clone());
        if let Some(value) = result {
            return value;
        }
    }
    Proof {
        d: 0,
        items: vec![],
    }
}

fn go(k: usize, pre_hash: Vec<PreHash>, round_entry: Round) -> Option<Proof> {
    let mut cycle_stack: Vec<Cycle> = Vec::new();
    cycle_stack.push(Cycle {
        k,
        round: round_entry,
    });

    while let Some(cycle) = cycle_stack.pop() {
        if k == 0 || pre_hash.is_empty() {
            continue;
        }

        let t = cycle.round.t; //t

        for ph in &pre_hash {
            let mut new_hash = cycle.round.h.clone();
            new_hash.append(&mut ph.h.clone());

            let mut new_s_list = cycle.round.s_list.clone();
            new_s_list.push(ph.s);

            let mod_value: BigUint = BigUint::one() << 64;
            let n_pj_bigint: BigUint = BigUint::from_bytes_be(&new_hash) % mod_value;
            let n_pj_new = n_pj_bigint.to_usize().unwrap();

            if n_pj_new == 0 {
                return Some(Proof {
                    d: n_pj_new,
                    items: new_s_list,
                });
            };

            let new_cycle = Cycle {
                k: &k - 1,
                round: Round {
                    t,
                    h: new_hash,
                    s_list: new_s_list,
                    n_pi: n_pj_new,
                },
            };
            cycle_stack.push(new_cycle);
        }
    }
    None
}

// Helper functions
pub fn hash_bytes(data: &[u8]) -> [u8; 32] {
    let mut hasher = Blake2bVar::new(32).unwrap();
    hasher.update(data);
    let mut buf = [0u8; 32];
    hasher.finalize_variable(&mut buf).unwrap();
    buf
}
pub fn gen_items(size: u8) -> Vec<[u8; 32]> {
    let mut s_p = Vec::with_capacity(size as usize);
    for b in 0..size {
        let b_arr = [b];
        let item = hash_bytes(&b_arr);
        s_p.push(item);
    }
    s_p
}
pub fn mod_non_power_of_2(bytes: &[u8; 32], n_p: usize) -> BigUint {
    let i = BigUint::from_bytes_be(bytes);
    let k = size_of_val(&n_p.clone());
    let d = BigUint::from(((1).shl(k - 1)) as usize) / n_p;

    println!("{:?} {:?} {:?}", i, d, n_p);
    assert_le!(i, (d.clone() * n_p));

    i % BigUint::from(n_p)
}
pub fn oracle(h: &[u8; 32], n_p: usize) -> BigUint {
    if n_p.is_power_of_two() {
        let mut little = [0u8; 8];
        little.copy_from_slice(&h[0..8]);
        let cnt = BigUint::from_bytes_le(&little);
        cnt & BigUint::from(n_p - 1)
    } else {
        mod_non_power_of_2(h, n_p)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_prove() {
        let params = Params {
            lambda_sec: 128.0,
            lambda_rel: 128.0,
            n_p: 100,
            n_f: 50,
        };
        let alba_setup = AlbaSetup::gen_setup(params.clone());
        let s_p = gen_items(10);

        let proof = prove(s_p, alba_setup);
        println!("{:?}", proof)
    }

    #[test]
    fn try_functions() {
        // Set parameters
        let params = Params {
            lambda_sec: 128.0,
            lambda_rel: 128.0,
            n_p: 100,
            n_f: 50,
        };

        // Generate u, d, q
        let alba_setup = AlbaSetup::gen_setup(params.clone());
        println!(
            "ALBA Setup for lambda_sec: {:?}, lambda_rel: {:?}, n_p: {:?}, n_f: {:?}",
            params.lambda_sec, params.lambda_rel, params.n_p, params.n_f
        );
        println!("u: {:?}", alba_setup.u);
        println!("d: {:?}", alba_setup.d);
        println!("q: {:?}", alba_setup.q);

        // Generate and print the items.
        let s_p = gen_items(10);
        println!("\n10 items generated:");
        for s in &s_p {
            println!("{:?}", hex::encode(s));
        }

        // Test oracle - Fails with the current input.
        // let item = s_p[0].clone();
        // println!("{:?}", oracle(&item, params.n_p));
        // println!("{:?}", mod_non_power_of_2(&item, params.n_p));
    }
}
