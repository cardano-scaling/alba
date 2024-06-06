use blake2::digest::Update;
use blake2::digest::VariableOutput;
use blake2::Blake2bVar;
use more_asserts::assert_le;
use num_bigint::BigUint;
use std::f64::consts::LOG2_E;
use std::mem::size_of_val;
use std::ops::{Div, Shl};

/// Type of the digest output.
pub type DIGEST = [u8; 32];

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

// Helper functions
pub fn hash_bytes(data: &[u8]) -> DIGEST {
    let mut hasher = Blake2bVar::new(32).unwrap();
    hasher.update(data);
    let mut buf = [0u8; 32];
    hasher.finalize_variable(&mut buf).unwrap();
    buf
}
pub fn gen_items(size: u8) -> Vec<DIGEST> {
    let mut s_p = Vec::with_capacity(size as usize);
    for b in 0..size {
        let b_arr = [b];
        let item = hash_bytes(&b_arr);
        s_p.push(item);
    }
    s_p
}
pub fn mod_non_power_of_2(bytes: &DIGEST, n_p: usize) -> BigUint {
    let i = BigUint::from_bytes_be(bytes);
    let k = size_of_val(&n_p.clone());
    let d = BigUint::from(((1).shl(k - 1)) as usize) / n_p;

    println!("{:?} {:?} {:?}", i, d, n_p);
    assert_le!(i, (d.clone() * n_p));

    i % BigUint::from(n_p)
}
pub fn oracle(h: &DIGEST, n_p: usize) -> BigUint {
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
