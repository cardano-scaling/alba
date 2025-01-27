//! Benchmarking simple_lottery shared helper functions

#![allow(unused)]

use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;

use alba::simple_lottery::{params::Params, Lottery};

#[path = "../../tests/common/mod.rs"]
mod test_utils;

#[path = "../common/mod.rs"]
pub mod common;
use common::criterion_helpers::centralized::BenchParam;

/// Global variables
pub const NAME: &str = "Lottery";

/// Function generating a random set of elements to bench and calling Simple Lottery setup
pub fn setup(rng: &mut ChaCha20Rng, params: &BenchParam) -> (Vec<[u8; 48]>, Lottery) {
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset: Vec<[u8; 48]> = test_utils::gen_items(&seed, params.total_num_elements);
    let telescope = Lottery::create(
        params.lambda_sec,
        params.lambda_rel,
        params.set_size,
        params.lower_bound,
    );
    (dataset, telescope)
}
