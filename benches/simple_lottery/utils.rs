//! Benchmarking simple_lottery shared helper functions

#![allow(unused)]

use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;

use alba::simple_lottery::{params::Params, SimpleLottery};

#[path = "../../tests/common/mod.rs"]
mod test_utils;

#[path = "../common/mod.rs"]
pub mod common;
use common::criterion_helpers::centralized::BenchParam;

/// Global variables
pub const NAME: &str = "Lottery";

/// Function generating a random set of elements to bench and calling Alba's centralized setup
pub fn setup(rng: &mut ChaCha20Rng, params: &BenchParam) -> (Vec<[u8; 32]>, SimpleLottery) {
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset: Vec<[u8; 32]> = test_utils::gen_items(&seed, params.set_card);
    let params = Params {
        soundness_param: params.lambda_sec,
        completeness_param: params.lambda_rel,
        set_size: params
            .set_size_per
            .saturating_mul(params.set_card)
            .div_ceil(100),
        lower_bound: params
            .lower_bound_per
            .saturating_mul(params.set_card)
            .div_ceil(100),
    };
    let telescope = SimpleLottery::create(&params);
    (dataset, telescope)
}
