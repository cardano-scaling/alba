//! Benchmarking centralized_telescope shared helper functions

#![allow(unused)]

use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;

use alba::centralized_telescope::{params::Params, CentralizedTelescope};

#[path = "../../tests/common/mod.rs"]
mod test_utils;

#[path = "../common/mod.rs"]
pub mod common;
use common::criterion_helpers::centralized::BenchParam;

/// Global variables
pub const NAME: &str = "Centralized";

/// Function generating a random set of elements to bench and calling Alba's centralized setup
pub fn setup(rng: &mut ChaCha20Rng, params: &BenchParam) -> (Vec<[u8; 48]>, CentralizedTelescope) {
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset: Vec<[u8; 48]> = test_utils::gen_items(&seed, params.total_num_elements);
    let params = Params {
        soundness_param: params.lambda_sec,
        completeness_param: params.lambda_rel,
        set_size: params.set_size,
        lower_bound: params.lower_bound,
    };
    let telescope = CentralizedTelescope::create(&params);
    (dataset, telescope)
}
