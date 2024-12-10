//! Benchmarking centralized_telescope

use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;

use alba::centralized_telescope::{params::Params, CentralizedTelescope};

use crate::benchmark_helpers;

#[path = "../../src/utils/test_utils.rs"]
mod test_utils;

/// Benchmark parameters to use
pub const PARAMS: &[benchmark_helpers::BenchParam; 2] =
    &[benchmark_helpers::LOW_PARAM, benchmark_helpers::MID_PARAM];

/// Global variables
pub const NAME: &str = "Centralized";

/// Function generating a random set of elements to bench and calling Alba's centralized setup
pub fn setup(
    rng: &mut ChaCha20Rng,
    params: &benchmark_helpers::BenchParam,
) -> (Vec<[u8; 32]>, CentralizedTelescope) {
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
    let telescope = CentralizedTelescope::create(&params);
    (dataset, telescope)
}
