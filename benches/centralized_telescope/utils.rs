//! Benchmarking centralized_telescope shared helper functions

#![allow(unused)]

use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;

use alba::centralized_telescope::{params::Params, Telescope};

#[path = "../../tests/common/mod.rs"]
mod test_utils;

#[path = "../common/mod.rs"]
pub mod common;
use common::{
    criterion_helpers::centralized::BenchParam,
    test_vectors::{Data, DATA_LENGTH},
};

use blake2::Blake2bVar;

/// Global variables
pub const NAME: &str = "Centralized";

/// Function generating a random set of elements to bench and calling Alba's centralized setup
pub fn setup(rng: &mut ChaCha20Rng, params: &BenchParam) -> (Vec<Data>, Telescope) {
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset =
        test_utils::gen_items::<Blake2bVar, DATA_LENGTH>(&seed, params.total_num_elements);
    let telescope = Telescope::create(
        params.lambda_sec,
        params.lambda_rel,
        params.set_size,
        params.lower_bound,
    );
    (dataset, telescope)
}
