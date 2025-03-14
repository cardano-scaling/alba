//! Benchmarking centralized_telescope shared helper functions

#![allow(unused)]

use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;

use alba::centralized_telescope::{params::Params, Telescope};
use alba::utils::types::Element;

#[path = "../../tests/common/mod.rs"]
mod test_utils;

#[path = "../common/mod.rs"]
pub mod common;
use common::{criterion_helpers::centralized::BenchParam, test_vectors::DATA_LENGTH};

/// Global variables
pub const NAME: &str = "Centralized";

/// Function generating a random set of elements to bench and calling Alba's centralized setup
pub fn setup(rng: &mut ChaCha20Rng, params: &BenchParam) -> (Vec<Element>, Telescope) {
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset = test_utils::gen_items::<DATA_LENGTH>(&seed, params.total_num_elements)
        .into_iter()
        .map(|d| d.to_vec())
        .collect();
    let s_p: Vec<Element> = Element::element_list_from_bytes_with_index(dataset);

    let telescope = Telescope::create(
        params.lambda_sec,
        params.lambda_rel,
        params.set_size,
        params.lower_bound,
    );
    (s_p, telescope)
}
