//! Benchmarking the proof size of the Centralized Telescope scheme

use alba::centralized_telescope::params::Params;

mod utils;
use utils::common::{
    criterion_helpers::centralized::BenchParam, test_vectors::centralized::ALL_TESTS,
};

/// Function benchmarking the proof size
fn proof_bench(params: &[BenchParam]) {
    println!("Centralized telescope -- proof size");
    println!(
        "{0: <23} | {1: <26} | {2: <14} | {3: <17} | {4: <14}",
        "soundness_param (λ_sec)",
        "completeness_param (λ_rel)",
        "set_size (n_p)",
        "lower_bound (n_f)",
        "proof_size (u)"
    );
    for param in params {
        let parameters = Params::new(
            param.lambda_sec,
            param.lambda_rel,
            param.set_size,
            param.lower_bound,
        );
        println!(
            "{0: <23} | {1: <26} | {2: <14} | {3: <17} | {4: <14}",
            param.lambda_sec,
            param.lambda_rel,
            param.set_size,
            param.lower_bound,
            parameters.proof_size
        );
    }
}

fn main() {
    proof_bench(ALL_TESTS);
}
