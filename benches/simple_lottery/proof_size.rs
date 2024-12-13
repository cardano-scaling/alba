//! Benchmarking the proof size of the Simple Lottery scheme

use alba::simple_lottery::params::Params;

mod utils;
use utils::common::{
    criterion_helpers::centralized::BenchParam, test_vectors::centralized::ALL_TESTS,
};

/// Function benchmarking the proof size
fn proof_bench(params: &[BenchParam]) {
    println!("Simple Lottery -- proof size");
    println!(
        "{0: <23} | {1: <26} | {2: <14} | {3: <17} | {4: <14} | {5: <23}",
        "soundness_param (λ_sec)",
        "completeness_param (λ_rel)",
        "set_size (n_p)",
        "lower_bound (n_f)",
        "proof_size (u)",
        "lottery_probability (q)"
    );
    for param in params {
        let setup = Params::new(
            param.lambda_sec,
            param.lambda_rel,
            param.set_size,
            param.lower_bound,
        );
        println!(
            "{0: <23} | {1: <26} | {2: <14} | {3: <17} | {4: <14} | {5: <23}",
            param.lambda_sec,
            param.lambda_rel,
            param.set_size,
            param.lower_bound,
            setup.proof_size,
            setup.lottery_probability
        );
    }
}

fn main() {
    proof_bench(ALL_TESTS);
}
