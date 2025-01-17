//! Benchmarking the proof size of the Centralized Telescope scheme

use alba::centralized_telescope::{init, params::Params};

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
        let params = Params {
            soundness_param: param.lambda_sec,
            completeness_param: param.lambda_rel,
            set_size: param.set_size,
            lower_bound: param.lower_bound,
        };
        let setup = init::make_setup(&params);
        println!(
            "{0: <23} | {1: <26} | {2: <14} | {3: <17} | {4: <14}",
            param.lambda_sec, param.lambda_rel, param.set_size, param.lower_bound, setup.proof_size
        );
    }
}

fn main() {
    proof_bench(ALL_TESTS);
}
