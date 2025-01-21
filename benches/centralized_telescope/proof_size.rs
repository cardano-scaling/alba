//! Benchmarking the proof size of the Centralized Telescope scheme

use alba::centralized_telescope::params::Params;

mod utils;
use utils::common::{
    criterion_helpers::centralized::BenchParam, test_vectors::centralized::ALL_TESTS,
};

fn print_bench(
    soundness_param: &str,
    completeness_param: &str,
    set_size: &str,
    lower_bound: &str,
    proof_size: &str,
) {
    println!(
        "{0: <23} | {1: <26} | {2: <14} | {3: <17} | {4: <14}",
        soundness_param, completeness_param, set_size, lower_bound, proof_size
    );
}

/// Function benchmarking the proof size
fn proof_bench(params: &[BenchParam]) {
    println!("Centralized telescope -- proof size");
    print_bench(
        "soundness_param (λ_sec)",
        "completeness_param (λ_rel)",
        "set_size (n_p)",
        "lower_bound (n_f)",
        "proof_size (u)",
    );
    for param in params {
        let parameters = Params::new(
            param.lambda_sec,
            param.lambda_rel,
            param.set_size,
            param.lower_bound,
        );
        print_bench(
            param.lambda_sec.to_string().as_str(),
            param.lambda_rel.to_string().as_str(),
            param.set_size.to_string().as_str(),
            param.lower_bound.to_string().as_str(),
            parameters.proof_size.to_string().as_str(),
        );
    }
}

fn main() {
    proof_bench(ALL_TESTS);
}
