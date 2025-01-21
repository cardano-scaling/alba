//! Benchmarking the proof size of the Simple Lottery scheme

use alba::simple_lottery::params::Params;

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
    lottery_proba: &str,
) {
    println!(
        "{0: <23} | {1: <26} | {2: <14} | {3: <17} | {4: <14} | {5: <23}",
        soundness_param, completeness_param, set_size, lower_bound, proof_size, lottery_proba
    );
}

/// Function benchmarking the proof size
fn proof_bench(params: &[BenchParam]) {
    println!("Simple Lottery -- proof size");
    print_bench(
        "soundness_param (λ_sec)",
        "completeness_param (λ_rel)",
        "set_size (n_p)",
        "lower_bound (n_f)",
        "proof_size (u)",
        "lottery_probability (q)",
    );
    for param in params {
        let setup = Params::new(
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
            setup.proof_size.to_string().as_str(),
            setup.lottery_probability.to_string().as_str(),
        );
    }
}

fn main() {
    proof_bench(ALL_TESTS);
}
