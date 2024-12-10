//! Benchmarking centralized_telescope's proof size
use alba::centralized_telescope::{init, params::Params};

#[path = "../benchmark_helpers.rs"]
pub mod benchmark_helpers;
use benchmark_helpers::BenchParam;

pub mod utils;
use utils::PARAMS;

fn proof_bench(params: &[BenchParam]) {
    println!("Centralized telescope -- proof size");
    println!(
        "{0: <10} | {1: <10} | {2: <10} | {3: <10} | {4: <10}",
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
            set_size: param.set_size_per,
            lower_bound: param.lower_bound_per,
        };
        let setup = init::make_setup(&params);
        println!(
            "{0: <10} | {1: <10} | {2: <10} | {3: <10} | {4: <10}",
            param.lambda_sec,
            param.lambda_rel,
            param.set_size_per,
            param.lower_bound_per,
            setup.proof_size
        );
    }
}

fn main() {
    proof_bench(PARAMS);
}
