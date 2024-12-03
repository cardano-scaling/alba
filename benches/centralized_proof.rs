//! Benchmarking centralized_telescope's proof size
use alba::centralized_telescope::{init, params::Params};

pub mod benchmark_helpers;
use benchmark_helpers::BenchParam;

use blake2 as _;
use criterion as _;
use rand as _;
use rand_chacha as _;
use rand_core as _;

// Benchmark parameters to use
const PARAMS: &[BenchParam; 3] = &[
    benchmark_helpers::LOW_PARAM,
    benchmark_helpers::MID_PARAM,
    benchmark_helpers::HIGH_PARAM,
];

fn proof_bench() {
    println!("Centralized telescope -- proof size");
    println!(
        "{0: <10} | {1: <10} | {2: <10} | {3: <10} | {4: <10}",
        "soundness_param (λ_sec)",
        "completeness_param (λ_rel)",
        "set_size (n_p)",
        "lower_bound (n_f)",
        "proof_size (u)"
    );
    for param in PARAMS {
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
    proof_bench();
}
