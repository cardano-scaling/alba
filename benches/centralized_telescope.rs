//! Benchmarking centralized_telescope

use criterion::{black_box, criterion_group, criterion_main, measurement::WallTime, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use std::time::{Duration, Instant};

use alba::centralized_telescope::{algorithm, init, params::Params, setup::Setup};

pub mod benchmark_helpers;
use benchmark_helpers::{benchmarks, BenchParam, Steps};
#[path = "../src/utils/test_utils.rs"]
mod test_utils;

// Benchmark parameters to use
const PARAMS: &[BenchParam; 2] = &[benchmark_helpers::LOW_PARAM, benchmark_helpers::MID_PARAM];

// Global variables
const NAME: &str = "Centralized";

/// Function generating a random set of elements to bench and calling Alba's centralized setup
pub fn centralized_setup(rng: &mut ChaCha20Rng, params: &BenchParam) -> (Vec<[u8; 32]>, Setup) {
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
    (dataset, init::make_setup(&params))
}

/// Bench the duration of both the proving and verifiying algorithm of Alba centralized
fn time_benches(c: &mut Criterion) {
    #[allow(clippy::unit_arg)]
    fn prove_duration(params: &BenchParam, truncate_size: u64, n: u64) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, params);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size as usize);
            // Bench
            let start = Instant::now();
            black_box(algorithm::prove(&bench_setup, &dataset));
            total_duration = total_duration.saturating_add(start.elapsed());
        }
        total_duration
    }

    #[allow(clippy::unit_arg)]
    fn verify_duration(params: &BenchParam, truncate_size: u64, n: u64) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, params);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size as usize);
            // Prove
            let proof_opt = algorithm::prove(&bench_setup, &dataset);
            if let Some(proof) = proof_opt {
                // Bench
                let start = Instant::now();
                black_box(algorithm::verify(&bench_setup, &proof));
                total_duration = total_duration.saturating_add(start.elapsed());
            }
        }
        total_duration
    }

    benchmarks::<Instant, Duration, WallTime>(
        c,
        PARAMS,
        format!("{} - {}", NAME, "Time"),
        "Prove",
        &prove_duration,
    );

    benchmarks::<Instant, Duration, WallTime>(
        c,
        PARAMS,
        format!("{} - {}", NAME, "Time"),
        "Verify",
        &verify_duration,
    );
}

/// Bench the number of steps, i.e. DFS calls, of Alba centralized prover
fn step_benches(c: &mut Criterion<Steps>) {
    #[allow(clippy::unit_arg)]
    fn prove_steps(param: &BenchParam, truncate_size: u64, n: u64) -> u64 {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_steps = 0u64;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, param);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size as usize);
            // Bench
            black_box({
                let (steps, _, _) = algorithm::bench(&bench_setup, &dataset);
                total_steps = total_steps.saturating_add(steps);
            });
        }
        total_steps
    }

    benchmarks::<u64, u64, Steps>(
        c,
        PARAMS,
        format!("{} - {}", NAME, "Steps"),
        "Prove",
        &prove_steps,
    );
}

mod criterion_groups {
    #![allow(missing_docs)]
    use super::{criterion_group, step_benches, time_benches, Criterion, Duration, Steps};

    // Benchmarking proving and verifying time
    criterion_group!(name = centralized_time;
                     config = Criterion::default().measurement_time(Duration::from_secs(30));
                     targets = time_benches
    );

    // Benchmarking the number of DFS calls per proof
    criterion_group!(name = centralized_step;
        config = Criterion::default().with_measurement(Steps).measurement_time(Duration::from_secs(30));
        targets = step_benches
    );
}

criterion_main!(
    criterion_groups::centralized_time,
    criterion_groups::centralized_step,
);
