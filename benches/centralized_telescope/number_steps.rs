//! Benchmarking centralized_telescope

#![allow(unused_crate_dependencies)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::Duration;

pub mod utils;
use utils::{setup, NAME, PARAMS};

#[path = "../benchmark_helpers.rs"]
pub mod benchmark_helpers;
use benchmark_helpers::{benchmarks, BenchParam, Steps};

/// Bench the number of steps, i.e. DFS calls, of Alba centralized prover
fn step_benches(c: &mut Criterion<Steps>) {
    #[allow(clippy::unit_arg)]
    fn prove_steps(param: &BenchParam, truncate_size: u64, n: u64) -> u64 {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_steps = 0u64;
        for _ in 0..n {
            // Setup
            let (mut dataset, telescope) = setup(&mut rng, param);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size as usize);
            // Bench
            black_box({
                let (steps, _, _) = telescope.bench_prove(&dataset);
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

mod criterion_group {
    #![allow(missing_docs)]
    use super::{criterion_group, step_benches, Criterion, Duration, Steps};

    // Benchmarking the number of DFS calls per proof
    criterion_group!(name = centralized_step;
        config = Criterion::default().with_measurement(Steps).measurement_time(Duration::from_secs(30));
        targets = step_benches
    );
}

criterion_main!(criterion_group::centralized_step);
