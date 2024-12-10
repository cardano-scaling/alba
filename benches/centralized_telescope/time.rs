//! Benchmarking centralized_telescope

use criterion::{black_box, criterion_group, criterion_main, measurement::WallTime, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::{Duration, Instant};

use alba::centralized_telescope::algorithm;

#[path = "../benchmark_helpers.rs"]
pub mod benchmark_helpers;
use benchmark_helpers::{benchmarks, BenchParam};

pub mod utils;
use crate::utils::{centralized_setup, PARAMS};

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
        format!("{} - {}", utils::NAME, "Time"),
        "Prove",
        &prove_duration,
    );

    benchmarks::<Instant, Duration, WallTime>(
        c,
        PARAMS,
        format!("{} - {}", utils::NAME, "Time"),
        "Verify",
        &verify_duration,
    );
}

mod criterion_group {
    #![allow(missing_docs)]
    use super::{criterion_group, time_benches, Criterion, Duration};

    // Benchmarking proving and verifying time
    criterion_group!(name = centralized_time;
                     config = Criterion::default().measurement_time(Duration::from_secs(30));
                     targets = time_benches
    );
}

criterion_main!(criterion_group::centralized_time,);
