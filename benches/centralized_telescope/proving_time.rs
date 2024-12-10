//! Benchmarking centralized_telescope

use criterion::{black_box, criterion_group, criterion_main, measurement::WallTime, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::{Duration, Instant};

#[path = "../benchmark_helpers.rs"]
pub mod benchmark_helpers;
use benchmark_helpers::{benchmarks, BenchParam};

pub mod utils;
use utils::{setup, NAME, PARAMS};

fn prove_duration(params: &BenchParam, truncate_size: u64, n: u64) -> Duration {
    let mut rng = ChaCha20Rng::from_entropy();
    let mut total_duration = Duration::ZERO;
    for _ in 0..n {
        // Setup
        let (mut dataset, telescope) = setup(&mut rng, params);
        // Truncate the dataset to give truncate_size elements to the prover
        dataset.truncate(truncate_size as usize);
        // Bench
        let start = Instant::now();
        black_box(telescope.prove(&dataset));
        total_duration = total_duration.saturating_add(start.elapsed());
    }
    total_duration
}

/// Bench the duration of both the proving and verifiying algorithm of Alba centralized
fn proving_benches(c: &mut Criterion) {
    benchmarks::<Instant, Duration, WallTime>(
        c,
        PARAMS,
        format!("{} - {}", NAME, "Time"),
        "Prove",
        &prove_duration,
    );
}

mod criterion_group {
    #![allow(missing_docs)]
    use super::{criterion_group, proving_benches, Criterion, Duration};

    // Benchmarking proving and verifying time
    criterion_group!(name = centralized_proving_time;
                     config = Criterion::default().measurement_time(Duration::from_secs(30));
                     targets = proving_benches
    );
}

criterion_main!(criterion_group::centralized_proving_time,);
