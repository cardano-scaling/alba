//! Benchmarking centralized_telescope

#![allow(unused_crate_dependencies)]

use criterion::{black_box, criterion_group, criterion_main, measurement::WallTime, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::{Duration, Instant};

#[path = "../benchmark_helpers.rs"]
pub mod benchmark_helpers;
use benchmark_helpers::{benchmarks, BenchParam};

pub mod utils;
use utils::{setup, NAME, PARAMS};

fn verify_duration(params: &BenchParam, truncate_size: u64, n: u64) -> Duration {
    let mut rng = ChaCha20Rng::from_entropy();
    let mut total_duration = Duration::ZERO;
    for _ in 0..n {
        // Setup
        let (mut dataset, telescope) = setup(&mut rng, params);
        // Truncate the dataset to give truncate_size elements to the prover
        dataset.truncate(truncate_size as usize);
        // Prove
        let proof_opt = telescope.prove(&dataset);
        if let Some(proof) = proof_opt {
            // Bench
            let start = Instant::now();
            black_box(telescope.verify(&proof));
            total_duration = total_duration.saturating_add(start.elapsed());
        }
    }
    total_duration
}

/// Bench the duration of both the proving and verifiying algorithm of Alba centralized
fn verify_benches(c: &mut Criterion) {
    benchmarks::<Instant, Duration, WallTime>(
        c,
        PARAMS,
        format!("{} - {}", NAME, "Time"),
        "Verify",
        &verify_duration,
    );
}

mod criterion_group {
    #![allow(missing_docs)]
    use super::{criterion_group, verify_benches, Criterion, Duration};

    // Benchmarking proving and verifying time
    criterion_group!(name = centralized_verifying_time;
                     config = Criterion::default().measurement_time(Duration::from_secs(30));
                     targets = verify_benches
    );
}

criterion_main!(criterion_group::centralized_verifying_time,);
