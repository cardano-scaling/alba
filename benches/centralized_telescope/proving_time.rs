//! Benchmarking the proving time of the Centralized Telescope scheme

use criterion::{black_box, criterion_group, criterion_main, measurement::WallTime, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::{Duration, Instant};

mod utils;
use utils::{
    common::{
        criterion_helpers::centralized::{benchmarks, BenchParam},
        test_vectors::centralized::SHORT_TESTS,
    },
    setup, NAME,
};

/// Function benchmarking n times the proving time
fn prove_duration(params: &BenchParam, truncate_size: u64, n: u64) -> Duration {
    let mut rng = ChaCha20Rng::from_entropy();
    let mut total_duration = Duration::ZERO;
    for _ in 0..n {
        // Setup
        let (mut dataset, telescope) = setup(&mut rng, params);
        // Truncate the dataset to give truncate_size elements to the prover
        dataset.truncate(truncate_size as usize);
        // Bench the proving time
        let start = Instant::now();
        black_box(telescope.prove(&dataset));
        total_duration = total_duration.saturating_add(start.elapsed());
    }
    total_duration
}

/// Run prove benchmarks on list of parameters, varying the dataset the prover
/// generates a proof from. More particularly we change
/// - the dataset elements,
/// - the dataset cardinality, between `set_size` and `set_card`.
fn proving_benches(c: &mut Criterion) {
    benchmarks::<Instant, Duration, WallTime>(
        c,
        SHORT_TESTS,
        format!("{} - {}", NAME, "Time"),
        "Prove",
        &prove_duration,
    );
}

mod criterion_group {
    #![allow(missing_docs)]
    use super::{criterion_group, proving_benches, Criterion, Duration};

    criterion_group!(name = centralized_proving_time;
                     config = Criterion::default().measurement_time(Duration::from_secs(30));
                     targets = proving_benches
    );
}

criterion_main!(criterion_group::centralized_proving_time,);
