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

use crate::utils::common::test_vectors::D;
use digest::{Digest, FixedOutput};
use sha2::Sha256;

/// Function benchmarking `sample_size` times the proving time
fn prove_duration<H: Digest + FixedOutput>(
    params: &BenchParam,
    truncate_size: u64,
    n: u64,
) -> Duration {
    let mut rng = ChaCha20Rng::from_os_rng();
    let mut total_duration = Duration::ZERO;

    // Setup
    let (mut dataset, telescope) = setup(&mut rng, params);
    // Truncate the dataset to give truncate_size elements to the prover
    dataset.truncate(truncate_size as usize);

    // Iterate on each sample `n` times
    for _ in 0..n {
        // Bench the proving time
        let start = Instant::now();
        black_box(telescope.prove::<D, H>(&dataset));
        total_duration = total_duration.saturating_add(start.elapsed());
    }
    total_duration
}

/// Run prove benchmarks on list of parameters, varying the dataset the prover
/// generates a proof from. More particularly we change
/// - the dataset elements,
/// - the dataset cardinality, between `set_size` and `total_num_elements`.
fn proving_benches(c: &mut Criterion) {
    benchmarks::<Instant, Duration, WallTime>(
        c,
        SHORT_TESTS,
        format!("{} - {}", NAME, "Time"),
        "Sha256/Prove",
        &prove_duration::<Sha256>,
    );
}

mod criterion_group {
    #![allow(missing_docs)]
    use super::{criterion_group, proving_benches, Criterion, Duration};
    use crate::utils::common::criterion_helpers::{MEASUREMENT_TIME_SEC, SAMPLE_SIZE};

    criterion_group!(name = centralized_proving_time;
                     config = Criterion::default().measurement_time(Duration::from_secs(MEASUREMENT_TIME_SEC)).sample_size(SAMPLE_SIZE);
                     targets = proving_benches
    );
}

criterion_main!(criterion_group::centralized_proving_time);
