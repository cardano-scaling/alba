//! Benchmarking the verification time of the Simple Lottery scheme

use criterion::{black_box, criterion_group, criterion_main, measurement::WallTime, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::{Duration, Instant};

mod utils;
use utils::{
    common::{
        criterion_helpers::centralized::{benchmarks, BenchParam},
        test_vectors::{centralized::SHORT_TESTS, D},
    },
    setup, NAME,
};

use digest::{Digest, FixedOutput};
use sha2::Sha256;

/// Function benchmarking n times the verification time
fn verify_duration<H: Digest + FixedOutput>(
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
    // Generate the proof
    let proof_opt = telescope.prove::<D, H>(&dataset);

    if let Some(proof) = proof_opt {
        // Iterate on each sample `n` times
        for _ in 0..n {
            // Benching the verification time
            let start = Instant::now();
            black_box(telescope.verify(&proof));
            total_duration = total_duration.saturating_add(start.elapsed());
        }
    }
    total_duration
}

/// Run verify benchmarks on list of parameters, varying the dataset the prover
/// generates a proof from. More particularly we change
/// - the dataset elements,
/// - the dataset cardinality, between `set_size` and `total_num_elements`.
fn verify_benches(c: &mut Criterion) {
    benchmarks::<Instant, Duration, WallTime>(
        c,
        SHORT_TESTS,
        format!("{} - {}", NAME, "Time"),
        "Sha256/Verify",
        &verify_duration::<Sha256>,
    );
}

mod criterion_group {
    #![allow(missing_docs)]
    use super::{criterion_group, verify_benches, Criterion, Duration};

    criterion_group!(name = lottery_verifying_time;
                     config = Criterion::default().measurement_time(Duration::from_secs(30));
                     targets = verify_benches
    );
}

criterion_main!(criterion_group::lottery_verifying_time,);
