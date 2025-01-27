//! Benchmarking the number of DFS calls, aka number of steps, of the
//! Centralized Telescope scheme

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::Duration;

mod utils;
use utils::{
    common::{
        criterion_helpers::{
            centralized::{benchmarks, BenchParam},
            Steps,
        },
        test_vectors::centralized::SHORT_TESTS,
    },
    setup, NAME,
};

use alba::centralized_telescope::proof::Proof;

/// Function benchmarking `sample_size` times the number of DFS calls, aka steps
#[allow(clippy::unit_arg)]
fn prove_steps(param: &BenchParam, truncate_size: u64, n: u64) -> u64 {
    let mut rng = ChaCha20Rng::from_entropy();
    let mut total_steps = 0u64;

    // Setup
    let (mut dataset, telescope) = setup(&mut rng, param);
    let set_size = telescope.get_set_size();
    let params = telescope.get_params();
    // Truncate the dataset to give truncate_size elements to the prover
    dataset.truncate(truncate_size as usize);

    // As the number of steps is determinitistic, we do not iterate but bench a
    // single time the number of steps/DFS calls while generating a proof and
    // retun this number times n to fasten the bechmark.
    black_box({
        let steps = Proof::bench(set_size, &params, &dataset).0;
        total_steps = total_steps.saturating_add(steps);
    });
    total_steps.saturating_mul(n)
}

/// Run step benchmarks on list of parameters, varying the dataset the prover
/// generates a proof from. More particularly we change
/// - the dataset elements,
/// - the dataset cardinality, between `set_size` and `total_num_elements`.
fn step_benches(c: &mut Criterion<Steps>) {
    benchmarks::<u64, u64, Steps>(
        c,
        SHORT_TESTS,
        format!("{} - {}", NAME, "Steps"),
        "Prove",
        &prove_steps,
    );
}

mod criterion_group {
    #![allow(missing_docs)]
    use super::{criterion_group, step_benches, Criterion, Duration, Steps};
    use crate::utils::common::criterion_helpers::{MEASUREMENT_TIME_SEC, SAMPLE_SIZE};

    criterion_group!(name = centralized_step;
        config = Criterion::default().with_measurement(Steps).measurement_time(Duration::from_secs(MEASUREMENT_TIME_SEC)).sample_size(SAMPLE_SIZE);
        targets = step_benches
    );
}

criterion_main!(criterion_group::centralized_step);
