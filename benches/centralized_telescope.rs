//! Benchmarking centralized_telescope

use criterion::{black_box, criterion_group, criterion_main, measurement::WallTime, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use std::time::{Duration, Instant};

use alba::centralized_telescope::{algorithm, init, params::Params, setup::Setup};

pub mod criterion_helpers;
use criterion_helpers::{benchmarks, Repetitions, Steps};

#[path = "../src/utils/test_utils.rs"]
mod test_utils;

// Global variables
const NAME: &str = "Centralized";
// Benchmark parameters
const L: &[f64] = &[50.0, 128.0]; // Security parameter
const SP: &[u64] = &[1_000]; // Size of set to lower bound
const NP: &[u64] = &[80, 90, 95, 98]; // Alba's np parameter, |Sp| >= np
const NF: &[u64] = &[67, 75]; // Alba's nf parameter,  |Sp| >= np > nf

/// Function generating a random set of elements to bench and calling Alba's centralized setup
pub fn centralized_setup(
    rng: &mut ChaCha20Rng,
    l: f64,
    sp: u64,
    np: u64,
    nf: u64,
) -> (Vec<[u8; 32]>, Setup) {
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset: Vec<[u8; 32]> = test_utils::gen_items(&seed, sp);
    let params = Params {
        soundness_param: l,
        completeness_param: l,
        set_size: np.saturating_mul(sp).div_ceil(100),
        lower_bound: nf.saturating_mul(sp).div_ceil(100),
    };
    (dataset, init::make_setup(&params))
}

/// Bench the duration of both the proving and verifiying algorithm of Alba centralized
fn time_benches(c: &mut Criterion) {
    #[allow(clippy::unit_arg)]
    fn prove_duration(l: f64, sp: u64, np: u64, nf: u64, truncate_size: u64, n: u64) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, l, sp, np, nf);
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
    fn verify_duration(l: f64, sp: u64, np: u64, nf: u64, truncate_size: u64, n: u64) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, l, sp, np, nf);
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
        L,
        SP,
        NP,
        NF,
        format!("{} - {}", NAME, "Time"),
        "Prove",
        &prove_duration,
    );

    benchmarks::<Instant, Duration, WallTime>(
        c,
        L,
        SP,
        NP,
        NF,
        format!("{} - {}", NAME, "Time"),
        "Verify",
        &verify_duration,
    );
}

/// Bench the number of steps, i.e. DFS calls, of Alba centralized prover
fn step_benches(c: &mut Criterion<Steps>) {
    #[allow(clippy::unit_arg)]
    fn prove_steps(l: f64, sp: u64, np: u64, nf: u64, truncate_size: u64, n: u64) -> u64 {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_steps = 0;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, l, sp, np, nf);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size as usize);
            // Bench
            black_box({
                let (steps, _, _) = algorithm::bench(&bench_setup, &dataset);
                total_steps += steps;
            });
        }
        total_steps
    }

    benchmarks::<u64, u64, Steps>(
        c,
        L,
        SP,
        NP,
        NF,
        format!("{} - {}", NAME, "Steps"),
        "Prove",
        &prove_steps,
    );
}

/// Bench the number of repetitions, i.e. the "r" parameter, of Alba centralized prover
fn repetition_benches(c: &mut Criterion<Repetitions>) {
    #[allow(clippy::unit_arg)]
    fn prove_repetitions(l: f64, sp: u64, np: u64, nf: u64, truncate_size: u64, n: u64) -> u64 {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_repetitions = 0;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, l, sp, np, nf);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size as usize);
            // Bench
            black_box({
                let (_, r, _) = algorithm::bench(&bench_setup, &dataset);
                total_repetitions += 1 + r;
            });
        }
        total_repetitions
    }

    benchmarks::<u64, u64, Repetitions>(
        c,
        L,
        SP,
        NP,
        NF,
        format!("{} - {}", NAME, "Repetitions"),
        "Prove",
        &prove_repetitions,
    );
}

mod criterion_groups {
    #![allow(missing_docs)]
    use super::{
        criterion_group, repetition_benches, step_benches, time_benches, Criterion, Duration,
        Repetitions, Steps,
    };

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

    // Benchmarking the number of repetitions, i.e. prove_index calls, per proof
    criterion_group!(name = centralized_repetitions;
        config = Criterion::default().with_measurement(Repetitions).measurement_time(Duration::from_secs(30));
        targets = repetition_benches
    );
}

criterion_main!(
    criterion_groups::centralized_time,
    criterion_groups::centralized_step,
    criterion_groups::centralized_repetitions
);
