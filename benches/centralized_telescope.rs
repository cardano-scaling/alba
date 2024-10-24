use criterion::{black_box, criterion_group, criterion_main, measurement::WallTime, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use std::time::{Duration, Instant};

use alba::{
    centralized_telescope::{Params, Proof, Setup},
    test_utils::gen_items,
};

pub mod criterion_helpers;
use criterion_helpers::{benchmarks, Repetitions, Steps};

// Global variables
const NAME: &str = "Centralized";
// Benchmark parameters
const L: &[u32] = &[50, 128]; // Security parameter
const SP: &[usize] = &[1_000]; // Size of set to lower bound
const NP: &[usize] = &[80, 90, 95, 98]; // Alba's np parameter, |Sp| >= np
const NF: &[usize] = &[67, 75]; // Alba's nf parameter,  |Sp| >= np > nf

/// Function generating a random set of elements to bench and calling Alba's centralized setup
pub fn centralized_setup(
    rng: &mut ChaCha20Rng,
    l: u32,
    sp: usize,
    np: usize,
    nf: usize,
) -> (Vec<[u8; 32]>, Setup) {
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset: Vec<[u8; 32]> = gen_items(seed, sp);
    let params = Params {
        lambda_sec: l,
        lambda_rel: l,
        n_p: (np * sp).div_ceil(100),
        n_f: (nf * sp).div_ceil(100),
    };
    (dataset, Setup::new(&params))
}

/// Bench the duration of both the proving and verifiying algorithm of Alba centralized
fn time_benches(c: &mut Criterion) {
    fn prove_duration(
        l: u32,
        sp: usize,
        np: usize,
        nf: usize,
        truncate_size: usize,
        n: u64,
    ) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, l, sp, np, nf);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size);
            // Bench
            let start = Instant::now();
            black_box({
                Proof::prove(&bench_setup, &dataset);
            });
            total_duration = total_duration.saturating_add(start.elapsed());
        }
        total_duration
    }

    fn verify_duration(
        l: u32,
        sp: usize,
        np: usize,
        nf: usize,
        truncate_size: usize,
        n: u64,
    ) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, l, sp, np, nf);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size);
            // Prove
            let proof_opt = Proof::prove(&bench_setup, &dataset);
            // Bench
            if proof_opt.is_some() {
                let start = Instant::now();
                black_box({
                    Proof::verify(&bench_setup, proof_opt.unwrap());
                });
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
        "Prove".to_string(),
        &prove_duration,
    );

    benchmarks::<Instant, Duration, WallTime>(
        c,
        L,
        SP,
        NP,
        NF,
        format!("{} - {}", NAME, "Time"),
        "Verify".to_string(),
        &verify_duration,
    );
}

/// Bench the number of steps, i.e. DFS calls, of Alba centralized prover
fn step_benches(c: &mut Criterion<Steps>) {
    fn prove_steps(l: u32, sp: usize, np: usize, nf: usize, truncate_size: usize, n: u64) -> u64 {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_steps = 0;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, l, sp, np, nf);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size);
            // Bench
            black_box({
                let (steps, _, _) = Proof::bench(&bench_setup, &dataset);
                total_steps += steps;
            });
        }
        total_steps as u64
    }

    benchmarks::<u64, u64, Steps>(
        c,
        L,
        SP,
        NP,
        NF,
        format!("{} - {}", NAME, "Steps"),
        "Prove".to_string(),
        &prove_steps,
    );
}

/// Bench the number of repetitions, i.e. the "r" parameter, of Alba centralized prover
fn repetition_benches(c: &mut Criterion<Repetitions>) {
    fn prove_repetitions(
        l: u32,
        sp: usize,
        np: usize,
        nf: usize,
        truncate_size: usize,
        n: u64,
    ) -> u64 {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_repetitions = 0;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = centralized_setup(&mut rng, l, sp, np, nf);
            // Truncate the dataset to give truncate_size elements to the prover
            dataset.truncate(truncate_size);
            // Bench
            black_box({
                let (_, r, _) = Proof::bench(&bench_setup, &dataset);
                total_repetitions += 1 + r;
            });
        }
        total_repetitions as u64
    }

    benchmarks::<u64, u64, Repetitions>(
        c,
        L,
        SP,
        NP,
        NF,
        format!("{} - {}", NAME, "Repetitions"),
        "Prove".to_string(),
        &prove_repetitions,
    );
}

criterion_group!(name = centralized_telescope_time;
                 config = Criterion::default().measurement_time(Duration::from_secs(30));
                 targets = time_benches
);

criterion_group!(name = centralized_telescope_step;
    config = Criterion::default().with_measurement(Steps).measurement_time(Duration::from_secs(30));
    targets = step_benches
);

criterion_group!(name = centralized_telescope_repetitions;
    config = Criterion::default().with_measurement(Repetitions).measurement_time(Duration::from_secs(30));
    targets = repetition_benches
);

criterion_main!(
    centralized_telescope_time,
    centralized_telescope_step, // centralized_telescope_repetitions
);
