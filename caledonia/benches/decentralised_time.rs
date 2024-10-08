use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::{Duration, Instant};

use caledonia::bounded::Proof;

pub mod utils;

fn prove(c: &mut Criterion, lambdas: &[usize], s_p: &[usize], n_p: &[usize], _hash_size: usize) {
    let mut group = c.benchmark_group("Alba Bounded".to_string());

    fn prove_duration(l: usize, sp: usize, np: usize, truncate_size: usize, n: u64) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = utils::setup_bounded_wrapper(&mut rng, l, sp, np);
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

    for &l in lambdas {
        for &sp in s_p {
            for &np in n_p {
                // Bench with all of Sp
                let high = sp;

                // Bench with all of np% of Sp
                let low = (high * np).div_ceil(100);

                // Bench with  (100+np)/2 percent of Sp
                let mean = (100 + np).div_ceil(2);
                let mid = (high + low).div_ceil(2);

                group.bench_function(utils::bench_id("Proving time", np, l, sp, np), move |b| {
                    b.iter_custom(|n| prove_duration(l, sp, np, low, n))
                });
                group.bench_function(utils::bench_id("Proving time", mean, l, sp, np), move |b| {
                    b.iter_custom(|n| prove_duration(l, sp, np, mid, n))
                });
                group.bench_function(utils::bench_id("Proving time", 100, l, sp, np), move |b| {
                    b.iter_custom(|n| prove_duration(l, sp, np, high, n))
                });
            }
        }
    }
    group.finish();
}

fn verify(c: &mut Criterion, lambdas: &[usize], s_p: &[usize], n_p: &[usize], _hash_size: usize) {
    let mut group = c.benchmark_group("Alba".to_string());

    fn verify_duration(l: usize, sp: usize, np: usize, truncate_size: usize, n: u64) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = utils::setup_bounded_wrapper(&mut rng, l, sp, np);
            dataset.truncate(truncate_size);
            // Prove
            let proof = Proof::prove(&bench_setup, &dataset);
            // Bench
            let start = Instant::now();
            black_box({
                Proof::verify(&bench_setup, proof);
            });
            total_duration = total_duration.saturating_add(start.elapsed());
        }
        total_duration
    }

    for &l in lambdas {
        for &sp in s_p {
            for &np in n_p {
                // Bench with all of Sp
                let high = sp;

                // Bench with all of np% of Sp
                let low = (high * np).div_ceil(100);

                // Bench with  (100+np)/2 percent of Sp
                let mean = (100 + np).div_ceil(2);
                let mid = (high + low).div_ceil(2);

                group.bench_function(
                    utils::bench_id("Verification time", np, l, sp, np),
                    move |b| b.iter_custom(|n| verify_duration(l, sp, np, low, n)),
                );
                group.bench_function(
                    utils::bench_id("Verification time", mean, l, sp, np),
                    move |b| b.iter_custom(|n| verify_duration(l, sp, np, mid, n)),
                );
                group.bench_function(
                    utils::bench_id("Verification time", 100, l, sp, np),
                    move |b| b.iter_custom(|n| verify_duration(l, sp, np, high, n)),
                );
            }
        }
    }
    group.finish();
}

fn prove_benches(c: &mut Criterion) {
    // prove(c, &[128], &[1000, 5000], &[60, 66, 80], 256);
    prove(c, &[128], &[1_000_000], &[60, 66, 80], 256);
}

fn verify_benches(c: &mut Criterion) {
    verify(c, &[10], &[1000], &[60, 66, 80], 256);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    prove_benches,
    verify_benches
);

criterion_main!(benches);