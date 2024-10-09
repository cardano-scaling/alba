use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::{Duration, Instant};

use caledonia::centralised::Proof;

pub mod criterion_helpers;
pub mod utils;

fn prove(c: &mut Criterion, lambdas: &[usize], s_p: &[usize], n_p: &[usize], n_f: &[usize]) {
    let mut group = c.benchmark_group("Centralised".to_string());

    fn prove_duration(
        l: usize,
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
            let (mut dataset, bench_setup) =
                utils::setup_centralised_wrapper(&mut rng, l, sp, np, nf);
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
                for &nf in n_f {
                    // Bench with all of np% of Sp
                    let low = (sp * np).div_ceil(100);
                    group.bench_function(
                        criterion_helpers::bench_id("Proving time", np, l, sp, np, nf),
                        move |b| b.iter_custom(|n| prove_duration(l, sp, np, nf, low, n)),
                    );

                    // Bench with  (100+np)/2% of Sp
                    let mean = (100 + np).div_ceil(2);
                    let mid = (sp + low).div_ceil(2);
                    group.bench_function(
                        criterion_helpers::bench_id("Proving time", mean, l, sp, np, nf),
                        move |b| b.iter_custom(|n| prove_duration(l, sp, np, nf, mid, n)),
                    );

                    // Bench with all of Sp
                    group.bench_function(
                        criterion_helpers::bench_id("Proving time", 100, l, sp, np, nf),
                        move |b| b.iter_custom(|n| prove_duration(l, sp, np, nf, sp, n)),
                    );
                }
            }
        }
    }
    group.finish();
}

fn verify(c: &mut Criterion, lambdas: &[usize], s_p: &[usize], n_p: &[usize], n_f: &[usize]) {
    let mut group = c.benchmark_group("Centralised".to_string());

    fn verify_duration(
        l: usize,
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
            let (mut dataset, bench_setup) =
                utils::setup_centralised_wrapper(&mut rng, l, sp, np, nf);
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
                for &nf in n_f {
                    // Bench with all of np% of Sp
                    let low = (sp * np).div_ceil(100);
                    group.bench_function(
                        criterion_helpers::bench_id("Verification time", np, l, sp, np, nf),
                        move |b| b.iter_custom(|n| verify_duration(l, sp, np, nf, low, n)),
                    );

                    // Bench with  (100+np)/2% of Sp
                    let mean = (100 + np).div_ceil(2);
                    let mid = (sp + low).div_ceil(2);
                    group.bench_function(
                        criterion_helpers::bench_id("Verification time", mean, l, sp, np, nf),
                        move |b| b.iter_custom(|n| verify_duration(l, sp, np, nf, mid, n)),
                    );

                    // Bench with all of Sp
                    group.bench_function(
                        criterion_helpers::bench_id("Verification time", 100, l, sp, np, nf),
                        move |b| b.iter_custom(|n| verify_duration(l, sp, np, nf, sp, n)),
                    );
                }
            }
        }
    }
    group.finish();
}

fn prove_benches(c: &mut Criterion) {
    prove(c, &[50, 128], &[1_000], &[80, 90, 95, 98], &[67, 75]);
}

fn verify_benches(c: &mut Criterion) {
    verify(c, &[50, 128], &[1_000], &[80, 90, 95, 98], &[67, 75]);
}

criterion_group!(name = benches;
                 config = Criterion::default().measurement_time(Duration::from_secs(30));
                 targets =
    prove_benches,
    verify_benches
);

criterion_main!(benches);
