use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::{Duration, Instant};

use caledonia::weighted_decentralised::Proof;

pub mod utils;

fn prove(
    c: &mut Criterion,
    lambdas: &[usize],
    s_p: &[usize],
    s_u: &[usize],
    n_p: &[usize],
    _hash_size: usize,
) {
    let mut group = c.benchmark_group("Alba weighted decentralised".to_string());

    fn prove_duration(
        l: usize,
        sp: usize,
        users: usize,
        np: usize,
        truncate_size: usize,
        n: u64,
    ) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) =
                utils::setup_weighted_wrapper(&mut rng, l, sp, users, np);
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
            for &users in s_u {
                for &np in n_p {
                    // Bench with all of Sp
                    let high = sp;

                    // Bench with all of np% of Sp
                    let low = (high * np).div_ceil(100);

                    // Bench with  (100+np)/2 percent of Sp
                    let mean = (100 + np).div_ceil(2);
                    let mid = (high + low).div_ceil(2);

                    group.bench_function(
                        utils::bench_id_users("Proving time", np, l, sp, users, np),
                        move |b| b.iter_custom(|n| prove_duration(l, sp, users, np, low, n)),
                    );
                    group.bench_function(
                        utils::bench_id_users("Proving time", mean, l, sp, users, np),
                        move |b| b.iter_custom(|n| prove_duration(l, sp, users, np, mid, n)),
                    );
                    group.bench_function(
                        utils::bench_id_users("Proving time", 100, l, sp, users, np),
                        move |b| b.iter_custom(|n| prove_duration(l, sp, users, np, high, n)),
                    );
                }
            }
        }
    }
    group.finish();
}

fn verify(
    c: &mut Criterion,
    lambdas: &[usize],
    s_p: &[usize],
    s_u: &[usize],
    n_p: &[usize],
    _hash_size: usize,
) {
    let mut group = c.benchmark_group("Alba weighted decentralised".to_string());

    fn verify_duration(
        l: usize,
        sp: usize,
        users: usize,
        np: usize,
        truncate_size: usize,
        n: u64,
    ) -> Duration {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) =
                utils::setup_weighted_wrapper(&mut rng, l, sp, users, np);
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
            for &users in s_u {
                for &np in n_p {
                    // Bench with all of Sp
                    let high = sp;

                    // Bench with all of np% of Sp
                    let low = (high * np).div_ceil(100);

                    // Bench with  (100+np)/2 percent of Sp
                    let mean = (100 + np).div_ceil(2);
                    let mid = (high + low).div_ceil(2);

                    group.bench_function(
                        utils::bench_id_users("Verification time", np, l, sp, users, np),
                        move |b| b.iter_custom(|n| verify_duration(l, sp, users, np, low, n)),
                    );
                    group.bench_function(
                        utils::bench_id_users("Verification time", mean, l, sp, users, np),
                        move |b| b.iter_custom(|n| verify_duration(l, sp, users, np, mid, n)),
                    );
                    group.bench_function(
                        utils::bench_id_users("Verification time", 100, l, sp, users, np),
                        move |b| b.iter_custom(|n| verify_duration(l, sp, users, np, high, n)),
                    );
                }
            }
        }
    }
    group.finish();
}

fn prove_benches(c: &mut Criterion) {
    // prove(c, &[128], &[1_000, 5_000], &[1_000, 1_000], &[60, 66, 80], 256);
    prove(c, &[128], &[100_000], &[1_000], &[60, 66, 80], 256);
}

fn verify_benches(c: &mut Criterion) {
    verify(c, &[10], &[1_000], &[100], &[60, 66, 80], 256);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    prove_benches,
    verify_benches
);

criterion_main!(benches);