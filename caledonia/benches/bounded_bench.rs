use std::time::{Duration, Instant};

// use blake2::{digest::consts::U64, Blake2b, Digest};
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use caledonia::bounded::{Params, Proof, Setup};
use utils::gen_items;

fn setup_wrapper(rng: &mut ChaCha20Rng, l: usize, sp: usize, np: usize) -> (Vec<[u8; 32]>, Setup) {
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset: Vec<[u8; 32]> = gen_items(seed, sp);
    let params = Params {
        lambda_sec: l,
        lambda_rel: l,
        n_p: (np * sp).div_ceil(100),
        n_f: ((100 - np) * sp).div_ceil(100),
    };
    (dataset, Setup::new(&params))
}

fn prove(c: &mut Criterion, lambdas: &[usize], s_p: &[usize], n_p: &[usize], _hash_size: usize) {
    let mut group = c.benchmark_group("Alba".to_string());
    fn bench_id(pc: usize, l: usize, sp: usize, np: usize) -> BenchmarkId {
        BenchmarkId::new(
            "Proving time",
            format!("Security parameter: {l}, Sp:{sp} ({pc}%), n_p:{np}"),
        )
    }

    fn prove_duration(l: usize, sp: usize, np: usize, truncate_size: usize, n: u64) -> Duration {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = setup_wrapper(&mut rng, l, sp, np);
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

                group.bench_function(bench_id(np, l, sp, np), move |b| {
                    b.iter_custom(|n| prove_duration(l, sp, np, low, n))
                });
                group.bench_function(bench_id(mean, l, sp, np), move |b| {
                    b.iter_custom(|n| prove_duration(l, sp, np, mid, n))
                });
                group.bench_function(bench_id(100, l, sp, np), move |b| {
                    b.iter_custom(|n| prove_duration(l, sp, np, high, n))
                });
            }
        }
    }
}

fn verify(c: &mut Criterion, lambdas: &[usize], s_p: &[usize], n_p: &[usize], _hash_size: usize) {
    let mut group = c.benchmark_group("Alba".to_string());
    fn bench_id(pc: usize, l: usize, sp: usize, np: usize) -> BenchmarkId {
        BenchmarkId::new(
            "Verification time",
            format!("Security parameter: {l}, Sp:{sp} ({pc}%), n_p:{np}"),
        )
    }

    fn verify_duration(l: usize, sp: usize, np: usize, truncate_size: usize, n: u64) -> Duration {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let mut total_duration = Duration::ZERO;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = setup_wrapper(&mut rng, l, sp, np);
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

                group.bench_function(bench_id(np, l, sp, np), move |b| {
                    b.iter_custom(|n| verify_duration(l, sp, np, low, n))
                });
                group.bench_function(bench_id(mean, l, sp, np), move |b| {
                    b.iter_custom(|n| verify_duration(l, sp, np, mid, n))
                });
                group.bench_function(bench_id(100, l, sp, np), move |b| {
                    b.iter_custom(|n| verify_duration(l, sp, np, high, n))
                });
            }
        }
    }
}

fn prove_benches(c: &mut Criterion) {
    // prove(c, &[128], &[1000, 5000], &[60, 66, 80], 256);
    prove(c, &[10], &[1000], &[60, 66, 80], 256);
}

fn verify_benches(c: &mut Criterion) {
    // prove(c, &[128], &[1000, 5000], &[60, 66, 80], 256);
    verify(c, &[10], &[1000], &[60, 66, 80], 256);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    prove_benches,
    verify_benches
);
criterion_main!(benches);
