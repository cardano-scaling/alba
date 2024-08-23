use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::Duration;

use caledonia::bounded::Proof;

pub mod utils;

fn prepetitions(
    c: &mut Criterion<utils::Repetitions>,
    lambdas: &[usize],
    s_p: &[usize],
    n_p: &[usize],
    _hash_size: usize,
) {
    let mut group = c.benchmark_group("Alba Bounded".to_string());

    fn prove_repetitions(l: usize, sp: usize, np: usize, truncate_size: usize, n: u64) -> u64 {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_repetitions = 0;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = utils::setup_bounded_wrapper(&mut rng, l, sp, np);
            dataset.truncate(truncate_size);
            // Bench
            black_box({
                let (_, r, _) = Proof::bench(&bench_setup, &dataset);
                total_repetitions += 1 + r;
            });
        }
        total_repetitions as u64
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
                    utils::bench_id("Proving repetitions", np, l, sp, np),
                    move |b| b.iter_custom(|n| prove_repetitions(l, sp, np, low, n)),
                );
                group.bench_function(
                    utils::bench_id("Proving repetitions", mean, l, sp, np),
                    move |b| b.iter_custom(|n| prove_repetitions(l, sp, np, mid, n)),
                );
                group.bench_function(
                    utils::bench_id("Proving repetitions", 100, l, sp, np),
                    move |b| b.iter_custom(|n| prove_repetitions(l, sp, np, high, n)),
                );
            }
        }
    }
    group.finish();
}

fn prove_step_benches(c: &mut Criterion<utils::Repetitions>) {
    prepetitions(c, &[50], &[100], &[60, 66, 80], 256);
}

criterion_group!(name = benches;
                 config = Criterion::default().sample_size(500).nresamples(1000).measurement_time(Duration::from_secs(30)).with_measurement(utils::Repetitions);
                 targets =
    prove_step_benches
);

criterion_main!(benches);
