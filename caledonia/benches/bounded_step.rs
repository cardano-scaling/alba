use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

use caledonia::bounded::Proof;

mod utils;

fn psteps(
    c: &mut Criterion<utils::Steps>,
    lambdas: &[usize],
    s_p: &[usize],
    n_p: &[usize],
    _hash_size: usize,
) {
    let mut group = c.benchmark_group("Alba".to_string());

    fn prove_steps(l: usize, sp: usize, np: usize, truncate_size: usize, n: u64) -> u64 {
        let mut rng = ChaCha20Rng::from_entropy();
        let mut total_steps = 0;
        for _ in 0..n {
            // Setup
            let (mut dataset, bench_setup) = utils::setup_wrapper(&mut rng, l, sp, np);
            dataset.truncate(truncate_size);
            // Bench
            black_box({
                let (steps, _, _) = Proof::bench(&bench_setup, &dataset);
                total_steps += steps;
            });
        }
        total_steps as u64
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

                group.bench_function(utils::bench_id("Proving steps", np, l, sp, np), move |b| {
                    b.iter_custom(|n| prove_steps(l, sp, np, low, n))
                });
                group.bench_function(
                    utils::bench_id("Proving steps", mean, l, sp, np),
                    move |b| b.iter_custom(|n| prove_steps(l, sp, np, mid, n)),
                );
                group.bench_function(utils::bench_id("Proving steps", 100, l, sp, np), move |b| {
                    b.iter_custom(|n| prove_steps(l, sp, np, high, n))
                });
            }
        }
    }
    group.finish();
}

fn prove_step_benches(c: &mut Criterion<utils::Steps>) {
    // prove(c, &[128], &[1000, 5000], &[60, 66, 80], 256);
    psteps(c, &[10], &[1000], &[60, 66, 80], 256);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000).with_measurement(utils::Steps);
                 targets =
    prove_step_benches
);

criterion_main!(benches);
