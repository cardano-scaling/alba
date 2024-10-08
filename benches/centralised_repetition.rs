use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
use std::time::Duration;

use caledonia::centralised::Proof;

pub mod criterion_helpers;
pub mod utils;

fn prepetitions(
    c: &mut Criterion<criterion_helpers::Repetitions>,
    lambdas: &[usize],
    s_p: &[usize],
    n_p: &[usize],
    n_f: &[usize],
) {
    let mut group = c.benchmark_group("Centralised".to_string());

    fn prove_repetitions(
        l: usize,
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
            let (mut dataset, bench_setup) =
                utils::setup_centralised_wrapper(&mut rng, l, sp, np, nf);
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
                for &nf in n_f {
                    // Bench with all of np% of Sp
                    let low = (sp * np).div_ceil(100);
                    group.bench_function(
                        criterion_helpers::bench_id("Proving repetitions", np, l, sp, np, nf),
                        move |b| b.iter_custom(|n| prove_repetitions(l, sp, np, nf, low, n)),
                    );

                    // Bench with  (100+np)/2 percent of Sp
                    let mean = (100 + np).div_ceil(2);
                    let mid = (sp + low).div_ceil(2);
                    group.bench_function(
                        criterion_helpers::bench_id("Proving repetitions", mean, l, sp, np, nf),
                        move |b| b.iter_custom(|n| prove_repetitions(l, sp, np, nf, mid, n)),
                    );

                    // Bench with all of Sp
                    group.bench_function(
                        criterion_helpers::bench_id("Proving repetitions", 100, l, sp, np, nf),
                        move |b| b.iter_custom(|n| prove_repetitions(l, sp, np, nf, sp, n)),
                    );
                }
            }
        }
    }
    group.finish();
}

fn prove_step_benches(c: &mut Criterion<criterion_helpers::Repetitions>) {
    prepetitions(c, &[50, 128], &[100], &[80, 90, 95, 98], &[67, 75]);
}

criterion_group!(name = benches;
                 config = Criterion::default().with_measurement(criterion_helpers::Repetitions).measurement_time(Duration::from_secs(30));
                 targets =
    prove_step_benches
);

criterion_main!(benches);
