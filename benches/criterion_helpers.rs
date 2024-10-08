use criterion::BenchmarkId;

// Criterion helpers

pub fn bench_id(
    bench_name: &str,
    pc: usize,
    l: usize,
    sp: usize,
    np: usize,
    nf: usize,
) -> BenchmarkId {
    BenchmarkId::new(
        bench_name,
        format!("Security parameter: {l}, Sp:{sp} ({pc}%), n_p:{np}, n_f:{nf}"),
    )
}
