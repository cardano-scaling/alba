use criterion::{
    measurement::{Measurement, ValueFormatter},
    BenchmarkId, Criterion, Throughput,
};

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
        format!("(λ: {l}, Sp:{sp} ({pc}%), n_p:{np}, n_f:{nf})"),
    )
}

pub fn benchmarks<I, V, T: Measurement<Intermediate = I, Value = V>>(
    c: &mut Criterion<T>,
    lambdas: &[usize],
    s_p: &[usize],
    n_p: &[usize],
    n_f: &[usize],
    group_name: String,
    bench_name: String,
    f: &dyn Fn(usize, usize, usize, usize, usize, u64) -> V,
) {
    let mut group = c.benchmark_group(group_name);

    for &l in lambdas {
        for &sp in s_p {
            for &np in n_p {
                for &nf in n_f {
                    // Benchmark where the prover only has access to np percent elements of Sp,
                    // i.e. the minimum number of elements such that the soundness is lower than 2^-λ
                    let low = (sp * np).div_ceil(100);
                    group.bench_function(bench_id(&bench_name, np, l, sp, np, nf), move |b| {
                        b.iter_custom(|n| f(l, sp, np, nf, low, n))
                    });

                    // Benchmark where the prover only has access to (np+100)/2 percent elements of Sp
                    let mean = (100 + np).div_ceil(2);
                    let mid = (sp + low).div_ceil(2);
                    group.bench_function(bench_id(&bench_name, mean, l, sp, np, nf), move |b| {
                        b.iter_custom(|n| f(l, sp, np, nf, mid, n))
                    });

                    // Benchmark where the prover only has access to all elements of Sp
                    group.bench_function(bench_id(&bench_name, 100, l, sp, np, nf), move |b| {
                        b.iter_custom(|n| f(l, sp, np, nf, sp, n))
                    });
                }
            }
        }
    }
    group.finish();
}

// Measurements

/// Nb of DFS call per proof
pub struct Steps;
impl Measurement for Steps {
    type Intermediate = u64;
    type Value = u64;

    fn start(&self) -> Self::Intermediate {
        0
    }

    fn end(&self, _i: Self::Intermediate) -> Self::Value {
        0
    }

    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        v1 + v2
    }

    fn zero(&self) -> Self::Value {
        0
    }

    fn to_f64(&self, value: &Self::Value) -> f64 {
        *value as f64
    }

    fn formatter(&self) -> &dyn ValueFormatter {
        &StepsFormatter
    }
}

struct StepsFormatter;

impl ValueFormatter for StepsFormatter {
    fn format_value(&self, value: f64) -> String {
        format!("{:.4} steps", value)
    }

    fn format_throughput(&self, throughput: &Throughput, value: f64) -> String {
        match throughput {
            Throughput::Bytes(b) => format!("{:.4} spb", value / *b as f64),
            Throughput::Elements(b) => format!("{:.4} steps/{}", value, b),
            Throughput::BytesDecimal(b) => format!("{:.4} spb (decimal)", value / *b as f64),
        }
    }

    fn scale_values(&self, _typical_value: f64, _values: &mut [f64]) -> &'static str {
        "steps"
    }

    fn scale_throughputs(
        &self,
        _typical_value: f64,
        throughput: &Throughput,
        values: &mut [f64],
    ) -> &'static str {
        match throughput {
            Throughput::Bytes(n) => {
                for val in values {
                    *val /= *n as f64;
                }
                "spb"
            }
            Throughput::Elements(n) => {
                for val in values {
                    *val /= *n as f64;
                }
                "spe"
            }
            Throughput::BytesDecimal(n) => {
                for val in values {
                    *val /= *n as f64;
                }
                "spb (decimal)"
            }
        }
    }

    fn scale_for_machines(&self, _values: &mut [f64]) -> &'static str {
        "steps"
    }
}

/// Nb of repet, times prove_index was called, per proof
///
pub struct Repetitions;
impl Measurement for Repetitions {
    type Intermediate = u64;
    type Value = u64;

    fn start(&self) -> Self::Intermediate {
        0
    }

    fn end(&self, _i: Self::Intermediate) -> Self::Value {
        0
    }

    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        v1 + v2
    }

    fn zero(&self) -> Self::Value {
        0
    }

    fn to_f64(&self, value: &Self::Value) -> f64 {
        *value as f64
    }

    fn formatter(&self) -> &dyn ValueFormatter {
        &RepetitionsFormatter
    }
}

struct RepetitionsFormatter;

impl ValueFormatter for RepetitionsFormatter {
    fn format_value(&self, value: f64) -> String {
        format!("{:.4} repet", value)
    }

    fn format_throughput(&self, throughput: &Throughput, value: f64) -> String {
        match throughput {
            Throughput::Bytes(b) => format!("{:.4} rpb", value / *b as f64),
            Throughput::Elements(b) => format!("{:.4} repet/{}", value, b),
            Throughput::BytesDecimal(b) => format!("{:.4} rpb (decimal)", value / *b as f64),
        }
    }

    fn scale_values(&self, _typical_value: f64, _values: &mut [f64]) -> &'static str {
        "repet"
    }

    fn scale_throughputs(
        &self,
        _typical_value: f64,
        throughput: &Throughput,
        values: &mut [f64],
    ) -> &'static str {
        match throughput {
            Throughput::Bytes(n) => {
                for val in values {
                    *val /= *n as f64;
                }
                "rpb"
            }
            Throughput::Elements(n) => {
                for val in values {
                    *val /= *n as f64;
                }
                "rpe"
            }
            Throughput::BytesDecimal(n) => {
                for val in values {
                    *val /= *n as f64;
                }
                "rpb (decimal)"
            }
        }
    }

    fn scale_for_machines(&self, _values: &mut [f64]) -> &'static str {
        "repet"
    }
}
