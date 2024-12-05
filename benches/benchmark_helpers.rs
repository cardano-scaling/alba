//! Criterion helper functions including new Measurements and wrappers on
//! BenchmarkId and BenchmarkGroup

use alba as _;
use blake2 as _;
use rand as _;
use rand_chacha as _;
use rand_core as _;

use criterion::{
    measurement::{Measurement, ValueFormatter},
    BenchmarkId, Criterion, Throughput,
};

/// Benchmark parameters
#[derive(Debug, Clone, Copy)]
pub struct BenchParam {
    /// Soundness Security parameter
    pub lambda_sec: f64,
    /// Completeness Security parameter  
    pub lambda_rel: f64,
    /// Alba's set cardinality (|Sp|)      
    pub set_card: u64,
    /// Alba's set_size (np) parameter in percentage of the set cardinality        
    pub set_size_per: u64,
    /// Alba's lower bound (nf) parameter in percentage of the set cardinality
    pub lower_bound_per: u64,
}

// Benchmark parameters
// The parameters chosen correspond to cases of interest for Cardano's Peras and Leios protocols.

/// This case corresponds to the minimum security requirements with optimistic set_size
pub const LOW_PARAM: BenchParam = BenchParam {
    lambda_sec: 50.0,
    lambda_rel: 50.0,
    set_card: 1_000,
    set_size_per: 90,
    lower_bound_per: 67,
};

/// This case corresponds to medium security requirements with more realistic set_size
pub const MID_PARAM: BenchParam = BenchParam {
    lambda_sec: 80.0,
    lambda_rel: 80.0,
    set_card: 1_000,
    set_size_per: 80,
    lower_bound_per: 67,
};

/// This case corresponds to medium security requirements with more realistic set_size
pub const HIGH_PARAM: BenchParam = BenchParam {
    lambda_sec: 128.0,
    lambda_rel: 128.0,
    set_card: 1_000,
    set_size_per: 80,
    lower_bound_per: 67,
};

/// Helper function creating a Benchmark ID
pub fn bench_id(bench_name: &str, pc: u64, param: &BenchParam) -> BenchmarkId {
    BenchmarkId::new(
        bench_name,
        format!(
            "(λsec/rel: {}-{}, Sp:{} ({}%), n_p:{}, n_f:{})",
            param.lambda_sec,
            param.lambda_rel,
            param.set_card,
            pc,
            param.set_size_per,
            param.lower_bound_per
        ),
    )
}

/// Helper function to create series of benchmarks
#[allow(clippy::too_many_arguments)]
pub fn benchmarks<I, V, T: Measurement<Intermediate = I, Value = V>>(
    c: &mut Criterion<T>,
    params: &[BenchParam],
    group_name: String,
    bench_name: &str,
    f: &dyn Fn(&BenchParam, u64, u64) -> V,
) {
    let mut group = c.benchmark_group(group_name);

    for param in params {
        // Benchmark where the prover only has access to np percent elements of Sp,
        // i.e. the minimum number of elements such that the soundness is lower than 2^-λ
        let low = param
            .set_card
            .saturating_mul(param.set_size_per)
            .div_ceil(100);
        group.bench_function(bench_id(bench_name, param.set_size_per, param), move |b| {
            b.iter_custom(|n| f(param, low, n));
        });

        // Benchmark where the prover only has access to (np+100)/2 percent elements of Sp
        let mean = param.set_size_per.saturating_add(100).div_ceil(2);
        let mid: u64 = param.set_card.saturating_add(low).div_ceil(2);
        group.bench_function(bench_id(bench_name, mean, param), move |b| {
            b.iter_custom(|n| f(param, mid, n));
        });

        // Benchmark where the prover only has access to all elements of Sp
        group.bench_function(bench_id(bench_name, 100, param), move |b| {
            b.iter_custom(|n| f(param, param.set_card, n));
        });
    }

    group.finish();
}

// Measurements

/// Structure to count the number of DFS calls per proof
#[derive(Debug, Clone, Copy)]
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
        v1.saturating_add(*v2)
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

#[derive(Debug, Clone, Copy)]
struct StepsFormatter;

impl ValueFormatter for StepsFormatter {
    fn format_value(&self, value: f64) -> String {
        format!("{value:.4} steps")
    }

    fn format_throughput(&self, throughput: &Throughput, value: f64) -> String {
        match throughput {
            Throughput::Bytes(b) => format!("{:.4} spb", value / *b as f64),
            Throughput::Elements(b) => format!("{value:.4} steps/{b}"),
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
