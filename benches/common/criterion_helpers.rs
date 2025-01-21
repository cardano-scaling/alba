//! Criterion helper functions including new Measurements and wrappers on
//! BenchmarkId and BenchmarkGroup

use criterion::{
    measurement::{Measurement, ValueFormatter},
    BenchmarkId, Criterion, Throughput,
};

pub const SAMPLE_SIZE: usize = 200;

pub const MEASUREMENT_TIME_SEC: u64 = 30;
pub mod centralized {
    use super::{BenchmarkId, Criterion, Measurement};
    /// Benchmark parameters
    #[derive(Debug, Clone, Copy)]
    pub struct BenchParam {
        /// Soundness Security parameter
        pub lambda_sec: f64,
        /// Completeness Security parameter
        pub lambda_rel: f64,
        /// Total number of elements in the dataset.
        /// We will benchmark subsets of the dataset of size variying between
        /// set_size_per percent and 100% of it
        pub total_num_elements: u64,
        /// Alba's set_size (np) parameter in percentage of total number of
        /// elements of the dataset
        pub set_size: u64,
        /// Alba's lower bound (nf) parameter in percentage of total number of
        /// elements of the dataset
        pub lower_bound: u64,
    }

    impl BenchParam {
        /// Helper function creating a Benchmark ID
        pub fn bench_id(&self, bench_name: &str, truncate: u64) -> BenchmarkId {
            let set_size_percentage =
                (self.set_size as f32 / self.total_num_elements as f32 * 100.0).round() as u32;
            let lower_bound_percentage =
                (self.lower_bound as f32 / self.total_num_elements as f32 * 100.0).round() as u32;

            // Percentage of |Sp| sent to prover when generating proof
            let truncate_percentage =
                (truncate as f32 / self.total_num_elements as f32 * 100.0).round() as u32;

            BenchmarkId::new(
                bench_name,
                format!(
                    "Params{{λsec: {}, λrel: {}, n_p: {} ({}%), n_f: {} ({}%), {}% sent}}",
                    self.lambda_sec,
                    self.lambda_rel,
                    self.set_size,
                    set_size_percentage,
                    self.lower_bound,
                    lower_bound_percentage,
                    truncate_percentage
                ),
            )
        }
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
            group.bench_function(param.bench_id(bench_name, param.set_size), move |b| {
                b.iter_custom(|n| f(param, param.set_size, n));
            });

            // Extra benchmarks to show the proving time and number of steps
            // when giving more than set_size_percentage elements to the prover.
            // This can be helpful when deciding whether having smaller proofs
            // or smaller proving time.
            // Uncomment the following lines to run these extra benchmarks.

            // // Benchmark where the prover only has access to (np+100)/2 percent elements of Sp
            // let mid: u64 = param.total_num_elements.saturating_add(param.set_size).div_ceil(2);
            // group.bench_function(param.bench_id(bench_name, mid), move |b| {
            //     b.iter_custom(|n| f(param, mid, n));
            // });

            // // Benchmark where the prover only has access to all elements of Sp
            // group.bench_function(param.bench_id(bench_name, param.total_num_elements), move |b| {
            //     b.iter_custom(|n| f(param, param.total_num_elements, n));
            // });
        }

        group.finish();
    }
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

    // This step should never happen
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
