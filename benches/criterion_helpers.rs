use criterion::{
    measurement::{Measurement, ValueFormatter},
    BenchmarkId, Throughput,
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
        format!("Security parameter: {l}, Sp:{sp} ({pc}%), n_p:{np}, n_f:{nf}"),
    )
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
