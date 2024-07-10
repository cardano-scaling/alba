use criterion::{
    measurement::{Measurement, ValueFormatter},
    BenchmarkId, Throughput,
};
use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;

use caledonia::bounded::{Params, Setup};
use caledonia::utils::gen_items;

// Helper functions
pub fn setup_wrapper(
    rng: &mut ChaCha20Rng,
    l: usize,
    sp: usize,
    np: usize,
) -> (Vec<[u8; 32]>, Setup) {
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

pub fn bench_id(bench_name: &str, pc: usize, l: usize, sp: usize, np: usize) -> BenchmarkId {
    BenchmarkId::new(
        bench_name,
        format!("Security parameter: {l}, Sp:{sp} ({pc}%), n_p:{np}"),
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
