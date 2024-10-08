use criterion::{
    measurement::{Measurement, ValueFormatter},
    BenchmarkId, Throughput,
};

use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use caledonia::{
    utils::{gen_items, gen_weighted_items},
    weighted_decentralised::VerifiableData,
};
use vrf_dalek::vrf::{PublicKey, SecretKey};

pub fn setup_bounded_wrapper(
    rng: &mut ChaCha20Rng,
    l: usize,
    sp: usize,
    np: usize,
) -> (Vec<[u8; 32]>, caledonia::bounded::Setup) {
    use caledonia::bounded::*;
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

pub fn setup_decentralised_wrapper(
    rng: &mut ChaCha20Rng,
    l: usize,
    sp: usize,
    np: usize,
) -> (Vec<[u8; 32]>, caledonia::decentralised::Setup) {
    use caledonia::decentralised::*;
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let mu = Params::min_mu(
        l,
        l,
        (np * sp).div_ceil(100),
        ((100 - np) * sp).div_ceil(100),
    );
    let params = Params::new(
        l,
        l,
        (np * sp).div_ceil(100),
        ((100 - np) * sp).div_ceil(100),
        mu,
    );
    let dataset = gen_items(seed, sp)
        .iter()
        .filter_map(|&s| Proof::lottery(params.n_p, params.mu, s).then(|| s))
        .collect();
    (dataset, Setup::new(&params))
}

pub fn setup_weighted_wrapper(
    rng: &mut ChaCha20Rng,
    l: usize,
    sp: usize,
    voters: usize,
    np: usize,
) -> (
    Vec<VerifiableData>,
    caledonia::weighted_decentralised::Setup,
) {
    use caledonia::weighted_decentralised::*;
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset = gen_weighted_items(seed, sp, voters);
    let params = Params::new(
        l,
        l,
        (np * sp).div_ceil(100),
        ((100 - np) * sp).div_ceil(100),
    );
    let setup = Setup::new(&params);

    let mut verifiable_set = Vec::new();
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    for spi in dataset {
        let ski = SecretKey::generate(&mut rng);
        let pki = PublicKey::from(&ski);
        let (data, stake) = spi;
        let votes = Proof::prove_lottery(setup.n_p_lottery, setup.mu, data, &ski, &pki, stake);
        for v in votes {
            verifiable_set.push(v);
        }
    }

    let params = Params::new(
        l,
        l,
        (np * sp).div_ceil(100),
        ((100 - np) * sp).div_ceil(100),
    );
    (verifiable_set, Setup::new(&params))
}

pub fn bench_id(bench_name: &str, pc: usize, l: usize, sp: usize, np: usize) -> BenchmarkId {
    BenchmarkId::new(
        bench_name,
        format!("Security parameter: {l}, Sp:{sp} ({pc}%), n_p:{np}"),
    )
}

pub fn bench_id_users(
    bench_name: &str,
    pc: usize,
    l: usize,
    sp: usize,
    users: usize,
    np: usize,
) -> BenchmarkId {
    BenchmarkId::new(
        bench_name,
        format!("Security parameter: {l}, Sp:{sp} (users {users}, {pc}%), n_p:{np}"),
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
