//! Benchmarking centralized_telescope's proof size
use alba::centralized_telescope::{init, params::Params};

use blake2 as _;
use criterion as _;
use rand as _;
use rand_chacha as _;
use rand_core as _;

// Benchmark parameters
// We bench every combination of securiy parameters, dataset cardinality, set size and lower bounds
// The parameters chosen correspond to cases of interest for Cardano's Peras and Leios protocols.
const L: &[f64] = &[50.0, 80.0, 128.0]; // Security parameter
const NP: &[u64] = &[80, 90, 95, 98]; // Alba's np parameter, |Sp| >= np
const NF: &[u64] = &[67, 75]; // Alba's nf parameter,  |Sp| >= np > nf

fn proof_bench() {
    println!("Centralized telescope -- proof size");
    println!(
        "{0: <10} | {1: <10} | {2: <10} | {3: <10} | {4: <10}",
        "soundness_param (λ_sec)",
        "completeness_param (λ_rel)",
        "set_size (n_p)",
        "lower_bound (n_f)",
        "proof_size (u)"
    );
    for &lsec in L {
        for &lrel in L {
            for &np in NP {
                for &nf in NF {
                    let params = Params {
                        soundness_param: lsec,
                        completeness_param: lrel,
                        set_size: np,
                        lower_bound: nf,
                    };
                    let setup = init::make_setup(&params);
                    println!(
                        "{0: <10} | {1: <10} | {2: <10} | {3: <10} | {4: <10}",
                        lsec, lrel, np, nf, setup.proof_size
                    );
                }
            }
        }
    }
}

fn main() {
    proof_bench();
}
