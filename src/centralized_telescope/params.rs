//! ALBA's Setup structure

#![doc = include_str!("../../docs/centralized/params.md")]

/// Setup input parameters
#[derive(Debug, Clone, Copy)]
pub struct Params {
    /// Soundness security parameter
    pub soundness_param: f64,
    /// Completeness security parameter
    pub completeness_param: f64,
    /// Approximate size of the prover set to lower bound
    pub set_size: u64,
    /// Lower bound to prove on prover set
    pub lower_bound: u64,
}
