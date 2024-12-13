//! Centralized Telescope's `Params` structure comprising the internal parameters
use super::cases::{Case, Cases, High, Mid, Small};
use std::f64::consts::LOG2_E;

/// Internal parameters
#[derive(Debug, Clone, Copy)]
pub struct Params {
    /// Number of prover set's elements
    pub proof_size: u64,
    /// Maximum number of retries to find a proof
    pub max_retries: u64,
    /// Maximum number of subtrees to search to find a proof
    pub search_width: u64,
    /// Probability that a tuple of element is a valid proof
    pub valid_proof_probability: f64,
    /// Maximum number of DFS calls permitted to find a proof
    pub dfs_bound: u64,
}

impl Params {
    /// Setup algorithm taking as input the security parameters, the set size
    /// and the lower bound and returning the internal parameters `Params`
    pub fn new(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
    ) -> Self {
        let proof_size_f64 = Self::proof_size(
            soundness_param,
            completeness_param,
            set_size as f64,
            lower_bound as f64,
        );

        match Cases::which(completeness_param, set_size, proof_size_f64 as u64) {
            Cases::Small => Small::new(completeness_param, set_size, proof_size_f64)
                .create_params(proof_size_f64),
            Cases::Mid => {
                Mid::new(completeness_param, set_size, proof_size_f64).create_params(proof_size_f64)
            }
            Cases::High => High::new(completeness_param, set_size, proof_size_f64)
                .create_params(proof_size_f64),
        }
    }

    fn proof_size(
        soundness_param: f64,
        completeness_param: f64,
        set_size: f64,
        lower_bound: f64,
    ) -> f64 {
        let numerator = soundness_param + completeness_param.log2() + 5.0 - LOG2_E.log2();
        let denominator = (set_size / lower_bound).log2();
        (numerator / denominator).ceil()
    }
}
