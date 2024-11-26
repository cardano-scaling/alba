//! ALBA's Params structure comprising internal parameters
use super::cases::{Case, Cases, High, Mid, Small};
use std::f64::consts::LOG2_E;

/// Params structure comprising internal parameters
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
    /// and the lower bound and returning the internal parameters Params
    pub(super) fn new(
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

    pub(super) fn check_from(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
        params: Params,
    ) -> bool {
        let proof_size_f64 = params.proof_size as f64;

        match Cases::which(completeness_param, set_size, proof_size_f64 as u64) {
            Cases::Small => {
                let small = Small::new(completeness_param, set_size, proof_size_f64);
                Params::check_case(
                    soundness_param,
                    completeness_param,
                    set_size,
                    lower_bound,
                    params,
                    &small,
                )
            }
            Cases::Mid => {
                let mid = Mid::new(completeness_param, set_size, proof_size_f64);
                Params::check_case(
                    soundness_param,
                    completeness_param,
                    set_size,
                    lower_bound,
                    params,
                    &mid,
                )
            }
            Cases::High => {
                let high = High::new(completeness_param, set_size, proof_size_f64);
                Params::check_case(
                    soundness_param,
                    completeness_param,
                    set_size,
                    lower_bound,
                    params,
                    &high,
                )
            }
        }
    }

    /// Check that internal parameters are consistent wit user parameters
    fn check_case(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
        params: Params,
        case: &impl Case,
    ) -> bool {
        fn completeness_error(u: f64, d: u64, q: f64) -> f64 {
            (-(q - u * q * q / 2.0) * d as f64).exp()
        }
        fn soundness_error(np: u64, nf: u64, u: f64, d: u64, q: f64) -> f64 {
            (nf as f64 / np as f64).powf(u) * d as f64 * q
        }
        let mut bool = true;

        // Checks the proof size given is at least as big as one computed from user parameters
        let proof_size = params.proof_size as f64;
        if Params::proof_size(
            soundness_param,
            completeness_param,
            set_size as f64,
            lower_bound as f64,
        ) > proof_size
        {
            bool = false;
        }

        // Check that the number of max retries is at least as big as one
        // computed from given user parameters
        if case.max_retries() > params.max_retries {
            bool = false;
        }

        // Check that the search width is at least as big as the one computed
        // from given user parameters and given proof size
        let search_width = case.search_width(proof_size);
        if search_width > params.search_width {
            bool = false;
        };

        // Check that the valid proof probability is close enough from the one
        // computed from the given user and internal parameters
        let error = 8f64.recip();
        let valid_proof_probability = case.valid_proof_probability(params.search_width);
        // Checking the completness error difference is bounded by the error
        if (completeness_error(proof_size, params.search_width, valid_proof_probability)
            / completeness_error( proof_size, search_width, params.valid_proof_probability))
        .log2() // Computes log2(2^-l1 / 2^-l2) = (-l1) - (-l2)
        .abs() // Computes |l1 - l2|
            > error
        {
            bool = false;
        };
        // Checking the soundness error difference is bounded by the error
        if (soundness_error(
            set_size,
            lower_bound,
            proof_size,
            params.search_width,
            params.valid_proof_probability,
        ) / soundness_error(
            set_size,
            lower_bound,
            proof_size,
            params.search_width,
            valid_proof_probability,
        ))
        .log2()// Computes log2(2^-l1 / 2^-l2) =  (-l1) - (-l2)
        .abs() // Computes |l1 - l2|
            > error
        {
            bool = false;
        }

        // Check that the DFS bound is at least as big as the one given by the user and internal parametesr.
        let dfs_bound = case.dfs_bound(proof_size, search_width);
        if dfs_bound > params.dfs_bound {
            bool = false;
        };

        bool
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
