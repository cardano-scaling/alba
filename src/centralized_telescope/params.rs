//! Centralized Telescope's `Params` structure comprising the internal parameters

use super::cases::{Case, Cases, High, Mid, Small};
use std::{f64::consts::LOG2_E, ops::Neg};

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
    /// Returns a `Params` structure from user parameters
    ///
    /// # Arguments
    ///
    /// * `soundness_param` - the protocol soundness parameter, typically set at 128
    /// * `completeness_param` - the protocol completeness parameter, typically set at 128
    /// * `set_size` - the size of the prover set to lower bound
    /// * `lower_bound` - the lower bound to prove
    ///
    /// # Returns
    ///
    /// A `Params` structure
    ///
    /// # Example
    ///
    /// ```
    /// use alba::centralized_telescope::params::Params;
    /// let params = Params::new(128.0, 128.0, 1_000, 750);
    /// ```
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

    /// Check if input `Params` structure is secure with respect to user
    /// parameters
    pub(super) fn check_from(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
        params: &Self,
    ) -> bool {
        let proof_size_f64 = params.proof_size as f64;

        match Cases::which(completeness_param, set_size, proof_size_f64 as u64) {
            Cases::Small => {
                let small = Small::new(completeness_param, set_size, proof_size_f64);
                Self::check_case(
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
                Self::check_case(
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
                Self::check_case(
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
        params: &Self,
        case: &impl Case,
    ) -> bool {
        fn completeness_error(u: f64, d: u64, q: f64) -> f64 {
            // TODO: update in case
            (-(q - u * q * q / 2.0) * d as f64).exp()
        }
        fn soundness_error(np: u64, nf: u64, u: f64, d: u64, q: f64) -> f64 {
            (nf as f64 / np as f64).powf(u) * d as f64 * q
        }

        // Checks the proof size given is at least as big as one computed from user parameters
        let proof_size = params.proof_size as f64;
        if Self::proof_size(
            soundness_param,
            completeness_param,
            set_size as f64,
            lower_bound as f64,
        ) > proof_size
        {
            return false;
        }

        // Check that the number of max retries is at least as big as one
        // computed from given user parameters
        if case.max_retries() > params.max_retries {
            return false;
        }

        // Check that the search width is at least as big as the one computed
        // from given user parameters and given proof size
        let search_width = case.search_width(proof_size);
        if search_width > params.search_width {
            return false;
        };

        // Check that the valid proof probability is close enough from the one
        // computed from the given user and internal parameters
        let error = 0.01;
        let valid_proof_probability = case.valid_proof_probability(params.search_width);
        if (valid_proof_probability - params.valid_proof_probability).abs()
            / valid_proof_probability
            > error
        {
            return false;
        }

        // Checking the completeness error is smaller than asked
        if completeness_error(proof_size, search_width, valid_proof_probability)
            .log2()
            .neg()
            < completeness_param
        {
            return false;
        };

        // Checking the soundness error is smaller than asked
        if soundness_error(
            set_size,
            lower_bound,
            proof_size,
            params.search_width,
            params.valid_proof_probability,
        )
        .log2()
        .neg()
            < soundness_param
        {
            return false;
        }

        // Check that the DFS bound is at least as big as the one given by the user and internal parametesr.
        let dfs_bound = case.dfs_bound(proof_size, search_width);
        if dfs_bound > params.dfs_bound {
            return false;
        };

        true
    }

    /// Compute the proof size out of the security parameters, the set size and
    /// the lower bound
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

#[cfg(test)]
mod tests {
    use super::Params;
    use test_case::test_case;

    #[derive(Debug, Clone, Copy)]
    struct Expected {
        u: u64, // proof size
        d: u64, // search width
        r: u64, // max retries
        q: f64, // valid proof probability
        b: u64, // DFS bound
    }

    #[test_case(
            128.0,
            128.0,
            10_000,
            6_666,
        Expected {
            u: 239,
            d: 19_005,
            r: 128,
            q: 0.000_261_500_305_160_536_7,
            b: 14_684_495,
        };
        "small"
    )]
    #[test_case(
            64.0,
            64.0,
            10_000_000,
            6_666_666,
        Expected {
            u: 128,
            d: 38_928,
            r: 4,
            q: 0.000_976_545_833_617_772_9,
            b: 6_178_942_541,
        };
        "mid"
    )]
    #[test_case(
            64.0,
            64.0,
            10_000_000,
            10,
        Expected {
            u: 4,
            d: 2_928,
            r: 1,
            q: 0.031_248_438_467_866_384,
            b: 11_982,
        };
        "high"
    )]
    fn all(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
        expected: Expected,
    ) {
        let params = Params::new(soundness_param, completeness_param, set_size, lower_bound);
        assert_eq!(expected.u, params.proof_size);
        assert_eq!(expected.d, params.search_width);
        assert_eq!(expected.r, params.max_retries);
        assert!((expected.q - params.valid_proof_probability).abs() <= expected.q * 0.001);
        assert_eq!(expected.b, params.dfs_bound);
    }
}
