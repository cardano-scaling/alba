//! Centralized Telescope's `Params` structure comprising the internal parameters

#![doc = include_str!("../../docs/rustdoc/centralized_telescope/params/overview.md")]

use std::f64::consts::LOG2_E;

#[doc = include_str!("../../docs/rustdoc/centralized_telescope/params/parameters.md")]
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
#[doc = include_str!("../../docs/rustdoc/centralized_telescope/params/setup.md")]
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
        let set_size_f64 = set_size as f64;
        let lower_bound_f64 = lower_bound as f64;

        // u = ceil( (λ_sec + log(λ_rel) + 5 - log(log(e))) / (log(n_p/n_f)) )
        let proof_size_f64 = {
            // numerator = λ_sec + log(λ_rel) + 5 - log(log(e))
            let numerator_constant = 5.0 - LOG2_E.log2();
            let numerator = soundness_param + completeness_param.log2() + numerator_constant;
            // denomitor = log(n_p/n_f)
            let denominator = (set_size_f64 * lower_bound_f64.recip()).log2();
            (numerator / denominator).ceil()
        };

        let (s1, s2) = {
            // ratio = (9 * n_p * log(e)) / (17 * u)^2
            let ratio_factor = 9.0 * LOG2_E / 289.0;
            let ratio = ratio_factor * set_size_f64 / (proof_size_f64 * proof_size_f64);
            // s1 = ratio - 7 ; s2 = ratio - 2
            let s1_constant = 7.0;
            let s2_constant = 2.0;
            (ratio - s1_constant, ratio - s2_constant)
        };

        if s1 < 1.0 || s2 < 1.0 {
            // Small case, i.e. set_size <= λ^2
            Self::param_small_case(completeness_param, proof_size_f64)
        } else {
            let completeness_param2 = completeness_param.min(s2);
            if proof_size_f64 < completeness_param2 {
                // Case 3, Theorem 14, i.e.  set_size >= λ^3
                Self::param_high_case(
                    completeness_param,
                    set_size,
                    proof_size_f64,
                    completeness_param2,
                )
            } else {
                // Case 2, Theorem 13, i.e. λ^2 < set_size < λ^3
                Self::param_mid_case(completeness_param, set_size, proof_size_f64, s1)
            }
        }
    }

    /// Helper function that returns Params, used when set_size <= λ^2
    fn param_small_case(completeness_param: f64, proof_size_f64: f64) -> Params {
        let ln12 = (12f64).ln();

        // r = ceil(λ_rel)
        let max_retries = completeness_param.ceil() as u64;

        // d = ceil(32 ln(12) * u)
        let search_width_factor = 32.0 * ln12;
        let search_width = (search_width_factor * proof_size_f64).ceil();

        // q = (2 ln12) / d
        let proof_proba_factor = 2.0 * ln12;
        let valid_proof_probability = proof_proba_factor * search_width.recip();

        // B = floor((8 * (u+1) * d) / ln12)
        let dfs_factor = 8.0 * ln12.recip();
        let dfs_constant = 1.0;
        let dfs_bound =
            (dfs_factor * (proof_size_f64 + dfs_constant) * search_width).floor() as u64;

        Params {
            proof_size: proof_size_f64 as u64,
            max_retries,
            search_width: search_width as u64,
            valid_proof_probability,
            dfs_bound,
        }
    }

    /// Helper function that returns Params, used when set_size >= λ^3
    fn param_high_case(
        completeness_param: f64,
        set_size: u64,
        proof_size_f64: f64,
        completeness_param2: f64,
    ) -> Params {
        let set_size_f64 = set_size as f64;

        // λ_rel' = λ_{rel,2} + 2
        let lambda_rel2 = completeness_param2 + 2.0;

        // r = ceil(λ_rel / λ_{rel,2})
        let max_retries = (completeness_param * completeness_param2.recip()).ceil() as u64;

        // d = ceil(16 * u * λ_rel' / log(e))
        let search_width_factor = 16.0 * LOG2_E.recip();
        let search_width = (search_width_factor * proof_size_f64 * lambda_rel2).ceil();

        // q = (4 + 2 * λ_{rel,2}) / (d * log(e)) = 2 * λ_rel' / (d * log(e))
        let proof_proba_factor = 2.0 * LOG2_E.recip();
        let valid_proof_probability = proof_proba_factor * lambda_rel2 * search_width.recip();

        // B = floor((λ_rel' + log(u) / λ_rel') * (3 * u * d) / 4 + d + u)
        let dfs_bound_coeff = 0.75;
        let dfs_bound = (dfs_bound_coeff
            * (proof_size_f64 * search_width)
            * ((lambda_rel2 + proof_size_f64.log2()) * lambda_rel2.recip())
            + search_width
            + proof_size_f64)
            .floor() as u64;

        debug_assert!(set_size_f64 >= search_width * search_width * LOG2_E / (9.0 * lambda_rel2));
        Params {
            proof_size: proof_size_f64 as u64,
            max_retries,
            search_width: search_width as u64,
            valid_proof_probability,
            dfs_bound,
        }
    }

    /// Helper function that returns Params, used when λ^2 < set_size < λ^3
    fn param_mid_case(
        completeness_param: f64,
        set_size: u64,
        proof_size_f64: f64,
        s1: f64,
    ) -> Params {
        let set_size_f64 = set_size as f64;

        fn max_vertices_visited(proof_size: f64, l1: f64) -> f64 {
            fn factorial_check(max_vertices: f64, l1: f64) -> bool {
                let bound = (-l1).exp2();
                let mut factor = (max_vertices.ceil() as u64).saturating_add(1);
                let max_vertices_plus_2 = max_vertices + 2.0;
                let exp_1_over_max_vertices = max_vertices.recip().exp();
                let ratio_constant = 14.0;
                let mut ratio = (ratio_constant
                    * max_vertices
                    * max_vertices
                    * max_vertices_plus_2
                    * exp_1_over_max_vertices)
                    / (max_vertices_plus_2 - exp_1_over_max_vertices);
                while factor != 0 {
                    ratio /= factor as f64;
                    if ratio <= bound {
                        return true;
                    }
                    factor = factor.saturating_sub(1);
                }
                false
            }
            let mut max_vertices = proof_size;
            while !factorial_check(max_vertices, l1) {
                max_vertices += 1.0;
            }
            max_vertices
        }

        // λ_{rel,1} = min(λ_rel, s1)
        let completeness_param1 = completeness_param.min(s1);

        // λ_{rel, bar} = (λ_{rel,1} + 7.0) / log(e)
        let lbar_factor = LOG2_E.recip();
        let lbar_constant = 7.0;
        let lbar = lbar_factor * (completeness_param1 + lbar_constant);

        // r = λ_rel / λ_{rel,1}
        let max_retries = (completeness_param * completeness_param1.recip()).ceil() as u64;

        // d = ceil(16 * u * λ_{rel, bar})
        let search_width_factor = 16.0;
        let search_width = (search_width_factor * proof_size_f64 * lbar).ceil();

        // q = 2 * λ_{rel, bar} / d
        let proof_proba_factor = 2.0;
        let valid_proof_probability = proof_proba_factor * lbar * search_width.recip();

        // B = floor(((w * λ_{rel, bar} / d) + 1) * exponential * d * u + d)
        let dfs_bound = {
            // w
            let max_vertices = max_vertices_visited(proof_size_f64, completeness_param1);
            // exponential = exp(2 * u * w * λ_{rel, bar} / np + 7 * u / w)
            let exponential = {
                // first = 2 * u * w * λ_{rel, bar} / np
                let exp_first_factor = 2.0;
                let exp_first =
                    exp_first_factor * proof_size_f64 * max_vertices * lbar * set_size_f64.recip();
                // second = 7 * u / w
                let exp_second_factor = 7.0;
                let exp_second = exp_second_factor * proof_size_f64 * max_vertices.recip();
                (exp_first + exp_second).exp()
            };

            let dfs_first_constant = 1.0;
            let dfs_first = (max_vertices * lbar * search_width.recip() + dfs_first_constant)
                * (exponential * search_width * proof_size_f64);
            (dfs_first + search_width).floor() as u64
        };

        debug_assert!(set_size_f64 >= search_width * search_width / (9.0 * lbar));
        Params {
            proof_size: proof_size_f64 as u64,
            max_retries,
            search_width: search_width as u64,
            valid_proof_probability,
            dfs_bound,
        }
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
