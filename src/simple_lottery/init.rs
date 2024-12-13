//! Parameter derivation for simple lottery.

use super::setup::Setup;

use std::f64::consts::LN_2;

/// Calculates Setup parameters.
pub fn make_setup(
    soundness_param: f64,
    completeness_param: f64,
    set_size: u64,
    lower_bound: u64,
) -> Setup {
    // The following follows section 4.1 of the ALBA paper. https://eprint.iacr.org/2023/1655

    let ratio = set_size as f64 / lower_bound as f64;

    // Execute binary search for the optimal value of `rs` in the interval
    // ]1.0, ratio[.

    let mut left: f64 = 1.0;
    let mut right: f64 = ratio;

    loop {
        let middle = (left + right) / 2.0;
        let ratio_soundness = middle; // rs
        let ratio_completeness = ratio / ratio_soundness; // rc

        let bound_soundness =
            soundness_param * LN_2 / (ratio_soundness.ln() - 1.0 + 1.0 / ratio_soundness);
        let bound_completeness =
            completeness_param * LN_2 / (ratio_completeness - 1.0 - ratio_completeness.ln());

        if (middle <= left) || (middle >= right) {
            let u = bound_soundness.max(bound_completeness).ceil();
            let mu = u * ratio_completeness;
            return if lower_bound < u as u64 {
                Setup {
                    proof_size: lower_bound.saturating_add(1),
                    lottery_probability: 1.0,
                }
            } else {
                // Since u <= n_f, the lottery probability p = u * r_c / n_p <= n_f / n_p * r_c < 1.
                Setup {
                    proof_size: u as u64,
                    lottery_probability: mu / set_size as f64,
                }
            };
        }

        if bound_soundness > bound_completeness {
            left = middle;
        } else {
            right = middle;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::make_setup;
    use test_case::test_case;

    #[derive(Debug, Clone, Copy)]
    struct Expected {
        u: u64,     // proof size
        mu_lb: f64, // average communication lower bound
        mu_ub: f64, // average communication upper bound
    }

    #[test_case(
            128.0,
            128.0,
            20_000,
            10_000,
        Expected {
            u: 1488,
            mu_lb: 2062.806,
            mu_ub: 2062.807,
        };
        "basic"
    )]
    #[test_case(
            128.0,
            64.0,
            20_000,
            10_000,
        Expected {
            u: 1127,
            mu_lb: 1473.574,
            mu_ub: 1473.575,
        };
        "different_lambdas"
    )]
    #[test_case(
            128.0,
            128.0,
            15_000,
            10_000,
        Expected {
            u: 4328,
            mu_lb: 5264.558,
            mu_ub: 5264.559,
        };
        "fractional_ratio"
    )]
    #[test_case(
            128.0,
            1.0,
            20_000,
            10_000,
        Expected {
            u: 527,
            mu_lb: 554.495,
            mu_ub: 554.496,
        };
        "small_lambda_reliability"
    )]
    #[test_case(
            1.0,
            128.0,
            20_000,
            10_000,
        Expected {
            u: 358,
            mu_lb: 672.362,
            mu_ub: 672.363,
        };
        "small_lambda_security"
    )]
    #[test_case(
            128.0,
            128.0,
            200,
            100,
        Expected {
            u: 101,
            mu_lb: 199.99,
            mu_ub: 200.01,
        };
        "small_dataset"
    )]
    fn all(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
        expected: Expected,
    ) {
        let setup = make_setup(soundness_param, completeness_param, set_size, lower_bound);
        assert_eq!(expected.u, setup.proof_size);
        let mu = setup.lottery_probability * set_size as f64;
        assert!(mu > expected.mu_lb);
        assert!(mu < expected.mu_ub);
    }
}
