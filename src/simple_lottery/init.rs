//! Parameter derivation for simple lottery.

use super::params::Params;
use super::setup::Setup;

use std::f64::consts::LN_2;

/// Calculates Setup parameters.
pub fn make_setup(params: &Params) -> Setup {
    let ratio = params.set_size as f64 / params.lower_bound as f64;

    // Execute binary search for the optimal value of `rs` in the interval
    // ]1.0, ratio[.

    let mut left: f64 = 1.0;
    let mut right: f64 = ratio;

    loop {
        let middle = (left + right) / 2.0;
        let ratio_soundness = middle; // rs
        let ratio_completeness = ratio / ratio_soundness; // rc

        let lhs = params.soundness_param / (ratio_soundness.ln() - 1.0 + 1.0 / ratio_soundness);
        let rhs = params.completeness_param / (ratio_completeness - 1.0 - ratio_completeness.ln());

        if (middle <= left) || (middle >= right) {
            let u = (lhs.max(rhs) * LN_2).ceil();
            let mu = u * ratio_completeness;
            return Setup {
                proof_size: u as u64,
                lottery_probability: mu / params.set_size as f64,
            };
        }

        if lhs > rhs {
            left = middle;
        } else {
            right = middle;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::params::Params;
    use super::make_setup;
    use test_case::test_case;

    #[derive(Debug, Clone, Copy)]
    struct Expected {
        u: u64,     // proof size
        mu_lb: f64, // average communication lower bound
        mu_ub: f64, // average communication upper bound
    }

    #[test_case(
        Params {
            soundness_param: 128.0,
            completeness_param: 128.0,
            set_size: 200,
            lower_bound: 100,
        },
        Expected {
            u: 1488,
            mu_lb: 2062.806,
            mu_ub: 2062.807,
        };
        "basic"
    )]
    #[test_case(
        Params {
            soundness_param: 128.0,
            completeness_param: 64.0,
            set_size: 200,
            lower_bound: 100,
        },
        Expected {
            u: 1127,
            mu_lb: 1473.574,
            mu_ub: 1473.575,
        };
        "different_lambdas"
    )]
    #[test_case(
        Params {
            soundness_param: 128.0,
            completeness_param: 128.0,
            set_size: 150,
            lower_bound: 100,
        },
        Expected {
            u: 4328,
            mu_lb: 5264.558,
            mu_ub: 5264.559,
        };
        "fractional_ratio"
    )]
    #[test_case(
        Params {
            soundness_param: 128.0,
            completeness_param: 1.0,
            set_size: 200,
            lower_bound: 100,
        },
        Expected {
            u: 527,
            mu_lb: 554.495,
            mu_ub: 554.496,
        };
        "small_lambda_reliability"
    )]
    #[test_case(
        Params {
            soundness_param: 1.0,
            completeness_param: 128.0,
            set_size: 200,
            lower_bound: 100,
        },
        Expected {
            u: 358,
            mu_lb: 672.362,
            mu_ub: 672.363,
        };
        "small_lambda_security"
    )]
    fn all(params: Params, expected: Expected) {
        let setup = make_setup(&params);
        assert_eq!(expected.u, setup.proof_size);
        let mu = setup.lottery_probability * params.set_size as f64;
        assert!(mu > expected.mu_lb);
        assert!(mu < expected.mu_ub);
    }
}
