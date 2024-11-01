use super::params::Params;
use std::f64::consts::LOG2_E;

/// Setup output parameters
#[derive(Debug, Clone, Copy)]
pub struct Setup {
    /// Approximate size of set Sp to lower bound
    pub(super) n_p: u64,
    /// Proof size (in Sp elements)
    pub(super) u: u64,
    /// Proof max counter
    pub(super) r: u64,
    /// Proof max 2nd counter
    pub(super) d: u64,
    /// Probability q
    pub(super) q: f64,
    /// Computation bound
    pub(super) b: u64,
}
impl Setup {
    fn param_small_case(params: &Params, u_f64: f64) -> Self {
        let ln12 = (12f64).ln();
        let d = (32.0 * ln12 * u_f64).ceil();
        Self {
            n_p: params.n_p,
            u: u_f64 as u64,
            r: params.lambda_rel as u64,
            d: d as u64,
            q: 2.0 * ln12 / d,
            b: (8.0 * (u_f64 + 1.0) * d / ln12).floor() as u64,
        }
    }

    fn param_high_case(params: &Params, u_f64: f64, lambda_rel2: f64) -> Self {
        let l2 = lambda_rel2 + 2.0;
        let d = (16.0 * u_f64 * l2 / LOG2_E).ceil();
        debug_assert!(params.n_p as f64 >= d * d * LOG2_E / (9.0 * l2));
        Self {
            n_p: params.n_p,
            u: u_f64 as u64,
            r: (params.lambda_rel / lambda_rel2).ceil() as u64,
            d: d as u64,
            q: 2.0 * l2 / (d * LOG2_E),
            b: (((l2 + u_f64.log2()) / l2) * (3.0 * u_f64 * d / 4.0) + d + u_f64).floor() as u64,
        }
    }

    fn param_mid_case(params: &Params, u_f64: f64, s1: f64) -> Self {
        fn compute_w(u: f64, l: f64) -> f64 {
            fn factorial_check(w: f64, l: f64) -> bool {
                let bound = (-l).exp2();
                let mut factor = (w.ceil() as u64).saturating_add(1);
                let w_2 = w + 2.0;
                let exp_1_over_w = w.recip().exp();
                let mut ratio = (14.0 * w * w * w_2 * exp_1_over_w) / (w_2 - exp_1_over_w);
                while factor != 0 {
                    ratio /= factor as f64;
                    if ratio <= bound {
                        return true;
                    }
                    factor = factor.saturating_sub(1);
                }
                false
            }
            let mut w = u;
            while !factorial_check(w, l) {
                w += 1.0;
            }
            w
        }
        let lambda_rel1 = params.lambda_rel.min(s1);
        let lbar = (lambda_rel1 + 7.0) / LOG2_E;
        let d = (16.0 * u_f64 * lbar).ceil();
        let lbar_over_d = lbar / d;
        debug_assert!(params.n_p as f64 >= d * d / (9.0 * lbar));

        let w = compute_w(u_f64, lambda_rel1);
        let exponential = (2.0 * u_f64 * w * lbar / params.n_p as f64 + 7.0 * u_f64 / w).exp();
        Self {
            n_p: params.n_p,
            u: u_f64 as u64,
            r: (params.lambda_rel / lambda_rel1).ceil() as u64,
            d: d as u64,
            q: 2.0 * lbar_over_d,
            b: ((w * lbar_over_d + 1.0) * exponential * d * u_f64 + d).floor() as u64,
        }
    }

    /// Setup algorithm taking a Params as input and returning setup parameters (u,d,q)
    pub fn new(params: &Params) -> Self {
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;

        let u_f64 = {
            let numerator = params.lambda_sec + params.lambda_rel.log2() + 5.0 - LOG2_E.log2();
            let denominator = (n_p_f64 / n_f_f64).log2();
            (numerator / denominator).ceil()
        };

        let ratio = 9.0 * n_p_f64 * LOG2_E / ((17.0 * u_f64).powi(2));
        let s1 = ratio - 7.0;
        let s2 = ratio - 2.0;

        if s1 < 1.0 || s2 < 1.0 {
            // Small case, i.e. n_p <= λ^2
            Self::param_small_case(params, u_f64)
        } else {
            let lambda_rel2 = params.lambda_rel.min(s2);
            if u_f64 < lambda_rel2 {
                // Case 3, Theorem 14, i.e.  n_p >= λ^3
                Self::param_high_case(params, u_f64, lambda_rel2)
            } else {
                // Case 2, Theorem 13, i.e. λ^3 > n_p > λ^2
                Self::param_mid_case(params, u_f64, s1)
            }
        }
    }
}
