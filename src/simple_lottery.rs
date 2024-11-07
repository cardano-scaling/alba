//! ALBA Telescope with simple lottery construction given in Section 4.1.

use std::f64::consts::LOG2_E;

/// Setup input parameters
#[derive(Debug)]
pub struct Params {
    /// Soundness security parameter
    pub lambda_sec: f64,
    /// Completeness security parameter
    pub lambda_rel: f64,
    /// Approximate size of set Sp to lower bound
    pub n_p: u64,
    /// Target lower bound
    pub n_f: u64,
    /// Expected number of network participants
    pub mu: f64,
}

/// Setup output parameters
#[derive(Debug, Clone)]
pub struct Setup {
    /// Approximate size of set Sp to lower bound
    pub n_p: u64,
    /// Proof size (in Sp elements)
    pub u: u64,
    /// Proof max counter
    pub r: u64,
    /// Proof max 2nd counter
    pub d: u64,
    /// Probability q
    pub q: f64,
    /// Computation bound
    pub b: u64,
    /// Lottery oracle probability
    pub p: f64,
}
impl Setup {
    fn param_small_case(params: &Params, u_f64: f64, p: f64) -> Self {
        let ln12 = (12f64).ln();
        let d = (32.0 * ln12 * u_f64).ceil();
        Self {
            n_p: params.n_p,
            u: u_f64 as u64,
            r: params.lambda_rel as u64,
            d: d as u64,
            q: 2.0 * ln12 / d,
            b: (8.0 * (u_f64 + 1.0) * d / ln12).floor() as u64,
            p,
        }
    }

    fn param_high_case(params: &Params, u_f64: f64, lambda_rel2: f64, p: f64) -> Self {
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
            p,
        }
    }

    fn param_mid_case(params: &Params, u_f64: f64, s1: f64, p: f64) -> Self {
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
            p,
        }
    }

    fn bound_u(lambda_sec: f64, lambda_rel: f64, rs: f64, rc: f64) -> f64 {
        let ln2 = 2f64.ln();
        let lhs = (lambda_sec * ln2) / (rs.ln() - 1.0 + rs.recip());
        let rhs = (lambda_rel * ln2) / (rc - 1.0 - rc.ln());
        lhs.max(rhs)
    }

    fn compute_u(params: &Params, p: f64) -> f64 {
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;
        let np_nf_ratio = n_p_f64 / n_f_f64;
        let mut u = 0.0;
        let mut bound_u = 0.0;
        let mut rc;
        let mut rs;

        if params.lambda_sec == params.lambda_rel {
            let ln_np_nf = np_nf_ratio.ln();

            rc = (n_p_f64 / (n_p_f64 - n_f_f64)) * ln_np_nf;
            rs = ((n_p_f64 - n_f_f64) / n_f_f64) * ln_np_nf.recip();

            u = rs * p * n_f_f64;
            bound_u = Self::bound_u(params.lambda_sec, params.lambda_rel, rs, rc);
        } else {
            rc = np_nf_ratio.sqrt();
            rs = rc;
            if params.lambda_sec > params.lambda_rel {
                while rc > 1.0 {
                    u = rs * p * n_f_f64;
                    bound_u = Self::bound_u(params.lambda_sec, params.lambda_rel, rs, rc);
                    if u >= bound_u {
                        break;
                    }
                    rc -= -0.1;
                    rs = np_nf_ratio / rc;
                }
            }
            if params.lambda_rel > params.lambda_sec {
                while rs > 1.0 {
                    u = rs * p * n_f_f64;
                    bound_u = Self::bound_u(params.lambda_sec, params.lambda_rel, rs, rc);
                    if u >= bound_u {
                        break;
                    }
                    rs -= 0.1;
                    rc = np_nf_ratio / rs;
                }
            }
        }
        let bound_mu = bound_u * rc;
        if u >= bound_u && params.mu >= bound_mu {
            u
        } else {
            0.0
        }
    }

    /// Setup algorithm taking a Params as input and returning setup parameters (u,d,q)
    pub fn new(params: &Params) -> Option<Self> {
        let n_p_f64 = params.n_p as f64;
        // let n_f_f64 = params.n_f as f64;

        let p = params.mu / n_p_f64;

        let u_f64 = Self::compute_u(params, p);

        if u_f64 > 0.0 {
            let ratio = 9.0 * n_p_f64 * LOG2_E / ((17.0 * u_f64).powi(2));
            let s1 = ratio - 7.0;
            let s2 = ratio - 2.0;

            if s1 < 1.0 || s2 < 1.0 {
                // Small case, i.e. n_p <= λ^2
                Some(Self::param_small_case(params, u_f64, p))
            } else {
                let lambda_rel2 = params.lambda_rel.min(s2);
                if u_f64 < lambda_rel2 {
                    // Case 3, Theorem 14, i.e.  n_p >= λ^3
                    Some(Self::param_high_case(params, u_f64, lambda_rel2, p))
                } else {
                    // Case 2, Theorem 13, i.e. λ^3 > n_p > λ^2
                    Some(Self::param_mid_case(params, u_f64, s1, p))
                }
            }
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_mu() {
        let params = Params {
            lambda_sec: 10.0,
            lambda_rel: 10.0,
            n_p: 80,
            n_f: 20,
            mu: 55.0,
        };
        let setup = Setup::new(&params);
        // println!("{}", setup.u)
        assert!(setup.is_some())
    }
}
