//! ALBA Telescope with simple lottery construction given in Section 4.1.

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
pub struct LotterySetup {
    /// Proof size (in Sp elements)
    pub u: u64,
    /// Lottery oracle probability
    pub p: f64,
}

impl LotterySetup {
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
            // println!("u: {}, bound_u: {}", u, bound_u);
        } else {
            rc = np_nf_ratio.sqrt();
            rs = rc;
            if params.lambda_sec > params.lambda_rel {
                while rc > 1.0 {
                    u = rs * p * n_f_f64;
                    bound_u = Self::bound_u(params.lambda_sec, params.lambda_rel, rs, rc);
                    if u.ceil() >= bound_u {
                        // println!("u: {}, bound_u: {}", u, bound_u);
                        break;
                    }
                    rc -= -0.5;
                    rs = np_nf_ratio / rc;
                }
            }
            if params.lambda_rel > params.lambda_sec {
                while rs > 1.0 {
                    u = rs * p * n_f_f64;
                    bound_u = Self::bound_u(params.lambda_sec, params.lambda_rel, rs, rc);
                    if u.ceil() >= bound_u {
                        // println!("u: {}, bound_u: {}", u, bound_u);
                        break;
                    }
                    rs -= 0.01;
                    rc = np_nf_ratio / rs;
                }
            }
        }
        let bound_mu = bound_u * rc;
        if u.ceil() >= bound_u && params.mu >= bound_mu {
            u
        } else {
            0.0
        }
    }

    /// Setup algorithm taking a Params as input and returning setup parameters (u,d,q)
    pub fn new(params: &Params) -> Option<Self> {
        let n_p_f64 = params.n_p as f64;
        let p = params.mu / n_p_f64;
        let u = Self::compute_u(params, p) as u64;

        if u > 0 {
            Some(Self { u, p })
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
        let setup = LotterySetup::new(&params);
        // println!("{}", setup.unwrap().u)
        assert!(setup.is_some())
    }
}
