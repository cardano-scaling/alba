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
    fn bounds_u(lambda_sec: f64, lambda_rel: f64, rs: f64, rc: f64) -> (f64, f64) {
        let ln2 = 2f64.ln();
        let lhs = (lambda_sec * ln2) / (rs.ln() - 1.0 + rs.recip());
        let rhs = (lambda_rel * ln2) / (rc - 1.0 - rc.ln());
        (lhs, rhs)
    }

    fn compute_u(params: &Params, p: f64) -> u64 {
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;
        let np_nf_ratio = n_p_f64 / n_f_f64;
        let mut u;
        let mut bound_u;
        let mut rc;
        let mut rs;

        if (params.lambda_sec - params.lambda_rel).abs() < 0.001 {
            let ln_np_nf = np_nf_ratio.ln();

            rc = (n_p_f64 / (n_p_f64 - n_f_f64)) * ln_np_nf;
            rs = ((n_p_f64 - n_f_f64) / n_f_f64) * ln_np_nf.recip();

            u = (rs * p * n_f_f64).ceil();
            let (bound_sec, bound_rel) =
                Self::bounds_u(params.lambda_sec, params.lambda_rel, rs, rc);
            bound_u = bound_sec.max(bound_rel);
        } else {
            rc = np_nf_ratio.sqrt();
            rs = rc;
            let (bound_sec, bound_rel) =
                Self::bounds_u(params.lambda_sec, params.lambda_rel, rs, rc);
            let mut bound_rel_rc = bound_rel;
            let mut bound_sec_rs = bound_sec;
            // Minimizing the overall bound
            while bound_sec_rs.ceil().ne(&bound_rel_rc.ceil()) && rs > 1.0 && rc > 1.0 {
                // TODO : make step dynamic, according to difference between bound_sec_rs and bound_rel_rc
                let step = 1.0 / 100.0;
                if bound_rel_rc > bound_sec_rs {
                    rs -= step;
                    rc = np_nf_ratio / rs;
                } else {
                    rc -= step;
                    rs = np_nf_ratio / rc;
                }
                let (bound_sec, bound_rel) =
                    Self::bounds_u(params.lambda_sec, params.lambda_rel, rs, rc);

                bound_rel_rc = bound_rel;
                bound_sec_rs = bound_sec;
            }
            bound_u = bound_sec_rs.ceil().max(bound_rel_rc.ceil());
            // println!("\n--- Found optimal bound_u:{}", bound_u);
            // Finding minimal u > bound_u
            u = (rs * p * n_f_f64).ceil();
            while u < bound_u {
                // TODO : make step dynamic, according to difference between u and bound_u
                let step = 1.0 / 10.0;
                rs += step;
                rc = np_nf_ratio / rs;
                let (bound_sec, bound_rel) =
                    Self::bounds_u(params.lambda_sec, params.lambda_rel, rs, rc);
                bound_u = bound_sec.max(bound_rel);
                u = (rs * p * n_f_f64).ceil();
            }
        }
        let bound_mu = bound_u * rc;
        if u >= bound_u && params.mu >= bound_mu {
            u as u64
        } else {
            0
        }
    }

    /// Setup algorithm taking a Params as input and returning setup parameters (u,d,q)
    pub fn new(params: &Params) -> Option<Self> {
        let n_p_f64 = params.n_p as f64;
        let p = params.mu / n_p_f64;
        let u = Self::compute_u(params, p);

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
            lambda_rel: 12.0,
            n_p: 80,
            n_f: 20,
            mu: 55.0,
        };
        let setup = LotterySetup::new(&params);
        // println!("{}", setup.unwrap().u)
        assert!(setup.is_some())
    }
}
