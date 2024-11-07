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

    fn compute_u(params: &Params, p: f64) -> Option<u64> {
        let n_p_f64 = params.n_p as f64;
        let n_f_f64 = params.n_f as f64;
        let np_nf_ratio = n_p_f64 / n_f_f64;
        let ln_np_nf = np_nf_ratio.ln();

        let mut rc = (n_p_f64 / (n_p_f64 - n_f_f64)) * ln_np_nf;
        let mut rs = ((n_p_f64 - n_f_f64) / n_f_f64) * ln_np_nf.recip();
        let (bound_sec, bound_rel) = Self::bounds_u(params.lambda_sec, params.lambda_rel, rs, rc);
        let mut bound_rel_rc = bound_rel;
        let mut bound_sec_rs = bound_sec;
        let mut bound_u;
        let mut u;

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
        while u < bound_u && rc > 1.0 && rs > 1.0 {
            // TODO : make step dynamic, according to difference between u and bound_u
            let step = 1.0 / 10.0;
            rs += step;
            rc = np_nf_ratio / rs;
            let (bound_sec, bound_rel) =
                Self::bounds_u(params.lambda_sec, params.lambda_rel, rs, rc);
            bound_u = bound_sec.max(bound_rel);
            u = (rs * p * n_f_f64).ceil();
        }
        if u >= bound_u && rc > 1.0 && rs > 1.0 {
            return Some(u as u64);
        }
        None
    }

    /// Setup algorithm taking a Params as input and returning setup parameters (u,d,q)
    pub fn new(params: &Params) -> Option<Self> {
        let n_p_f64 = params.n_p as f64;
        let p = params.mu / n_p_f64;
        let u_opt = Self::compute_u(params, p);

        u_opt.map(|u| Self { u, p })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    const NFNP: [(u64, u64); 5] = [(51, 75), (67, 75), (67, 90), (75, 90), (75, 95)];

    #[test]
    fn test_u_same_lamndas() {
        let nb_tests = 100;
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        for (n_f, n_p) in NFNP {
            for _ in 0..nb_tests {
                let lambda = rng.next_u64() as f64;
                let params = Params {
                    lambda_sec: lambda,
                    lambda_rel: lambda,
                    n_p,
                    n_f,
                    mu: 55.0,
                };
                let setup = LotterySetup::new(&params);
                // println!("{}", setup.unwrap().u)
                assert!(setup.is_some())
            }
        }
    }

    #[test]
    fn test_u_different_lambdas() {
        let nb_tests = 100;
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        for (n_f, n_p) in NFNP {
            for _ in 0..nb_tests {
                let params = Params {
                    lambda_sec: rng.next_u64() as f64,
                    lambda_rel: rng.next_u64() as f64,
                    n_p,
                    n_f,
                    mu: 55.0,
                };
                let setup = LotterySetup::new(&params);
                // println!("{}", setup.unwrap().u)
                assert!(setup.is_some())
            }
        }
    }
}
