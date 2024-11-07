//! ALBA Telescope with simple lottery construction given in Section 4.1.

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

    /// Helper algorithm taking as input the security parameters, the set size,
    /// the lower bound and returns the minimum proof size and number of
    /// participants.
    fn compute_u_mu(lambda_sec: f64, lambda_rel: f64, n_p: u64, n_f: u64) -> (u64, u64) {
        let n_p_f64 = n_p as f64;
        let n_f_f64 = n_f as f64;
        let np_nf_ratio = n_p_f64 / n_f_f64;
        let ln_np_nf = np_nf_ratio.ln();

        let mut rc = (n_p_f64 / (n_p_f64 - n_f_f64)) * ln_np_nf;
        let mut rs = ((n_p_f64 - n_f_f64) / n_f_f64) * ln_np_nf.recip();
        let (bound_sec, bound_rel) = Self::bounds_u(lambda_sec, lambda_rel, rs, rc);
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
            let (bound_sec, bound_rel) = Self::bounds_u(lambda_sec, lambda_rel, rs, rc);

            bound_rel_rc = bound_rel;
            bound_sec_rs = bound_sec;
        }
        let u_bound = bound_sec_rs.max(bound_rel_rc);
        (u_bound.ceil() as u64, (u_bound * rc).ceil() as u64)
    }

    /// Helper algorithm taking as input the security parameters, the set size,
    /// the lower bound and the probability of winning the lottery and returns
    /// the minimum proof sizeif the parameters are in bounds, None otherwise.
    fn compute_u(lambda_sec: f64, lambda_rel: f64, n_p: u64, n_f: u64, p: f64) -> Option<u64> {
        let n_p_f64 = n_p as f64;
        let n_f_f64 = n_f as f64;
        let np_nf_ratio = n_p_f64 / n_f_f64;
        let ln_np_nf = np_nf_ratio.ln();

        let mut rc = (n_p_f64 / (n_p_f64 - n_f_f64)) * ln_np_nf;
        let mut rs = ((n_p_f64 - n_f_f64) / n_f_f64) * ln_np_nf.recip();
        let (bound_sec, bound_rel) = Self::bounds_u(lambda_sec, lambda_rel, rs, rc);
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
            let (bound_sec, bound_rel) = Self::bounds_u(lambda_sec, lambda_rel, rs, rc);

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
            let (bound_sec, bound_rel) = Self::bounds_u(lambda_sec, lambda_rel, rs, rc);
            bound_u = bound_sec.max(bound_rel);
            u = (rs * p * n_f_f64).ceil();
        }
        if u >= bound_u && rc > 1.0 && rs > 1.0 && p * n_p_f64 > bound_u * rc {
            return Some(u as u64);
        }
        None
    }

    /// Setup algorithm taking the security parameters, the set size and the lower
    /// bound and returns LotterySetup.
    pub fn new(lambda_sec: f64, lambda_rel: f64, n_p: u64, n_f: u64) -> Self {
        let (u, mu) = Self::compute_u_mu(lambda_sec, lambda_rel, n_p, n_f);

        Self {
            u,
            p: mu as f64 / n_p as f64,
        }
    }

    /// Setup algorithm taking the security parameters, the set size, the lower
    /// bound and the expected number of participants and returns LotterySetup
    /// if the parameters are in bounds, None otherwise.
    pub fn from(lambda_sec: f64, lambda_rel: f64, n_p: u64, n_f: u64, mu: u64) -> Option<Self> {
        let p = mu as f64 / n_p as f64;
        let u_opt = Self::compute_u(lambda_sec, lambda_rel, n_p, n_f, p);
        u_opt.map(|u| Self { u, p })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::Rng;
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
                let mu = rng.gen_range(n_f..n_p);
                let setup = LotterySetup::from(lambda, lambda, n_p, n_f, mu);
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
                let mu = rng.gen_range(n_f..n_p);
                let setup =
                    LotterySetup::from(rng.next_u64() as f64, rng.next_u64() as f64, n_p, n_f, mu);
                // println!("{}", setup.unwrap().u)
                assert!(setup.is_some())
            }
        }
    }
}
