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
    /// Setup algorithm taking as input the security parameters, the set size,
    /// and the lower bound and returns LotterySetup.
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

    // Setup algorithm taking as input the security parameters, the set size,
    /// and the lower bound and returns the minimum number of participants
    pub fn min_mu(lambda_sec: f64, lambda_rel: f64, n_p: u64, n_f: u64) -> u64 {
        Self::compute_u_mu(lambda_sec, lambda_rel, n_p, n_f).1
    }

    /// Helper algorithm taking as input the security parameters, the set size,
    /// the lower bound and the probability of winning the lottery and returns
    /// the minimum proof sizeif the parameters are in bounds, None otherwise.
    fn compute_u(lambda_sec: f64, lambda_rel: f64, n_p: u64, n_f: u64, p: f64) -> Option<u64> {
        let n_p_f64 = n_p as f64;
        let n_f_f64 = n_f as f64;
        let np_nf_ratio = n_p_f64 / n_f_f64;

        // Finding parameters to minimize the security bounds
        let (mut rs, mut rc, mut bound_u, _) =
            Self::minimize_bounds(lambda_sec, lambda_rel, n_p_f64, n_f_f64);

        // Finding minimal u such that u > bound_u
        let mut u = (rs * p * n_f_f64).ceil();
        while u < bound_u && rc > 1.0 && rs > 1.0 {
            // TODO : make step dynamic, according to difference between u and bound_u
            let step = 1.0 / 10.0;
            rs += step;
            rc = np_nf_ratio / rs;
            let (bound_sec, bound_rel) = Self::compute_bounds(lambda_sec, lambda_rel, rs, rc);
            bound_u = bound_sec.max(bound_rel);
            u = (rs * p * n_f_f64).ceil();
        }
        if u >= bound_u && rc > 1.0 && rs > 1.0 && p * n_p_f64 > bound_u * rc {
            return Some(u as u64);
        }
        None
    }

    /// Helper algorithm taking as input the security parameters, the set size,
    /// the lower bound and returns the minimum proof size and number of
    /// participants.
    fn compute_u_mu(lambda_sec: f64, lambda_rel: f64, n_p: u64, n_f: u64) -> (u64, u64) {
        let (_, _, bound_u, bound_mu) =
            Self::minimize_bounds(lambda_sec, lambda_rel, n_p as f64, n_f as f64);
        (bound_u.ceil() as u64, bound_mu.ceil() as u64)
    }

    // Compute soundness and completeness bounds
    fn compute_bounds(lambda_sec: f64, lambda_rel: f64, rs: f64, rc: f64) -> (f64, f64) {
        let ln2 = 2f64.ln();

        // bound_{λ_sec}(r_s) = ln(2) · λ_sec / ( ln(r_s) - 1 + 1/r_s )
        let lhs = (lambda_sec * ln2) / (rs.ln() - 1.0 + rs.recip());

        // bound_{λ_rel}(r_c) = ln(2) · λ_rel / ( r_c - 1 - ln(r_c) )
        let rhs = (lambda_rel * ln2) / (rc - 1.0 - rc.ln());

        (lhs, rhs)
    }

    // Compute minimal bound for u by converging completeness and soundness bounds
    fn minimize_bounds(
        lambda_sec: f64,
        lambda_rel: f64,
        n_p: f64,
        n_f: f64,
    ) -> (f64, f64, f64, f64) {
        let np_nf_ratio = n_p / n_f;
        let ln_np_nf = np_nf_ratio.ln();
        let diff = n_p - n_f;
        let mut rc = (n_p / diff) * ln_np_nf;
        let mut rs = (diff / n_f) * ln_np_nf.recip();
        let (b_sec, b_rel) = Self::compute_bounds(lambda_sec, lambda_rel, rs, rc);
        let mut bound_sec = b_sec;
        let mut bound_rel = b_rel;

        // Minimizing the overall bound
        while bound_sec.ceil().ne(&bound_rel.ceil()) && rs > 1.0 && rc > 1.0 {
            // TODO : make step dynamic, according to difference between bound_sec and bound_rel
            let step = 1.0 / 100.0;
            if bound_sec > bound_rel {
                rc -= step;
                rs = np_nf_ratio / rc;
            } else {
                rs -= step;
                rc = np_nf_ratio / rs;
            }
            let (b_sec, b_rel) = Self::compute_bounds(lambda_sec, lambda_rel, rs, rc);
            bound_sec = b_sec;
            bound_rel = b_rel;
        }

        let bound_u = bound_sec.max(bound_rel);
        (rs, rc, bound_u, bound_u * rc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::Rng;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    const SP: [u64; 2] = [100_000, 1_000_000];
    const NFNP_PERCENT: [(u64, u64); 5] = [(51, 75), (67, 75), (67, 90), (75, 90), (75, 95)];

    #[test]
    fn test_u_same_lambdas() {
        let nb_tests = 100;
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        for sp in SP {
            for (nf, np) in NFNP_PERCENT {
                for _ in 0..nb_tests {
                    let n_f = (sp * nf).div_ceil(100);
                    let n_p = (sp * np).div_ceil(100);
                    let lambda = rng.gen_range(1..=128) as f64;
                    let min_mu = LotterySetup::min_mu(lambda, lambda, n_p, n_f);
                    let mu = if min_mu < n_p {
                        rng.gen_range(min_mu..n_p)
                    } else {
                        n_p
                    };
                    let setup = LotterySetup::from(lambda, lambda, n_p, n_f, mu);
                    // println!("{}", setup.unwrap().u)
                    assert!(setup.is_some())
                }
            }
        }
    }

    #[test]
    fn test_u_different_lambdas() {
        let nb_tests = 100;
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        for sp in SP {
            for (nf, np) in NFNP_PERCENT {
                for _ in 0..nb_tests {
                    let n_f = (sp * nf).div_ceil(100);
                    let n_p = (sp * np).div_ceil(100);
                    let lambda1 = rng.gen_range(1..=128) as f64;
                    let lambda2 = rng.gen_range(1..=128) as f64;
                    let min_mu = LotterySetup::min_mu(lambda1, lambda2, n_p, n_f);
                    let mu = if min_mu < n_p {
                        rng.gen_range(min_mu..n_p)
                    } else {
                        n_p
                    };
                    let setup = LotterySetup::from(lambda1, lambda2, n_p, n_f, mu);
                    // println!("{}", setup.unwrap().u)
                    assert!(setup.is_some())
                }
            }
        }
    }
}
