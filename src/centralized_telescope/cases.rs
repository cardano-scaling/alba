//! Centralized Telescope's helper structures, traits and functions for
//! generating parameters

use super::params::Params;
use std::f64::consts::LOG2_E;

pub(super) enum Cases {
    Small,
    Mid,
    High,
}

impl Cases {
    /// Returns which parameters' case the user parameters correspond to
    pub(super) fn which(completeness_param: f64, set_size: u64, proof_size: u64) -> Cases {
        let set_size_f64 = set_size as f64;
        let proof_size_f64 = proof_size as f64;
        let ratio = 9.0 * set_size_f64 * LOG2_E / ((17.0 * proof_size_f64).powi(2));
        let s1 = ratio - 7.0;
        let s2 = ratio - 2.0;

        if s1 < 1.0 || s2 < 1.0 {
            // Small case, i.e. set_size <= λ^2
            Cases::Small
        } else {
            let completeness_param2 = completeness_param.min(s2);
            if proof_size_f64 < completeness_param2 {
                // Case 3, Theorem 14, i.e.  set_size >= λ^3
                Cases::High
            } else {
                // Case 2, Theorem 13, i.e. λ^2 < set_size < λ^3
                Cases::Mid
            }
        }
    }
}

/// Trait to compute internal parameters depending on which case we are
pub(super) trait Case {
    /// Trait constructor
    fn new(completeness_param: f64, set_size: u64, proof_size: f64) -> Self;

    /// Returns the maximum number of retries
    fn max_retries(&self) -> u64;

    /// Returns the search width
    fn search_width(&self, proof_size: f64) -> u64;

    /// Returns the valid proof probability
    fn valid_proof_probability(&self, search_width: u64) -> f64;

    /// Returns the DFS bound
    fn dfs_bound(&self, proof_size: f64, search_width: u64) -> u64;

    /// Returns Params
    fn create_params(&self, proof_size: f64) -> Params {
        let search_width = self.search_width(proof_size);
        Params {
            proof_size: proof_size as u64,
            max_retries: self.max_retries(),
            search_width,
            valid_proof_probability: self.valid_proof_probability(search_width),
            dfs_bound: self.dfs_bound(proof_size, search_width),
        }
    }
}

pub(super) struct Small {
    completeness_param: f64,
}

pub(super) struct Mid {
    completeness_param: f64,
    completeness_param1: f64,
    set_size: u64,
}

pub(super) struct High {
    completeness_param: f64,
    completeness_param2: f64,
}

impl Case for Small {
    fn new(completeness_param: f64, _set_size: u64, _proof_size: f64) -> Self {
        Self { completeness_param }
    }

    fn max_retries(&self) -> u64 {
        self.completeness_param as u64
    }

    fn search_width(&self, proof_size: f64) -> u64 {
        (32.0 * (12f64).ln() * proof_size).ceil() as u64
    }

    fn valid_proof_probability(&self, search_width: u64) -> f64 {
        2.0 * 12f64.ln() / search_width as f64
    }

    fn dfs_bound(&self, proof_size: f64, search_width: u64) -> u64 {
        (8.0 * (proof_size + 1.0) * search_width as f64 / (12f64).ln()).ceil() as u64
    }
}

impl Case for High {
    fn new(completeness_param: f64, set_size: u64, proof_size: f64) -> Self {
        let ratio = 9.0 * set_size as f64 * LOG2_E / ((17.0 * proof_size).powi(2));
        let s2 = ratio - 2.0;
        let completeness_param2 = completeness_param.min(s2);

        Self {
            completeness_param,
            completeness_param2,
        }
    }

    fn max_retries(&self) -> u64 {
        (self.completeness_param / self.completeness_param2).ceil() as u64
    }

    fn search_width(&self, proof_size: f64) -> u64 {
        (16.0 * proof_size * (self.completeness_param2 + 2.0) / LOG2_E).ceil() as u64
    }

    fn valid_proof_probability(&self, search_width: u64) -> f64 {
        (4.0 + 2.0 * self.completeness_param2) / (search_width as f64 * LOG2_E)
    }

    fn dfs_bound(&self, proof_size: f64, search_width: u64) -> u64 {
        let search_width_f64 = search_width as f64;
        ((1.0 + proof_size.log2() * (self.completeness_param2 + 2.0).recip())
            * 3.0
            * proof_size
            * search_width_f64
            / 4.0
            + proof_size
            + search_width_f64)
            .floor() as u64
    }
}

impl Mid {
    fn max_vertices_visited(proof_size: f64, l1: f64) -> f64 {
        fn factorial_check(max_v: f64, l1: f64) -> bool {
            let bound = (-l1).exp2();
            let mut factor = (max_v.ceil() as u64).saturating_add(1);
            let max_v_2 = max_v + 2.0;
            let exp_1_over_max_v = max_v.recip().exp();
            let mut ratio =
                (14.0 * max_v * max_v * max_v_2 * exp_1_over_max_v) / (max_v_2 - exp_1_over_max_v);
            while factor != 0 {
                ratio /= factor as f64;
                if ratio <= bound {
                    return true;
                }
                factor = factor.saturating_sub(1);
            }
            false
        }
        let mut max_v = proof_size;
        while !factorial_check(max_v, l1) {
            max_v += 1.0;
        }
        max_v
    }
}

impl Case for Mid {
    fn new(completeness_param: f64, set_size: u64, proof_size: f64) -> Self {
        let ratio = 9.0 * set_size as f64 * LOG2_E / ((17.0 * proof_size).powi(2));
        let s1 = ratio - 7.0;
        let completeness_param1 = completeness_param.min(s1);

        Self {
            completeness_param,
            completeness_param1,
            set_size,
        }
    }

    fn max_retries(&self) -> u64 {
        (self.completeness_param / self.completeness_param1).ceil() as u64
    }

    fn search_width(&self, proof_size: f64) -> u64 {
        (16.0 * proof_size * (self.completeness_param1 + 7.0) / LOG2_E).ceil() as u64
    }

    fn valid_proof_probability(&self, search_width: u64) -> f64 {
        2.0 * (self.completeness_param1 + 7.0) / (LOG2_E * search_width as f64)
    }

    fn dfs_bound(&self, proof_size: f64, search_width: u64) -> u64 {
        let search_width_f64 = search_width as f64;
        let lbar = (self.completeness_param1 + 7.0) / LOG2_E;
        let max_v = Mid::max_vertices_visited(proof_size, self.completeness_param1);
        let exponential = (2.0 * proof_size * max_v * lbar / self.set_size as f64
            + 7.0 * proof_size / max_v)
            .exp();

        ((max_v * lbar + search_width_f64) * proof_size * exponential + search_width_f64).floor()
            as u64
    }
}
