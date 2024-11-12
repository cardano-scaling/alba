//! ALBA's bounded DFS scheme setup functions

use super::params::Params;

use std::f64::consts::LOG2_E;

/// Params algorithm taking a Params as input and returning setup parameters (u,d,q)
pub fn setup(
    soundness_param: f64,
    completeness_param: f64,
    set_size: u64,
    lower_bound: u64,
) -> Params {
    let set_size_f64 = set_size as f64;
    let lower_bound_f64 = lower_bound as f64;

    let proof_size_f64 = {
        let numerator = soundness_param + completeness_param.log2() + 5.0 - LOG2_E.log2();
        let denominator = (set_size_f64 / lower_bound_f64).log2();
        (numerator / denominator).ceil()
    };

    let ratio = 9.0 * set_size_f64 * LOG2_E / ((17.0 * proof_size_f64).powi(2));
    let s1 = ratio - 7.0;
    let s2 = ratio - 2.0;

    if s1 < 1.0 || s2 < 1.0 {
        // Small case, i.e. set_size <= λ^2
        param_small_case(completeness_param, proof_size_f64)
    } else {
        let completeness_param2 = completeness_param.min(s2);
        if proof_size_f64 < completeness_param2 {
            // Case 3, Theorem 14, i.e.  set_size >= λ^3
            param_high_case(
                completeness_param,
                set_size,
                proof_size_f64,
                completeness_param2,
            )
        } else {
            // Case 2, Theorem 13, i.e. λ^2 < set_size < λ^3
            param_mid_case(completeness_param, set_size, proof_size_f64, s1)
        }
    }
}

/// Helper function that returns Params, used when set_size <= λ^2
fn param_small_case(completeness_param: f64, proof_size_f64: f64) -> Params {
    let ln12 = (12f64).ln();
    let search_width = (32.0 * ln12 * proof_size_f64).ceil();
    Params {
        proof_size: proof_size_f64 as u64,
        max_retries: completeness_param as u64,
        search_width: search_width as u64,
        valid_proof_probability: 2.0 * ln12 / search_width,
        dfs_bound: (8.0 * (proof_size_f64 + 1.0) * search_width / ln12).floor() as u64,
    }
}

/// Helper function that returns Params, used when set_size >= λ^3
fn param_high_case(
    completeness_param: f64,
    set_size: u64,
    proof_size_f64: f64,
    completeness_param2: f64,
) -> Params {
    let l2 = completeness_param2 + 2.0;
    let search_width = (16.0 * proof_size_f64 * l2 / LOG2_E).ceil();
    debug_assert!(set_size as f64 >= search_width * search_width * LOG2_E / (9.0 * l2));
    Params {
        proof_size: proof_size_f64 as u64,
        max_retries: (completeness_param / completeness_param2).ceil() as u64,
        search_width: search_width as u64,
        valid_proof_probability: 2.0 * l2 / (search_width * LOG2_E),
        dfs_bound: (((l2 + proof_size_f64.log2()) / l2)
            * (3.0 * proof_size_f64 * search_width / 4.0)
            + search_width
            + proof_size_f64)
            .floor() as u64,
    }
}

/// Helper function that returns Params, used when λ^2 < set_size < λ^3
fn param_mid_case(completeness_param: f64, set_size: u64, proof_size_f64: f64, s1: f64) -> Params {
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
    let completeness_param1 = completeness_param.min(s1);
    let lbar = (completeness_param1 + 7.0) / LOG2_E;
    let search_width = (16.0 * proof_size_f64 * lbar).ceil();
    let lbar_over_sw = lbar / search_width;
    debug_assert!(set_size as f64 >= search_width * search_width / (9.0 * lbar));

    let max_v = max_vertices_visited(proof_size_f64, completeness_param1);
    let exponential = (2.0 * proof_size_f64 * max_v * lbar / set_size as f64
        + 7.0 * proof_size_f64 / max_v)
        .exp();
    Params {
        proof_size: proof_size_f64 as u64,
        max_retries: (completeness_param / completeness_param1).ceil() as u64,
        search_width: search_width as u64,
        valid_proof_probability: 2.0 * lbar_over_sw,
        dfs_bound: ((max_v * lbar_over_sw + 1.0) * exponential * search_width * proof_size_f64
            + search_width)
            .floor() as u64,
    }
}
