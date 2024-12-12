//! Benchmark parameters
//! The parameters chosen correspond to cases of interest for Cardano's Peras and Leios protocols.

pub mod centralized {
    use super::super::criterion_helpers::centralized::BenchParam;

    /// This case corresponds to the minimum security requirements with optimistic set_size
    const LOW_PARAM: BenchParam = BenchParam {
        lambda_sec: 64.0,
        lambda_rel: 64.0,
        set_card: 1_000,
        set_size_per: 90,
        lower_bound_per: 60,
    };

    /// This case corresponds to medium security requirements with more realistic set_size
    const MID_PARAM: BenchParam = BenchParam {
        lambda_sec: 80.0,
        lambda_rel: 80.0,
        set_card: 1_000,
        set_size_per: 80,
        lower_bound_per: 60,
    };

    /// This case corresponds to medium security requirements with more realistic set_size
    const HIGH_PARAM: BenchParam = BenchParam {
        lambda_sec: 128.0,
        lambda_rel: 128.0,
        set_card: 1_000,
        set_size_per: 80,
        lower_bound_per: 67,
    };

    pub const SHORT_TESTS: &[BenchParam; 2] = &[LOW_PARAM, MID_PARAM];

    pub const ALL_TESTS: &[BenchParam; 3] = &[LOW_PARAM, MID_PARAM, HIGH_PARAM];
}
