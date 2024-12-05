//! ALBA Simple lottery setup parameters.

/// Params output parameters
#[derive(Debug, Clone, Copy)]
pub struct Params {
    /// Number of prover set's elements
    pub proof_size: u64,
    /// Probability of winning the lottery, i.e. that one's element will be aggregated
    pub lottery_probability: f64,
}
