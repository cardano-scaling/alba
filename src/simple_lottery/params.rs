//! ALBA Simple lottery setup parameters.

/// Setup output parameters
#[derive(Debug, Clone, Copy)]
pub struct Setup {
    /// Number of prover set's elements
    pub proof_size: u64,
    /// Probability of winning the lottery, i.e. that one's element will be aggregated
    pub lottery_probability: f64,
}
