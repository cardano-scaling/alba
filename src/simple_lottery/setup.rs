//! ALBA Simple lottery setup parameters.

/// Setup output parameters
#[derive(Debug, Clone)]
pub struct LotterySetup {
    /// Proof size (in Sp elements)
    pub u: u64,
    /// Lottery oracle probability
    pub p: f64,
}
