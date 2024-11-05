/// Setup output parameters
#[derive(Debug, Clone, Copy)]
pub struct Setup {
    /// Approximate size of set Sp to lower bound
    pub(super) n_p: u64,
    /// Proof size (in Sp elements)
    pub(super) u: u64,
    /// Proof max counter
    pub(super) r: u64,
    /// Proof max 2nd counter
    pub(super) d: u64,
    /// Probability q
    pub(super) q: f64,
    /// Computation bound
    pub(super) b: u64,
}
