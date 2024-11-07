//! ALBA's Setup structure comprising, among others, the number of elements and internal parameters

/// Setup output parameters
#[derive(Debug, Clone, Copy)]
pub struct Setup {
    /// Approximate size of set Sp to lower bound
    pub n_p: u64,
    /// Proof size (in Sp elements)
    pub u: u64,
    /// Proof max counter
    pub r: u64,
    /// Proof max 2nd counter
    pub d: u64,
    /// Probability q
    pub q: f64,
    /// Computation bound
    pub b: u64,
}
