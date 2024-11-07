//! ALBA Simple lottery setup parameters.

/// Setup output parameters
#[derive(Debug, Clone, Copy)]
pub struct Setup {
    /// Proof size (in Sp elements)
    pub u: u64,
    /// Lottery oracle probability
    pub p: f64,
}