//! ALBA's Setup structure

/// Setup input parameters
#[derive(Debug, Clone, Copy)]
pub struct Params {
    /// Soundness security parameter
    pub(super) lambda_sec: f64,
    /// Completeness security parameter
    pub(super) lambda_rel: f64,
    /// Approximate size of set Sp to lower bound
    pub(super) n_p: u64,
    /// Target lower bound
    pub(super) n_f: u64,
}
