use crate::utils::types::Element;

#[derive(Debug, Clone)]
/// Alba proof
pub struct Proof {
    /// Proof counter
    pub(super) v: u64,
    /// Proof 2nd counter
    pub(super) t: u64,
    /// Proof tuple
    pub(super) items: Vec<Element>,
}
