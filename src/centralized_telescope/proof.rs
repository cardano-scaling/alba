//! ALBA's Proof structure

use crate::utils::types::Element;

/// Alba proof
#[derive(Debug, Clone)]
pub struct Proof {
    /// Proof counter
    pub v: u64,
    /// Proof 2nd counter
    pub t: u64,
    /// Proof tuple
    pub items: Vec<Element>,
}
