//! Simple lottery Proof structure

use crate::utils::types::Element;

/// Simple lottery proof
#[derive(Debug, Clone)]
pub struct Proof {
    /// Sequence of elements from prover's set
    pub element_sequence: Vec<Element>,
}
