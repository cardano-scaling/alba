//! ALBA's Proof structure

use crate::utils::types::Element;

/// Alba proof
#[derive(Debug, Clone)]
pub struct Proof {
    /// Numbers of retries done to find the proof
    pub retry_counter: u64,
    /// Index of the searched subtree to find the proof
    pub search_counter: u64,
    /// Sequence of elements from prover set
    pub element_sequence: Vec<Element>,
}
