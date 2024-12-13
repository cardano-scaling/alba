//! Centralized Telescope's Params structure comprising the internal parameters

/// Internal parameters
#[derive(Debug, Clone, Copy)]
pub struct Params {
    /// Number of prover set's elements
    pub proof_size: u64,
    /// Maximum number of retries to find a proof
    pub max_retries: u64,
    /// Maximum number of subtrees to search to find a proof
    pub search_width: u64,
    /// Probability that a tuple of element is a valid proof
    pub valid_proof_probability: f64,
    /// Maximum number of DFS calls permitted to find a proof
    pub dfs_bound: u64,
}
