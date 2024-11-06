//! ALBA's Setup structure comprising, among others, the number of elements and internal parameters

/// Setup output parameters
#[derive(Debug, Clone, Copy)]
pub struct Setup {
    /// Total number of elements available to the prover
    pub set_size: u64,
    /// Number of prover set's elements
    pub proof_size: u64,
    /// Maximum number of retries to find a proof
    pub max_retries: u64,
    /// Maximum number of subtrees to search to find a proof
    pub search_width: u64,
    /// Probability that a leaf was correctly chosen
    pub leaf_probability: f64,
    /// Maximum number of DFS calls permitted to find a proof
    pub dfs_bound: u64,
}
