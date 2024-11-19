use super::algorithm;
use super::proof::Proof;
use super::setup::Setup;
use crate::utils::types::Element;

/// The main ALBA struct with prove and verify functions.
#[derive(Debug, Clone, Copy)]
pub struct Wrapper {
    setup: Setup,
}

impl Wrapper {
    /// Initialize ALBA with `Setup` parameters.
    pub fn new(setup: &Setup) -> Self {
        Self { setup: *setup }
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Returns either a `Proof` or `None` if no proof is found.
    pub fn prove(&self, prover_set: &[Element]) -> Option<Proof> {
        algorithm::prove(&self.setup, prover_set)
    }

    /// Alba's verification algorithm.
    /// Returns true if and only if the proof is successfully verified.
    pub fn verify(&self, proof: &Proof) -> bool {
        algorithm::verify(&self.setup, proof)
    }
}
