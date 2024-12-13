use super::algorithm;
use super::init::make_setup;
use super::params::Params;
use super::proof::Proof;
use super::setup::Setup;
use crate::utils::types::Element;

/// The main centralized Telescope struct with prove and verify functions.
#[derive(Debug, Clone, Copy)]
pub struct Telescope {
    setup: Setup,
}

impl Telescope {
    /// Initialize ALBA with `Params`.
    pub fn create(params: &Params) -> Self {
        let setup = make_setup(params);
        Self::create_unsafe(&setup)
    }

    /// This function is unsafe to use and should be avoided.
    /// Initialize ALBA with `Setup`.
    pub fn create_unsafe(setup: &Setup) -> Self {
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
