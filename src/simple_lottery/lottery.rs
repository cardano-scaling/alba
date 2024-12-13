use super::algorithm;
use super::init::make_setup;
use super::proof::Proof;
use super::setup::Setup;
use crate::utils::types::Element;

/// The main simple lottery struct with prove and verify functions.
#[derive(Debug, Clone, Copy)]
pub struct Lottery {
    setup: Setup,
}

impl Lottery {
    /// Initialize ALBA with `Params`.
    pub fn create(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
    ) -> Self {
        let setup = make_setup(soundness_param, completeness_param, set_size, lower_bound);
        Self::create_unsafe(&setup)
    }

    /// This function is unsafe to use and should be avoided.
    /// Initialize ALBA with `Setup`.
    pub fn create_unsafe(setup: &Setup) -> Self {
        Self { setup: *setup }
    }

    /// Returns either a `Proof` or `None` if no proof is found.
    pub fn prove(&self, prover_set: &[Element]) -> Option<Proof> {
        algorithm::prove(&self.setup, prover_set)
    }

    /// Returns true if and only if the proof is successfully verified.
    pub fn verify(&self, proof: &Proof) -> bool {
        algorithm::verify(&self.setup, proof)
    }
}
