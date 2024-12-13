use super::algorithm;
use super::init::make_setup;
use super::proof::Proof;
use super::setup::Setup;
use crate::utils::types::Element;

/// The main centralized Telescope struct with prove and verify functions.
#[derive(Debug, Clone, Copy)]
pub struct Telescope {
    /// Approximate size of the prover set to lower bound
    set_size: u64,
    /// Internal parameters
    setup: Setup,
}

impl Telescope {
    /// Initialize ALBA with `Params`.
    pub fn create(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
    ) -> Self {
        let setup = make_setup(soundness_param, completeness_param, set_size, lower_bound);
        Self { set_size, setup }
    }

    /// Initialize ALBA with `set_size` and unchecked `Setup`.
    /// Use with caution, in tests or with trusted parameters.
    pub fn setup_unsafe(set_size: u64, setup: &Setup) -> Self {
        Self {
            set_size,
            setup: *setup,
        }
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
