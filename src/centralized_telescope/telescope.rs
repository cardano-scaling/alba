//! Customer facing Centralized Telescope structure
use super::params::Params;
use super::proof::Proof;
use crate::utils::types::Element;

/// The main centralized Telescope struct with prove and verify functions.
#[derive(Debug, Clone, Copy)]
pub struct Telescope {
    /// Approximate size of the prover set to lower bound
    set_size: u64,
    /// Internal parameters
    params: Params,
}

impl Telescope {
    /// Initialize ALBA with `Params`.
    pub fn create(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
    ) -> Self {
        let params = Params::new(soundness_param, completeness_param, set_size, lower_bound);
        Self { set_size, params }
    }

    /// Initialize ALBA with `set_size` and unchecked `Setup`.
    /// Use with caution, in tests or with trusted parameters.
    pub fn setup_unsafe(set_size: u64, params: &Params) -> Self {
        Self {
            set_size,
            params: *params,
        }
    }

    /// Alba's proving algorithm, based on a depth-first search algorithm.
    /// Returns either a `Proof` or `None` if no proof is found.
    pub fn prove(&self, prover_set: &[Element]) -> Option<Proof> {
        Proof::new(self.set_size, &self.params, prover_set)
    }

    /// Alba's verification algorithm.
    /// Returns true if and only if the proof is successfully verified.
    pub fn verify(&self, proof: &Proof) -> bool {
        proof.verify(self.set_size, &self.params)
    }
}
