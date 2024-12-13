use super::algorithm;
use super::params::Params;
use super::proof::Proof;
use crate::utils::types::Element;

/// The main simple lottery struct with prove and verify functions.
#[derive(Debug, Clone, Copy)]
pub struct Lottery {
    params: Params,
}

impl Lottery {
    /// Initialize ALBA with `Params`.
    pub fn create(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
    ) -> Self {
        let params = Params::new(soundness_param, completeness_param, set_size, lower_bound);
        Self::create_unsafe(&params)
    }

    /// This function is unsafe to use and should be avoided.
    /// Initialize ALBA with `Params`.
    pub fn create_unsafe(params: &Params) -> Self {
        Self { params: *params }
    }

    /// Returns either a `Proof` or `None` if no proof is found.
    pub fn prove(&self, prover_set: &[Element]) -> Option<Proof> {
        algorithm::prove(&self.params, prover_set)
    }

    /// Returns true if and only if the proof is successfully verified.
    pub fn verify(&self, proof: &Proof) -> bool {
        algorithm::verify(&self.params, proof)
    }
}
