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
        Self::setup_unsafe(&params)
    }

    /// Initialize ALBA with `Params`.
    pub fn setup(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
        params: &Params,
    ) -> Option<Self> {
        params
            .check_from(soundness_param, completeness_param, set_size, lower_bound)
            .then_some(Self::setup_unsafe(params))
    }

    /// Use with caution. Returns a `Lottery` structure from unchecked internal
    /// parameters.
    pub fn setup_unsafe(params: &Params) -> Self {
        Self { params: *params }
    }

    /// Returns either a `Proof` or `None` if no proof is found.
    pub fn prove(&self, prover_set: &[Element]) -> Option<Proof> {
        Proof::new(&self.params, prover_set)
    }

    /// Returns true if and only if the proof is successfully verified.
    pub fn verify(&self, proof: &Proof) -> bool {
        proof.verify(&self.params)
    }
}
