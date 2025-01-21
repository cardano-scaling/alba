//! Customer facing Lottery structure
use super::params::Params;
use super::proof::Proof;
use crate::utils::types::Element;

/// The main simple lottery struct with prove and verify functions.
#[derive(Debug, Clone, Copy)]
pub struct Lottery {
    params: Params,
}

impl Lottery {
    /// Returns a `Lottery` structure from input parameters
    ///
    /// # Arguments
    ///
    /// * `soundness_param` - the protocol soundness parameter, typically set at 128
    /// * `completeness_param` - the protocol completeness parameter, typically set at 128
    /// * `set_size` - the size of the prover set to lower bound
    /// * `lower_bound` - the lower bound to prove
    ///
    /// # Returns
    ///
    /// A `Lottery` structure
    ///
    /// # Example
    ///
    /// ```
    /// use alba::simple_lottery::Lottery;
    /// let lottery = Lottery::create(128.0, 128.0, 1_000, 750);
    /// ```
    pub fn create(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
    ) -> Self {
        let params = Params::new(soundness_param, completeness_param, set_size, lower_bound);
        Self::setup_unsafe(&params)
    }

    /// Use with caution. Returns a `Lottery` structure from internal
    /// parameters without checking
    ///
    /// # Arguments
    ///
    /// * `params` - some Lottery internal parameters
    ///
    /// # Returns
    ///
    /// A `Lottery` structure
    ///
    /// # Example
    ///
    /// ```
    /// use alba::simple_lottery::Lottery;
    /// use alba::simple_lottery::params::Params;
    /// let params = Params {proof_size : 200, lottery_probability: 0.001};
    /// let lottery = Lottery::setup_unsafe(&params);
    /// ```
    pub fn setup_unsafe(params: &Params) -> Self {
        Self { params: *params }
    }

    /// Generates a Lottery proof.
    ///
    /// # Arguments
    ///
    /// * `self` - the current `Lottery` structure
    /// * `prover_set` - an array of elements to generate an Alba proof on
    ///
    /// # Returns
    ///
    /// A `Proof` if found, `None` otherwise
    ///
    /// # Example
    ///
    /// ```
    /// use alba::simple_lottery::Lottery;
    /// let set_size = 200;
    /// let lottery = Lottery::create(64.0, 64.0, set_size, 100);
    /// let mut prover_set = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push([(i % 256) as u8 ;48]);
    /// }
    /// let proof = lottery.prove(&prover_set).unwrap();
    /// ```
    pub fn prove(&self, prover_set: &[Element]) -> Option<Proof> {
        Proof::new(&self.params, prover_set)
    }

    /// Verifies a Lottery proof.
    ///
    /// # Arguments
    ///
    /// * `self` - the current `Lottery` structure
    /// * `proof` - a Lottery proof
    ///
    /// # Returns
    ///
    /// True if the verification is successful, false otherwise
    ///
    /// # Example
    ///
    /// ```
    /// use alba::simple_lottery::Lottery;
    /// let set_size = 200;
    /// let lottery = Lottery::create(64.0, 64.0, set_size, 100);
    /// let mut prover_set = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push([(i % 256) as u8 ;48]);
    /// }
    /// let proof = lottery.prove(&prover_set).unwrap();
    /// assert!(lottery.verify(&proof));
    /// ```
    pub fn verify(&self, proof: &Proof) -> bool {
        proof.verify(&self.params)
    }
}
