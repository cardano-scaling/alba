//! Customer facing Centralized Telescope structure
use super::algorithm;
use super::params::Params;
use super::proof::Proof;
use crate::utils::types::Element;

/// The main centralized Telescope struct with prove and verify functions.
#[derive(Debug, Clone, Copy)]
pub struct Telescope {
    /// Soundness security parameter
    soundness_param: f64,
    /// Completeness security parameter
    completeness_param: f64,
    /// Approximate size of the prover set to lower bound
    set_size: u64,
    /// Lower bound to prove on prover set
    lower_bound: u64,
    /// Internal parameters
    params: Params,
}

impl Telescope {
    /// Returns a `Telescope`` structure from input parameters
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
    /// A `Telescope`` structure
    ///
    /// # Example
    ///
    /// ```
    /// use alba::centralized_telescope::telescope::Telescope;
    /// let telescope = Telescope::create(128.0, 128.0, 1_000, 750);
    /// ```
    pub fn create(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
    ) -> Self {
        let params = Params::new(soundness_param, completeness_param, set_size, lower_bound);
        Self {
            soundness_param,
            completeness_param,
            set_size,
            lower_bound,
            params,
        }
    }

    /// Returns a `Telescope` structure from input and internal parameters if
    /// the parameters verifies the paper bounds, otherwise None
    ///
    /// # Arguments
    ///
    /// * `soundness_param` - the protocol soundness parameter, typically set at 128
    /// * `completeness_param` - the protocol completeness parameter, typically set at 128
    /// * `set_size` - the size of the prover set to lower bound
    /// * `lower_bound` - the lower bound to prove
    /// * `params` - some centralized Telescope internal parameters
    ///
    /// # Returns
    ///
    /// A `Telescope` structure
    ///
    /// # Example
    ///
    /// ```
    /// use alba::centralized_telescope::telescope::Telescope;
    /// use alba::centralized_telescope::params::Params;
    /// let params = Params {proof_size : 200, max_retries: 128, search_width: 10, valid_proof_probability: 0.001, dfs_bound: 40_000};
    /// let telescope = Telescope::setup(128.0, 128.0, 1_000, 750, params);
    /// ```
    pub fn setup(
        soundness_param: f64,
        completeness_param: f64,
        set_size: u64,
        lower_bound: u64,
        params: &Params,
    ) -> Option<Self> {
        Params::check_from(
            soundness_param,
            completeness_param,
            set_size,
            lower_bound,
            params,
        )
        .then_some(Self::setup_unsafe(set_size, params))
    }

    /// Use with caution. Returns a `Telescope` structure from input and
    /// internal parameters without checking the consistency between parameters
    ///
    /// # Arguments
    ///
    /// * `soundness_param` - the protocol soundness parameter, typically set at 128
    /// * `completeness_param` - the protocol completeness parameter, typically set at 128
    /// * `set_size` - the size of the prover set to lower bound
    /// * `lower_bound` - the lower bound to prove
    /// * `params` - some centralized Telescope internal parameters
    ///
    /// # Returns
    ///
    /// A `Telescope` structure
    ///
    /// # Example
    ///
    /// ```
    /// use alba::centralized_telescope::telescope::Telescope;
    /// use alba::centralized_telescope::params::Params;
    /// let params = Params {proof_size : 200, max_retries: 128, search_width: 10, valid_proof_probability: 0.001, dfs_bound: 40_000};
    /// let telescope = Telescope::setup_unsafe(1_000, params);
    /// ```
    pub fn setup_unsafe(set_size: u64, params: &Params) -> Self {
        Self {
            soundness_param: 0f64,
            completeness_param: 0f64,
            set_size,
            lower_bound: 0,
            params: *params,
        }
    }

    /// Generates a Centralized Telescope proof.
    ///
    /// # Arguments
    ///
    /// * `self` - the current `Telescope` structure
    /// * `prover_set` - an array of elements to generate an Alba proof on
    ///
    /// # Returns
    ///
    /// A `Proof` if found, `None` otherwise
    ///
    /// # Example
    ///
    /// ```
    /// use alba::centralized_telescope::telescope::Telescope;
    /// let set_size = 200;
    /// let telescope = Telescope::create(128.0, 128.0, set_size, 150);
    /// let mut prover_set = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push([(i % 256) as u8 ;32]);
    /// }
    /// let proof = telescope.prove(&prover_set).unwrap();
    /// ```
    pub fn prove(&self, prover_set: &[Element]) -> Option<Proof> {
        algorithm::prove(self.set_size, &self.params, prover_set)
    }

    /// Verifies a Centralized Telescope proof.
    ///
    /// # Arguments
    ///
    /// * `self` - the current `Telescope` structure
    /// * `proof` - a centralized Telescope proof
    ///
    /// # Returns
    ///
    /// True if the verification is successful, false otherwise
    ///
    /// # Example
    ///
    /// ```
    /// use alba::centralized_telescope::telescope::Telescope;
    /// let set_size = 200;
    /// let telescope = Telescope::create(128.0, 128.0, set_size, 150);
    /// let mut prover_set = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push([(i % 256) as u8 ;32]);
    /// }
    /// let proof = telescope.prove(&prover_set).unwrap();
    /// assert!(telescope.verify(&proof));
    /// ```
    pub fn verify(&self, proof: &Proof) -> bool {
        algorithm::verify(self.set_size, &self.params, proof)
    }
}
