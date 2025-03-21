//! Customer facing Lottery structure
use super::params::Params;
use super::proof::Proof;
use crate::utils::types::Element;
use digest::{Digest, FixedOutput};

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

    /// Returns the `Params` structure from the `Lottery` structure
    ///
    /// # Arguments
    ///
    /// * `self` - the current `Lottery` structure
    ///
    /// # Returns
    ///
    /// A `Params` structure
    ///
    /// # Example
    ///
    /// ```
    /// use alba::simple_lottery::Lottery;
    /// use alba::simple_lottery::params::Params;
    /// let lottery = Lottery::create(128.0, 128.0, 1_000, 750);
    /// let params = lottery.get_params();
    /// ```
    pub fn get_params(&self) -> Params {
        self.params
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
    /// use sha2::Sha256;
    /// use alba::utils::types::Element;
    /// let set_size = 200;
    /// let lottery = Lottery::create(64.0, 64.0, set_size, 100);
    /// let mut prover_set: Vec<Element<[u8; 48]>> = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push(Element{ data: [(i % 256) as u8 ; 48], index: Some(i)});
    /// }
    /// let proof = lottery.prove::<[u8;48], Sha256>(&prover_set).unwrap();
    /// ```
    pub fn prove<E: AsRef<[u8]> + Clone, H: Digest + FixedOutput>(
        &self,
        prover_set: &[Element<E>],
    ) -> Option<Proof<E, H>> {
        Proof::<E, H>::new(&self.params, prover_set)
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
    /// use sha2::Sha256;
    /// use alba::utils::types::Element;
    /// let set_size = 200;
    /// let lottery = Lottery::create(64.0, 64.0, set_size, 100);
    /// let mut prover_set: Vec<Element<[u8; 48]>> = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push(Element{ data: [(i % 256) as u8 ; 48], index: Some(i)});
    /// }
    /// let proof = lottery.prove::<[u8;48], Sha256>(&prover_set).unwrap();
    /// assert!(lottery.verify::<[u8;48], Sha256>(&proof));
    /// ```
    pub fn verify<E: AsRef<[u8]> + Clone, H: Digest + FixedOutput>(
        &self,
        proof: &Proof<E, H>,
    ) -> bool {
        proof.verify(&self.params)
    }
}
