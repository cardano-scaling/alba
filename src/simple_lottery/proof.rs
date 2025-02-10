//! Simple Lottery's Proof structure
use super::params::Params;
use crate::utils::{
    sample,
    types::{Element, Hash, ProofGenerationError, VerificationError},
};
use blake2::{Blake2s256, Digest};

/// Simple lottery proof
#[derive(Debug, Clone)]
pub struct Proof {
    /// Sequence of elements from prover's set
    pub element_sequence: Vec<Element>,
}

impl Proof {
    /// Simple Lottery's proving algorithm, based on a DFS algorithm.
    ///
    /// # Arguments
    ///
    /// * `params` - the internal parameters to generate a proof from
    /// * `prover_set` - the dataset to generate a proof from
    ///
    /// # Returns
    ///
    /// A `Proof` structure
    ///
    /// # Example
    ///
    /// ```
    /// use alba::simple_lottery::params::Params;
    /// use alba::simple_lottery::proof::Proof;
    /// let set_size = 200;
    /// let params = Params::new(128.0, 128.0, set_size, 100);
    /// let mut prover_set = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push([(i % 256) as u8 ;48]);
    /// }
    /// let proof = Proof::new(&params, &prover_set).unwrap();
    /// ```
    ///
    /// # Errors
    ///
    /// Returns a `ProofGenerationError`
    pub fn new(params: &Params, prover_set: &[Element]) -> Result<Self, ProofGenerationError> {
        debug_assert!(crate::utils::misc::check_distinct(prover_set));

        let mut element_sequence = Vec::with_capacity(params.proof_size as usize);
        for &element in prover_set {
            if Proof::lottery_hash(params.lottery_probability, element) {
                element_sequence.push(element);
            }
            if element_sequence.len() as u64 >= params.proof_size {
                element_sequence.sort_unstable();
                return Ok(Proof { element_sequence });
            }
        }
        Err(ProofGenerationError::NotFound)
    }

    /// Simple Telescope's verification algorithm, returns true if the proof is
    /// successfully verified, following the DFS verification, false otherwise.
    ///
    /// # Arguments
    ///
    /// * `self` - the proof to verify
    /// * `params` - the internal parameters to generate a proof from
    ///
    /// # Returns
    ///
    /// A boolean, true if the proof verifies successfully otherwise false
    ///
    /// # Example
    ///
    /// ```
    /// use alba::simple_lottery::params::Params;
    /// use alba::simple_lottery::proof::Proof;
    /// let set_size = 200;
    /// let params = Params::new(128.0, 128.0, set_size, 100);
    /// let mut prover_set = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push([(i % 256) as u8 ;48]);
    /// }
    /// let proof = Proof::new(&params, &prover_set).unwrap();
    /// assert!(proof.verify(&params).is_ok());
    /// ```
    /// # Errors
    ///
    /// Returns a `VerificationError`
    pub fn verify(&self, params: &Params) -> Result<(), VerificationError> {
        if self.element_sequence.len() as u64 != params.proof_size {
            return Err(VerificationError::IncorrectNumberElements);
        }

        if !self.element_sequence.is_sorted_by(|a, b| a < b) {
            return Err(VerificationError::RepeatedElements);
        }
        if self
            .element_sequence
            .iter()
            .all(|&element| Proof::lottery_hash(params.lottery_probability, element))
        {
            Ok(())
        } else {
            Err(VerificationError::InvalidProof)
        }
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0
    /// otherwise
    fn lottery_hash(lottery_probability: f64, element: Element) -> bool {
        let mut hasher = Blake2s256::new();
        hasher.update(element);
        let digest: Hash = hasher.finalize().into();
        sample::sample_bernoulli(&digest, lottery_probability)
    }
}
