//! Simple Lottery's Proof structure
use super::params::Params;
use crate::utils::{
    errors::{ProofGenerationError, VerificationError},
    sample,
    types::{truncate, Element},
};
use digest::{Digest, FixedOutput};
use std::marker::PhantomData;

/// Simple lottery proof
#[derive(Debug, Clone)]
pub struct Proof<E, H> {
    /// Sequence of elements from prover's set
    pub element_sequence: Vec<Element<E>>,
    // Phantom type to link the tree with its hasher
    hasher: PhantomData<H>,
}

impl<E: AsRef<[u8]> + Clone, H: Digest + FixedOutput> Proof<E, H> {
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
    /// use sha2::Sha256;
    /// use alba::utils::types::Element;
    /// let set_size = 200;
    /// let params = Params::new(128.0, 128.0, set_size, 100);
    /// let mut prover_set: Vec<Element<[u8; 48]>> = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push(Element::new([(i % 256) as u8 ; 48], Some(i)));
    /// }
    /// let proof = Proof::<[u8;48], Sha256>::new(&params, &prover_set).unwrap();
    /// ```
    ///
    /// # Errors
    ///
    /// Returns a `ProofGenerationError`
    pub fn new(params: &Params, prover_set: &[Element<E>]) -> Result<Self, ProofGenerationError> {
        debug_assert!(crate::utils::misc::check_distinct(prover_set));

        if params.proof_size > prover_set.len() as u64 {
            return Err(ProofGenerationError::NotEnoughElements);
        }

        let mut element_sequence = Vec::new();
        for element in prover_set {
            if Self::lottery_hash(params.lottery_probability, element) {
                element_sequence.push(element.clone());
            }
        }

        match Element::sort_elements(&element_sequence) {
            Ok(sorted) => {
                let element_sequence = sorted
                    .into_iter()
                    .take(params.proof_size as usize)
                    .collect();
                Ok(Self {
                    element_sequence,
                    hasher: PhantomData,
                })
            }
            _ => Err(ProofGenerationError::NotFound),
        }
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
    /// use sha2::Sha256;
    /// use alba::utils::types::Element;
    /// let set_size = 200;
    /// let params = Params::new(128.0, 128.0, set_size, 100);
    /// let mut prover_set: Vec<Element<[u8; 48]>> = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push(Element::new([(i % 256) as u8 ; 48], Some(i)));
    /// }
    /// let proof = Proof::<[u8;48], Sha256>::new(&params, &prover_set).unwrap();
    /// assert!(proof.verify(&params).is_ok());
    /// ```
    /// # Errors
    ///
    /// Returns a `VerificationError`
    pub fn verify(&self, params: &Params) -> Result<(), VerificationError> {
        if self.element_sequence.len() as u64 != params.proof_size {
            return Err(VerificationError::IncorrectNumberElements);
        }

        if !Element::<E>::is_sorted(&self.element_sequence) {
            return Err(VerificationError::UnsortedElements);
        }

        if !Element::<E>::is_unique(&self.element_sequence) {
            return Err(VerificationError::RepeatedElements);
        }

        if !self
            .element_sequence
            .iter()
            .all(|element| Self::lottery_hash(params.lottery_probability, element))
        {
            return Err(VerificationError::InvalidProof);
        }
        Ok(())
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0
    /// otherwise
    fn lottery_hash(lottery_probability: f64, element: &Element<E>) -> bool {
        let mut hasher = H::new().chain_update(element.as_ref());
        if let Some(index) = element.index {
            hasher = hasher.chain_update(index.to_be_bytes());
        }
        let digest = hasher.finalize();
        let hash = truncate(digest.as_slice());

        sample::sample_bernoulli(&hash, lottery_probability)
    }
}
