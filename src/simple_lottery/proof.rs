//! Simple Lottery's Proof structure
use super::params::Params;
use crate::utils::{sample, types::Hash};
use blake2::{Blake2s256, Digest};

/// Simple lottery proof
#[derive(Debug, Clone)]
pub struct Proof<E: AsRef<[u8]> + Clone + Ord> {
    /// Sequence of elements from prover's set
    pub element_sequence: Vec<E>,
}

impl<E: AsRef<[u8]> + Clone + Ord> Proof<E> {
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
    pub fn new(params: &Params, prover_set: &[E]) -> Option<Self> {
        debug_assert!(crate::utils::misc::check_distinct(prover_set));

        let mut element_sequence = Vec::with_capacity(params.proof_size as usize);
        for element in prover_set {
            if Proof::lottery_hash(params.lottery_probability, element) {
                element_sequence.push(element.clone());
            }
            if element_sequence.len() as u64 >= params.proof_size {
                element_sequence.sort_unstable();
                return Some(Proof { element_sequence });
            }
        }
        None
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
    /// let b = proof.verify(&params);
    /// assert!(b);
    /// ```
    pub fn verify(&self, params: &Params) -> bool {
        (self.element_sequence.len() as u64 == params.proof_size)
            && self.element_sequence.is_sorted_by(|a, b| a < b)
            && self
                .element_sequence
                .iter()
                .all(|element| Self::lottery_hash(params.lottery_probability, element))
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0
    /// otherwise
    fn lottery_hash(lottery_probability: f64, element: &E) -> bool {
        let mut hasher = Blake2s256::new();
        hasher.update(element.as_ref());
        let digest: Hash = hasher.finalize().into();
        sample::sample_bernoulli(&digest, lottery_probability)
    }
}
