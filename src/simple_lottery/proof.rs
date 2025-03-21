//! Simple Lottery's Proof structure
use super::params::Params;
use crate::utils::types::Element;
use crate::utils::{sample, types::truncate};
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
    ///     prover_set.push(Element{ data: [(i % 256) as u8 ; 48], index: Some(i)});
    /// }
    /// let proof = Proof::<[u8;48], Sha256>::new(&params, &prover_set).unwrap();
    /// ```
    pub fn new(params: &Params, prover_set: &[Element<E>]) -> Option<Self> {
        debug_assert!(crate::utils::misc::check_distinct(prover_set));

        let mut element_sequence = Vec::with_capacity(params.proof_size as usize);
        for element in prover_set {
            if Self::lottery_hash(params.lottery_probability, element) {
                element_sequence.push(element.clone());
            }
            if element_sequence.len() as u64 >= params.proof_size {
                element_sequence
                    .sort_unstable_by(|a: &Element<E>, b: &Element<E>| a.as_ref().cmp(b.as_ref()));
                return Some(Self {
                    element_sequence,
                    hasher: PhantomData,
                });
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
    /// use sha2::Sha256;
    /// use alba::utils::types::Element;
    /// let set_size = 200;
    /// let params = Params::new(128.0, 128.0, set_size, 100);
    /// let mut prover_set: Vec<Element<[u8; 48]>> = Vec::new();
    /// for i in 0..set_size {
    ///     prover_set.push(Element{ data: [(i % 256) as u8 ; 48], index: Some(i)});
    /// }
    /// let proof = Proof::<[u8;48], Sha256>::new(&params, &prover_set).unwrap();
    /// let b = proof.verify(&params);
    /// assert!(b);
    /// ```
    pub fn verify(&self, params: &Params) -> bool {
        (self.element_sequence.len() as u64 == params.proof_size)
            && self
                .element_sequence
                .is_sorted_by(|a, b| a.as_ref() < b.as_ref())
            && self
                .element_sequence
                .iter()
                .all(|element| Self::lottery_hash(params.lottery_probability, element))
    }

    /// Oracle defined as Bernoulli(q) returning 1 with probability q and 0
    /// otherwise
    fn lottery_hash(lottery_probability: f64, element: &Element<E>) -> bool {
        let mut hasher = H::new();
        hasher = hasher.chain_update(element.as_ref());
        if let Some(index) = element.index {
            hasher = hasher.chain_update(index.to_be_bytes());
        }
        let digest = hasher.finalize();
        let hash = truncate(digest.as_slice());

        sample::sample_bernoulli(&hash, lottery_probability)
    }
}
