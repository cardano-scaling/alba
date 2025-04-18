//! Errors handling

use thiserror::Error;

/// Element error
#[derive(Error, Debug, Copy, Clone)]
pub enum ElementError {
    /// Elements are not unique (Lottery)
    #[error("Some elements are repeated")]
    RepeatedElements,
    /// Elements are not consistent
    #[error("Some elements do not have indices")]
    InconsistentElements,
}

/// Proof generation error
#[derive(Error, Debug, Copy, Clone)]
pub enum ProofGenerationError {
    /// Elements are not consistent
    #[error("Some elements do not have indices")]
    InconsistentElements,
    /// Proof cannot be guaranteed to be generated as not enough elements were submitted to the prover
    #[error("Not enough elements given to the prover")]
    NotEnoughElements,
    /// No proof was found within bound
    #[error("No proof bound within DFS bound")]
    NotFoundInBounds,
    /// No proof was found
    #[error("No proof found")]
    NotFound,
}

/// Proof verification error
#[derive(Error, Debug, Copy, Clone)]
pub enum VerificationError {
    /// Proof does not contain the correct number of elements
    #[error("Incorrect number of elements in the proof")]
    IncorrectNumberElements,
    /// Proof does not respect the given parameters
    #[error("Some parameters are not respected")]
    InvalidParameters,
    /// Proof does not contain unique elements (Lottery)
    #[error("Some elements are repeated")]
    RepeatedElements,
    /// Proof's elements are not sorted (Lottery)
    #[error("Elements are not sorted")]
    UnsortedElements,
    /// Proof's elements are not unique (Lottery)
    #[error("Elements are not unique")]
    UniqueElements,
    /// Proof does not verify successfully
    #[error("The proof does not verify")]
    InvalidProof,
}
