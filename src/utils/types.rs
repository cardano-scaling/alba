//! Types, traits and their implementation

use std::error::Error;
use std::fmt::{Display, Error as FmtError, Formatter};

pub(crate) const DATA_LENGTH: usize = 48;

/// Type of dataset's elements to lower bound
pub(crate) type Element = [u8; DATA_LENGTH];

/// Digest size for internal hashes
pub(crate) const DIGEST_SIZE: usize = 32;

/// Hash type for internal hashes
pub(crate) type Hash = [u8; DIGEST_SIZE];

/// Proof generation error
#[derive(Debug, Clone, Copy)]
pub enum ProofGenerationError {
    /// Proof cannot be guaranteed to be generated as not enough elements were submitted to the prover
    NotEnoughElements,
    /// No proof was found within bound
    NotFoundInBounds,
    /// No proof was found
    NotFound,
}

impl Display for ProofGenerationError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            Self::NotEnoughElements => write!(f, "Not enough elements given to the prover"),
            Self::NotFoundInBounds => write!(f, "No proof bound within DFS bound"),
            Self::NotFound => write!(f, "No proof found"),
        }
    }
}

impl Error for ProofGenerationError {}

/// Proof verification error
#[derive(Debug, Clone, Copy)]
pub enum VerificationError {
    /// Proof does not contain the correct number of elements
    IncorrectNumberElements,
    /// Proof does not respect the given parameters
    InvalidParameters,
    /// Proof does not contain unique elements (Lottery)
    RepeatedElements,
    /// Proof does not verify successfully
    InvalidProof,
}

impl Display for VerificationError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            Self::IncorrectNumberElements => write!(f, "Incorrect number of elements in the proof"),
            Self::InvalidParameters => write!(f, "Some parameters are not respected"),
            Self::RepeatedElements => write!(f, "Some elements are repeated"),
            Self::InvalidProof => write!(f, "The proof does not verify"),
        }
    }
}

impl Error for VerificationError {}
