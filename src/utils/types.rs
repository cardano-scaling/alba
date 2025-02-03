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
            Self::NotFoundInBounds => write!(f, "No proof bound within DFs bound"),
            Self::NotFound => write!(f, "No proof found"),
        }
    }
}

impl Error for ProofGenerationError {}

