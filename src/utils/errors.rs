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
