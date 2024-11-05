//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

// Prove and Verify functions
pub mod algorithm;

// Setup function
pub mod init;

// Structures
pub mod params;

pub mod proof;

pub(crate) mod round;

pub mod setup;

// Types
pub(crate) mod types;
