//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

// Prove and Verify functions
pub mod algorithm;

pub(crate) mod params;

pub(crate) mod proof;

pub(crate) mod round;

pub(crate) mod setup;

pub(crate) mod types;
