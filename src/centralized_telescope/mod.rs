//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

#![doc = include_str!("../../docs/rustdoc/centralized_telescope/intro.md")]
//!
//! See full documentation: [Telescope - Construction with Bounded DFS][crate::docs::centralized]
mod cases;

pub mod params;

pub mod proof;

mod round;

mod telescope;
pub use telescope::Telescope;
