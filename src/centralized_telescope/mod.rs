//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

#![doc = include_str!("../../docs/rustdoc/centralized_telescope/main.md")]

mod cases;

pub mod proof;

mod round;

pub mod params;

mod telescope;
pub use telescope::Telescope;
