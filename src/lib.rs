// #![deny(missing_docs)]
#![doc = include_str!("../README.md")]

//! An implementation of Approximate Lower Bound Arguments
//! (ALBA, <https://eprint.iacr.org/2023/1655.pdf>).

mod utils;

pub mod centralized_telescope;
pub mod simple_lottery;

pub mod docs;

// Silence a rustc warning about unused crate.
#[cfg(test)]
use criterion as _;

#[cfg(test)]
use rand as _;
