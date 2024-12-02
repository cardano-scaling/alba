// #![deny(missing_docs)]
#![doc = include_str!("../README.md")]

mod utils;

pub mod centralized_telescope;
pub mod simple_lottery;

pub mod docs;

// Silence a rustc warning about unused crate.
use rand_core as _;
