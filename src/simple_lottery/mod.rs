//! Simple lottery construction for the Telescope ALBA scheme.
//! Covering the Section 4.1 of the paper.

pub mod params;

pub mod proof;

mod lottery;

// Re-exports
pub use lottery::Lottery;
