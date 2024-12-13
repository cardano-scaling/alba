//! Simple lottery construction for the Telescope ALBA scheme.
//! Covering the Section 4.1 of the paper.

pub mod init;

pub mod params;

pub mod setup;

pub mod proof;

mod algorithm;

mod wrapper;

// Re-exports
pub use wrapper::Wrapper as SimpleLottery;
