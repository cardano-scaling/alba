//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

#![doc = include_str!("../../docs/centralized_telescope/main.md")]

pub mod algorithm;

pub mod init;

pub mod params;

pub mod proof;

mod round;

pub mod setup;

mod types;
