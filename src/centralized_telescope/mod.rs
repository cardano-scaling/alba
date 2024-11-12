//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

mod algorithm;

mod init;

pub mod proof;

mod round;

pub mod params;

pub mod telescope;

mod types;
