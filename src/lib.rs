//! An implementation of Approximate Lower Bound Arguments
//! (ALBA, <https://eprint.iacr.org/2023/1655.pdf>).
#[cfg(test)]
mod test_utils;

mod utils;

pub mod centralized_telescope;

// Silence a rustc warning about unused crate.
use rand_core as _;
