//! Telescope-ALBA documentation

#![doc = include_str!("../docs/intro.md")]

#[doc = include_str!("../docs/overview.md")]
pub mod overview {}

#[doc = include_str!("../docs/centralized/main.md")]
pub mod centralized {
    #[doc = include_str!("../docs/centralized/setup.md")]
    pub mod setup {}
}

#[doc = include_str!("../docs/lottery.md")]
pub mod lottery {}

#[doc = include_str!("../docs/decentralized.md")]
pub mod decentralized {}

#[doc = include_str!("../docs/wdecentralized.md")]
pub mod weighted_decentralized {}

#[doc = include_str!("../docs/variables.md")]
pub mod variables {}
