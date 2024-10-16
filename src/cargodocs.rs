//! Telescope-ALBA documentation

#![doc = include_str!("../cargodocs/intro.md")]

#[doc = include_str!("../cargodocs/overview.md")]
pub mod overview {}

#[doc = include_str!("../cargodocs/centralized.md")]
pub mod centralized {}

#[doc = include_str!("../cargodocs/lottery.md")]
pub mod lottery {}

#[doc = include_str!("../cargodocs/decentralized.md")]
pub mod decentralized {}

#[doc = include_str!("../cargodocs/wdecentralized.md")]
pub mod weighted_decentralized {}
