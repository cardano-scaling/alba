//! Telescope-ALBA documentation

#![doc = include_str!("../cargodocs/intro.md")]

#[doc = include_str!("../cargodocs/overview.md")]
pub mod Overview{}

#[doc = include_str!("../cargodocs/centralized.md")]
pub mod Centralized{}

#[doc = include_str!("../cargodocs/lottery.md")]
pub mod Lottery{}

#[doc = include_str!("../cargodocs/decentralized.md")]
pub mod Decentralized{}

#[doc = include_str!("../cargodocs/wdecentralized.md")]
pub mod WeightedDecentralized{}