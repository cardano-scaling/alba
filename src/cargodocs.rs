//! Telescope-ALBA documentation

#![doc = include_str!("../cargodocs/intro.md")]

#[doc = include_str!("../cargodocs/overview.md")]
pub mod overview {}

#[doc = include_str!("../cargodocs/centralized/main.md")]
pub mod centralized {
    #[doc = include_str!("../cargodocs/centralized/structs.md")]
    pub mod structs{}

    #[doc = include_str!("../cargodocs/centralized/setup.md")]
    pub mod setup{}

    #[doc = include_str!("../cargodocs/centralized/prover.md")]
    pub mod prover{}

    #[doc = include_str!("../cargodocs/centralized/verifier.md")]
    pub mod verifier{}
}

#[doc = include_str!("../cargodocs/lottery.md")]
pub mod lottery {}

#[doc = include_str!("../cargodocs/decentralized.md")]
pub mod decentralized {}

#[doc = include_str!("../cargodocs/wdecentralized.md")]
pub mod weighted_decentralized {}

#[doc = include_str!("../cargodocs/variables.md")]
pub mod variables {}