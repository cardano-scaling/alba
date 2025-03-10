//! Approximate Lower Bound Arguments (_ALBA_) documentation.

#![doc = include_str!("../docs/rustdoc/intro.md")]

#[doc = include_str!("../docs/rustdoc/varmap.md")]
pub mod variables {}

#[doc = include_str!("../docs/rustdoc/basic_construction.md")]
pub mod basic {}

#[doc = include_str!("../docs/rustdoc/prehashing.md")]
pub mod prehashing {}

#[doc = include_str!("../docs/rustdoc/centralized_telescope/intro.md")]
#[doc = include_str!("../docs/rustdoc/centralized_telescope/main.md")]
pub mod centralized {
    #[doc = include_str!("../docs/rustdoc/centralized_telescope/params/overview.md")]
    #[doc = include_str!("../docs/rustdoc/centralized_telescope/params/strategy.md")]
    #[doc = include_str!("../docs/rustdoc/centralized_telescope/structures/struct_params.md")]
    #[doc = include_str!("../docs/rustdoc/centralized_telescope/params/setup.md")]
    pub mod params {}

    #[doc = include_str!("../docs/rustdoc/centralized_telescope/proof.md")]
    pub mod proof {}

    #[doc = include_str!("../docs/rustdoc/centralized_telescope/round.md")]
    pub mod round {}
}
