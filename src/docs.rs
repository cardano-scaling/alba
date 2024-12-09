//! Approximate Lower Bound Arguments (_ALBA_) documentation.

#![doc = include_str!("../docs/intro.md")]

#[doc = include_str!("../docs/varmap.md")]
pub mod variables {}

#[doc = include_str!("../docs/centralized_telescope/main.md")]
pub mod centralized {
    #[doc = include_str!("../docs/centralized_telescope/params.md")]
    pub mod params {}

    #[doc = include_str!("../docs/centralized_telescope/proof.md")]
    pub mod proof {}

    #[doc = include_str!("../docs/centralized_telescope/round.md")]
    pub mod round {}
}
