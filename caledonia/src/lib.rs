//! Approximate Lower Bound Arguments (ALBA, <https://eprint.iacr.org/2023/1655.pdf>)
//!
//! Alba is a generic protocol to prove succinctly a lower bound of the size of
//! a, potentially weighted, set. Say we have a set Sp of size |Sp| >= $n_p$,
//! and a lower bound $n_f$ < $n_p$ of it we want to prove. Alba gives us a
//! method to generate a proof of knowledge of this bound by finding the
//! smallest subset of Sp of size $u$ to convince a verifier.
//! The paper presents several schemes and optimizations. The basic scheme is
//! enhanced in the "prehashed" version thanks to sorting Sp with a balls and
//! bins sorting algorithm reducing the number of hashes done per round. A
//! lottery scheme is also introduced to support Alba in a decentralised
//! settings as well as a modification to use PRF in the CRS settings instead of
//! using the ROM.

pub mod bounded;
pub mod prehashed;
