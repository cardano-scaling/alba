//! Test & Bench helpers

use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;

/// Generate a set of items given the set size and a seed
/// Items are generated by hashing the current index
pub(crate) fn gen_items<const N: usize>(seed: &[u8], set_size: u64) -> Vec<[u8; N]> {
    let mut s_p = Vec::with_capacity(set_size as usize);
    let mut data_buf: [u8; 8];
    let mut digest_buf: [u8; N] = [0u8; N];
    for b in 0..set_size {
        data_buf = b.to_be_bytes();
        let mut hasher = Blake2bVar::new(N).expect("Failed to construct hasher!");
        hasher.update(seed);
        hasher.update(&data_buf);
        hasher
            .finalize_variable(&mut digest_buf)
            .expect("Failed to finalize hashing");
        s_p.push(digest_buf);
    }
    s_p
}