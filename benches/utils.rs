use rand_chacha::ChaCha20Rng;
use rand_core::RngCore;

use caledonia::utils::gen_items;

pub fn setup_centralised_wrapper(
    rng: &mut ChaCha20Rng,
    l: usize,
    sp: usize,
    np: usize,
    nf: usize,
) -> (Vec<[u8; 32]>, caledonia::centralised::Setup) {
    use caledonia::centralised::*;
    let seed_u32 = rng.next_u32();
    let seed = seed_u32.to_ne_bytes().to_vec();
    let dataset: Vec<[u8; 32]> = gen_items(seed, sp);
    let params = Params {
        lambda_sec: l,
        lambda_rel: l,
        n_p: (np * sp).div_ceil(100),
        n_f: (nf * sp).div_ceil(100),
    };
    (dataset, Setup::new(&params))
}
