//! Helper functions for Alba primitives
use std::cmp::min;

/// Takes as input a hash and range $n$ and samples an integer from Unif[0, n[.
/// We do so by interpreting the hash as a random number and returns it modulo
/// n (c.f. Appendix B, Alba paper).
pub(crate) fn sample_uniform(hash: &[u8], n: u64, sec_param: u64) -> Option<u64> {
    // Computes the integer reprensation of hash* modulo n when n is not a
    // power of two. *(up to 8 bytes, in little endian)
    fn mod_non_power_of_2(hash: &[u8], n: u64, sec_param: u64) -> Option<u64> {
        fn log_base2(x: u64) -> u64 {
            u64::from(
                u64::BITS
                    .saturating_sub(x.leading_zeros())
                    .saturating_sub(1),
            )
        }
        let epsilon_fail: u64 = 1 << sec_param;
        let k = log_base2(n.saturating_mul(epsilon_fail));
        let k_prime: u64 = 1 << k;
        let d = k_prime.div_ceil(n);

        let i = mod_power_of_2(hash, k_prime);

        if i >= d.saturating_mul(n) {
            None
        } else {
            Some(i.rem_euclid(n))
        }
    }
    // Computes the integer reprensation of hash* modulo n when n is a power of
    // two. *(up to 8 bytes, in little endian)
    fn mod_power_of_2(hash: &[u8], n: u64) -> u64 {
        debug_assert!(8u32.saturating_mul(hash.len() as u32) >= n.ilog2());
        from_bytes_be(hash) & n.saturating_sub(1)
    }

    if n.is_power_of_two() {
        Some(mod_power_of_2(hash, n))
    } else {
        mod_non_power_of_2(hash, n, sec_param)
    }
}

/// Takes as input a hash and probability $q$ and returns true with
/// probability q otherwise false according to a Bernoulli distribution
/// (c.f. Appendix B, Alba paper).
pub(crate) fn sample_bernoulli(hash: &[u8], q: f64, sec_param: u64) -> bool {
    // For error parameter ɛ̝, find an approximation x/y of q with (x,y) in N²
    // such that 0 < q - x/y <= ɛ̝
    let epsilon_fail: u64 = 1 << sec_param;
    let mut x: u64 = q.ceil() as u64;
    let mut y: u64 = 1;
    while {
        let difference = q - (x as f64 / y as f64);
        difference >= 1.0 / epsilon_fail as f64 || difference < 0.0
    } {
        y = y.saturating_mul(2);
        x = (q * (y as f64)).round() as u64;
    }
    // Output i in [0; y-1] from hash
    debug_assert!(8.0 * hash.len() as f32 >= (y as f32).log2());
    let i = from_bytes_be(hash) & y.saturating_sub(1);
    // Return true if i < x
    i < x
}

// Returns the integer representation of, up to the 8 first bytes of, the
// input bytes in little endian
fn from_bytes_be(bytes: &[u8]) -> u64 {
    let mut array = [0u8; 8];
    let bytes = &bytes[..min(8, bytes.len())];
    array[..bytes.len()].copy_from_slice(bytes);
    u64::from_be_bytes(array)
}
