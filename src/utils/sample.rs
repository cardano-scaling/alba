//! Sample functions for Alba primitives

type Seed = [u8; 16];

/// Shorten an N-byte array, keeping the first M bytes and dropping the rest.
/// Checks that N >= M at compile time.
fn truncate_array<const N: usize, const M: usize>(arr: &[u8; N]) -> &[u8; M] {
    const {
        assert!(N >= M, "provided array is too small");
    }
    arr[..M].try_into().unwrap()
}

/// Takes as input a hash and range `n` and samples an integer from Unif[0, n[.
/// We do so by interpreting the hash as a random number and return it modulo
/// n (c.f. Appendix B, Alba paper). With negligible probability returns None.
/// `hash` must be at least 16 bytes.
#[allow(clippy::arithmetic_side_effects)]
pub(crate) fn sample_uniform<const N: usize>(hash: &[u8; N], n: u64) -> Option<u64> {
    let hash: &Seed = truncate_array(hash);

    // Computes the integer representation of hash modulo n when n is not a
    // power of two.
    fn mod_not_power_of_2(hash: &Seed, n: u64) -> Option<u64> {
        let n = u128::from(n);

        // Equals 2^128 / n since n is not a power of two.
        let d = u128::MAX / n;
        let i = u128::from_be_bytes(*hash);

        if i >= d * n {
            // We return here with probability (2^128 - d*n) / 2^128 < n / 2^128.
            // For n <= 2^40, this is less than 2^-88.
            None
        } else {
            Some((i % n) as u64)
        }
    }
    // Computes the integer representation of hash modulo n when n is a power of two.
    fn mod_power_of_2(hash: &Seed, n: u64) -> u64 {
        let bytes: &[u8; 8] = truncate_array(hash);
        u64::from_be_bytes(*bytes) & (n - 1)
    }

    if n.is_power_of_two() {
        Some(mod_power_of_2(hash, n))
    } else {
        mod_not_power_of_2(hash, n)
    }
}

/// Takes as input a `hash` and probability `q` and returns true with some
/// probability in [q - ε, q] for a negligible ε, otherwise false, according
/// to a Bernoulli distribution (c.f. Appendix B, Alba paper).
/// `hash` must be at least 16 bytes.
pub(crate) fn sample_bernoulli<const N: usize>(hash: &[u8; N], q: f64) -> bool {
    let hash: &Seed = truncate_array(hash);

    // We find an approximation x/y of q such that 0 <= q - x/y <= ε, where y = 2^128
    // and ε is a negligible number. Here ε is in the order of 2^-64 since the f64
    // calculation below has 64-bit precision. This is the best we can do since q
    // (which is f64) can already be 2^-64 away from the intended value.

    // Equals 2^128. We don't use f64::powi() since that function is non-deterministic.
    // See its documentation.
    const Y: f64 = (u128::pow(2, 127) as f64) * 2.0;
    // It is possible that Y*q is bigger than the maximum value of u128. In that case,
    // the casting using `as` saturates the result to the maximum allowed value, see
    // https://blog.rust-lang.org/2020/07/16/Rust-1.45.0.html#fixing-unsoundness-in-casts.
    // This can affect the error ε by at most 2^-128.
    let x = (Y * q).floor() as u128;
    let i = u128::from_be_bytes(*hash);

    // Return true iff i < x.
    i < x
}

#[cfg(test)]
#[allow(clippy::arithmetic_side_effects)]
mod tests {
    use super::sample_bernoulli;
    use super::sample_uniform;
    use test_case::test_case;

    type Hash = [u8; 16];

    fn u64_to_hash(x: u64) -> Hash {
        let mut res: Hash = Default::default();
        res[..8].copy_from_slice(&x.to_be_bytes());
        res
    }

    #[test_case(u64_to_hash(0), 1, 0; "n1_0")]
    #[test_case(u64_to_hash(1), 1, 0; "n1_1")]
    #[test_case(u64_to_hash(2), 1, 0; "n1_2")]
    #[test_case(u64_to_hash(0), 2, 0; "n2_0")]
    #[test_case(u64_to_hash(1), 2, 1; "n2_1")]
    #[test_case(u64_to_hash(2), 2, 0; "n2_2")]
    #[test_case(u64_to_hash(3), 2, 1; "n2_3")]
    #[test_case(u64_to_hash(0), 4, 0; "n4_0")]
    #[test_case(u64_to_hash(1), 4, 1; "n4_1")]
    #[test_case(u64_to_hash(2), 4, 2; "n4_2")]
    #[test_case(u64_to_hash(3), 4, 3; "n4_3")]
    #[test_case(u64_to_hash(4), 4, 0; "n4_4")]
    #[test_case(u64_to_hash(41), 32, 9; "n16")]
    #[test_case(0u128.to_be_bytes(), 3, 0; "n3_0")]
    #[test_case(1u128.to_be_bytes(), 3, 1; "n3_1")]
    #[test_case(2u128.to_be_bytes(), 3, 2; "n3_2")]
    #[test_case(3u128.to_be_bytes(), 3, 0; "n3_3")]
    #[test_case(40u128.to_be_bytes(), 17, 6; "n17")]
    fn sample_uniform_valid(hash: Hash, n: u64, expected: u64) {
        assert_eq!(expected, sample_uniform(&hash, n).unwrap());
    }

    #[test]
    fn sample_uniform_invalid() {
        // (2^128 - 1) is not divisible by 18 since the first is odd and the second
        // is even.
        assert_eq!(None, sample_uniform(&u128::MAX.to_be_bytes(), 18));
    }

    #[test_case(0u128.to_be_bytes(), 0.0, false; "q0_0")]
    #[test_case(1u128.to_be_bytes(), 0.0, false; "q0_1")]
    #[test_case(0u128.to_be_bytes(), 1.0, true; "q1_0")]
    #[test_case((u128::MAX - 1).to_be_bytes(), 1.0, true; "q1_large")]
    #[test_case(0u128.to_be_bytes(), 0.3, true; "q_third_0")]
    #[test_case((u128::MAX / 3 - 2u128.pow(73)).to_be_bytes(), 1.0/3.0, true;
        "q_third_mid_left")]
    #[test_case((u128::MAX / 3 + 1).to_be_bytes(), 1.0/3.0, false;
        "q_third_mid_right")]
    #[test_case((u128::MAX - 9).to_be_bytes(), 1.0/3.0, false;
        "q_third_large")]
    fn sample_bernoulli_all(hash: Hash, q: f64, expected: bool) {
        assert_eq!(expected, sample_bernoulli(&hash, q));
    }
}
