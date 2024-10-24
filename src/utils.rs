use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use std::cmp::min;

// Oracles

/// Takes as input a hash and range $n$ and samples an integer from Unif(0, n).
/// We do so by interpreting the hash as a random number an returns it modulo n
/// (c.f. Appendix B, Alba paper).
pub fn oracle_uniform(hash: &[u8], n: usize) -> usize {
    // Computes the integer reprensation of hash* modulo n when n is not a
    // power of two. *(up to 8 bytes, in little endian)
    fn mod_non_power_of_2(hash: &[u8], n: usize) -> usize {
        fn log_base2(x: usize) -> usize {
            usize::BITS as usize - x.leading_zeros() as usize - 1
        }
        let epsilon_fail: usize = 1 << 40; // roughly 1 trillion
        let k: usize = log_base2(n * epsilon_fail);
        let k_prime: usize = 1 << k;
        let d: usize = k_prime.div_ceil(n);

        let i = mod_power_of_2(hash, k_prime);

        if i >= d * n {
            panic!("failed: i = {}, d = {}, n = {}, k = {}", i, d, n, k);
        } else {
            i % n
        }
    }
    // Computes the integer reprensation of hash* modulo n when n is a power of
    // two. *(up to 8 bytes, in little endian)
    fn mod_power_of_2(hash: &[u8], n: usize) -> usize {
        assert!(8 * hash.len() >= (n as f32).log2() as usize);
        from_bytes_le(hash) & (n - 1)
    }

    if n.is_power_of_two() {
        mod_power_of_2(hash, n)
    } else {
        mod_non_power_of_2(hash, n)
    }
}

/// Takes as input a hash and probability $q$ and returns true with
/// probability q otherwise false according to a Bernouilli distribution
/// (c.f. Appendix B, Alba paper).
pub fn oracle_bernouilli(hash: &[u8], q: f64) -> bool {
    // For error parameter ɛ̝, find an approximation x/y of q with (x,y) in N²
    // such that 0 < q - x/y <= ɛ̝
    let epsilon_fail: usize = 1 << 40; // roughly 1 trillion
    let mut x: usize = q.ceil() as usize;
    let mut y: usize = 1;
    while {
        let difference = q - (x as f64 / y as f64);
        difference >= 1.0 / epsilon_fail as f64 || difference < 0.0
    } {
        y *= 2;
        x = (q * (y as f64)).round() as usize;
    }
    // Output i in [0; y-1] from hash
    assert!(8 * hash.len() >= (y as f32).log2() as usize);
    let i = from_bytes_le(hash) & (y - 1);
    // Return true if i < x
    i < x
}

// Returns the integer representation of, up to the 8 first bytes of, the
// input bytes in little endian
fn from_bytes_le(bytes: &[u8]) -> usize {
    let mut array = [0u8; 8];
    let bytes = &bytes[..min(8, bytes.len())];
    array[..bytes.len()].copy_from_slice(bytes);
    usize::from_le_bytes(array)
}

// Hash helpers

/// Return a N-byte long hash of the given data
pub fn hash_bytes<const N: usize>(data: &[u8]) -> [u8; N] {
    let mut hasher = Blake2bVar::new(N).expect("Failed to construct hasher!");
    hasher.update(data);
    let mut buf = [0u8; N];
    hasher
        .finalize_variable(&mut buf)
        .expect("Failed to finalize hashing");
    buf
}

/// Return a N-byte long hash of the given list of data
pub fn combine_hashes<const N: usize>(hash_list: Vec<Vec<u8>>) -> [u8; N] {
    let mut hasher = Blake2bVar::new(N).expect("Failed to construct hasher!");
    for data in hash_list.iter() {
        hasher.update(data);
    }
    let mut buf = [0u8; N];
    hasher
        .finalize_variable(&mut buf)
        .expect("Failed to finalize hashing");
    buf
}