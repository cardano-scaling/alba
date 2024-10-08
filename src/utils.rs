use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use std::cmp::min;

/// Binary Random Oracles (c.f. Appendix B, Alba paper)
/// Takes as input a hash and inverse probability and returns an integer
pub fn oracle(hash: &[u8], n: usize) -> usize {
    if n.is_power_of_two() {
        mod_power_of_2(hash, n)
    } else {
        mod_non_power_of_2(hash, n)
    }
}

fn mod_non_power_of_2(hash: &[u8], n: usize) -> usize {
    fn log_base2(x: usize) -> usize {
        usize::BITS as usize - x.leading_zeros() as usize - 1
    }
    let epsilon_fail: usize = 1 << 40; // roughly 1 in 10 billion
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

fn mod_power_of_2(hash: &[u8], n: usize) -> usize {
    fn from_bytes_le(bytes: &[u8]) -> usize {
        let mut array = [0u8; 8];
        let bytes = &bytes[..min(8, bytes.len())];
        array[..bytes.len()].copy_from_slice(bytes);
        usize::from_le_bytes(array)
    }
    let r = from_bytes_le(hash);
    (n - 1) & r
}

/// Compute the Probability Mass Function (PMF) of a Binomial Distribution B(n,p)
pub fn bin_pmf(n: usize, k: usize, p: f64) -> f64 {
    // Compute the binomial coefficient (k out of n)
    fn bin_coeff(n: usize, k: usize) -> usize {
        if k == 0 {
            return 1;
        };
        ((n as u128 * bin_coeff(n - 1, k - 1) as u128) / k as u128) as usize
    }
    let coeff = bin_coeff(n, k) as f64;
    coeff * p.powi(k as i32) * (1f64 - p).powi((n - k) as i32)
}

/// Compute the discrete Cumulative Distribution Function (CDF) of a Binomial Distribution B(n,p)
pub fn bin_cdf(n: usize, k: usize, p: f64) -> f64 {
    if k == n {
        return 1.0;
    };
    (0..=k).map(|i| bin_pmf(n, i, p)).sum()
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

// Test & Bench helpers

/// Generate a set of items given the set size and a seed
/// Items are generated by hashing the current index
pub fn gen_items<const N: usize>(seed: Vec<u8>, set_size: usize) -> Vec<[u8; N]> {
    let mut s_p = Vec::with_capacity(set_size);
    for b in 0..set_size {
        let mut data = Vec::new();
        data.push(seed.clone());
        data.push(b.to_ne_bytes().to_vec());
        let item = combine_hashes::<N>(data);
        s_p.push(item);
    }
    s_p
}

/// Generate a set of weighted items given the total weight size, number of items and a seed
/// Items are generated by hashing the current index
pub fn gen_weighted_items<const N: usize>(
    seed: Vec<u8>,
    total_weight: usize,
    set_size: usize,
) -> Vec<([u8; N], usize)> {
    assert!(set_size <= total_weight);
    let mut s_p = Vec::with_capacity(set_size);
    let mut s_n = Vec::with_capacity(set_size);
    let mut sum: u64 = 0;
    // Initialising items with random weights
    for b in 0..set_size {
        let mut data = Vec::new();
        data.push(seed.clone());
        data.push(b.to_ne_bytes().to_vec());
        let item = combine_hashes::<N>(data);
        let weight = u32::from_be_bytes(hash_bytes::<4>(&item));
        sum += weight as u64;
        s_p.push(item);
        s_n.push(weight);
    }
    // Updating weights to add up to around total_weight, with minimum weight of 1
    let denominator = sum as f32 / (total_weight - set_size) as f32;
    let mut new_sum: u64 = 0;
    s_n.iter_mut().for_each(|w| {
        *w = 1 + (*w as f32 / denominator).ceil() as u32;
        new_sum += *w as u64;
    });
    // Fixing ceiling error
    let total_weight = total_weight as u64;
    let b = total_weight < new_sum;
    let mut delta = if b {
        new_sum - total_weight
    } else {
        total_weight - new_sum
    };

    while delta != 0 {
        s_n = s_n
            .iter()
            .map(|&w| match (delta, b) {
                (0, _) => w,
                (_, true) => {
                    delta -= 1;
                    w - if w > 1 { 1 } else { 0 }
                }
                (_, false) => {
                    delta -= 1;
                    w + 1
                }
            })
            .collect();
    }
    let mut result: Vec<([u8; N], usize)> = Vec::new();
    for i in 0..set_size {
        result.push((s_p[i], s_n[i] as usize));
    }
    return result;
}

pub fn format_time(nanos: u128) -> String {
    let mut time = nanos;
    let bounds = [1000, 1000, 1000, 60, 60, 60];
    let units = ["ns", "μs", "ms", "s", "min", "h"];
    for (&bound, &unit) in bounds.iter().zip(units.iter()) {
        if time < bound {
            return time.to_string() + unit;
        }
        time = time / bound;
    }
    (time * 60).to_string() + "h"
}

pub fn format_nb(x: usize) -> String {
    let mut y = x;
    let mut s = String::new();
    let mut b = true;
    while y / 1000 != 0 {
        let to_add = (y % 1000).to_string();
        let preppend = "0".repeat(3 - to_add.len()) + &to_add;
        let append = if b { "" } else { &("_".to_string() + &s) };
        s = preppend + append;
        b = false;
        y = y / 1000;
    }
    if b {
        y.to_string()
    } else {
        (y % 1000).to_string() + "_" + &s
    }
}