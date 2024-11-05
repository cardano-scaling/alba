use super::types::Hash;
use super::Element;
use crate::utils::sample;
use blake2::{Blake2s256, Digest};

/// Round parameters
#[derive(Debug, Clone)]
pub(super) struct Round {
    /// Proof counter
    pub(super) v: u64,
    /// Proof 2nd counter
    pub(super) t: u64,
    // Round candidate tuple
    pub(super) s_list: Vec<Element>,
    /// Round candidate hash
    pub(super) h: Hash,
    /// Round candidate hash mapped to [1, n_p]
    pub(super) h_u64: u64,
    /// Approximate size of set Sp to lower bound
    pub(super) n_p: u64,
}

impl Round {
    /// Oracle producing a uniformly random value in [0, n_p[ used for round candidates
    /// We also return hash(data) to follow the optimization presented in Section 3.3
    fn h1(first_input: &[u8], second_input: &[u8], n_p: u64) -> (Hash, Option<u64>) {
        let mut hasher = Blake2s256::new();
        hasher.update(b"Telescope-H1");
        hasher.update(first_input);
        hasher.update(second_input);
        let digest: Hash = hasher.finalize().into();
        (digest, sample::sample_uniform(&digest, n_p))
    }

    /// Output a round from a proof counter and n_p
    /// Initilialises the hash with H1(t) and random value as oracle(H1(t), n_p)
    pub(super) fn new(v: u64, t: u64, n_p: u64) -> Option<Self> {
        let v_bytes: [u8; 8] = v.to_be_bytes();
        let t_bytes: [u8; 8] = t.to_be_bytes();
        let (h, h_u64_opt) = Self::h1(&v_bytes, &t_bytes, n_p);
        h_u64_opt.map(|h_u64| Self {
            v,
            t,
            s_list: Vec::new(),
            h,
            h_u64,
            n_p,
        })
    }

    /// Updates a round with an element of S_p
    /// Replaces the hash $h$ with $h' = H1(h, s)$ and the random value as oracle(h', n_p)
    pub(super) fn update(r: &Self, s: Element) -> Option<Self> {
        let mut s_list = r.s_list.clone();
        s_list.push(s);
        let (h, h_u64_opt) = Self::h1(&r.h, &s, r.n_p);
        h_u64_opt.map(|h_u64| Self {
            v: r.v,
            t: r.t,
            s_list,
            h,
            h_u64,
            n_p: r.n_p,
        })
    }
}
