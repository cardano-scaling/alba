//! Types and implementation

/// Digest size for internal hashes
pub(crate) const DIGEST_SIZE: usize = 32;

/// Hash type for internal hashes
pub type Hash = [u8; DIGEST_SIZE];

pub(crate) fn truncate(data: &[u8]) -> Hash {
    let n = DIGEST_SIZE.min(data.len());
    let mut hash = [0u8; DIGEST_SIZE];
    hash[..n].copy_from_slice(&data[..n]);
    hash
}
