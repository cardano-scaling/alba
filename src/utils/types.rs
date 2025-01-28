//! Types and implementation

/// Digest size for internal hashes
pub(crate) const DIGEST_SIZE: usize = 32;

/// Hash type for internal hashes
pub type Hash = [u8; DIGEST_SIZE];

pub(crate) fn truncate(data: Vec<u8>) -> Hash {
    let mut hash = [0u8; DIGEST_SIZE];
    for i in 0..DIGEST_SIZE.min(data.len()) {
        hash[i] = data[i];
    }
    hash
}
