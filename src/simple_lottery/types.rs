//! Types specific to simple lottery

/// Digest size for internal hashes
pub(super) const DIGEST_SIZE: usize = 32;

/// Hash type for internal hashes
pub(super) type Hash = [u8; DIGEST_SIZE];
