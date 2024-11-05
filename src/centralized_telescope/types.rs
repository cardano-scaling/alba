//! Types specific to centralized_telescope

/// Digest size for internal hashes
pub(crate) const DIGEST_SIZE: usize = 32;

/// Hash type for internal hashes
pub(super) type Hash = [u8; DIGEST_SIZE];
