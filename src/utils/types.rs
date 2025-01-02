pub(crate) const DATA_LENGTH: usize = 32;

/// Type of dataset's elements to lower bound
pub(crate) type Element = [u8; DATA_LENGTH];

/// Digest size for internal hashes
pub(crate) const DIGEST_SIZE: usize = 32;

/// Hash type for internal hashes
pub(crate) type Hash = [u8; DIGEST_SIZE];
