//! Types and implementation

/// Digest size for internal hashes
pub(crate) const DIGEST_SIZE: usize = 32;

/// Hash type for internal hashes
pub type Hash = [u8; DIGEST_SIZE];

pub(crate) fn truncate(data: &[u8]) -> Hash {
    debug_assert!(data.len() >= DIGEST_SIZE);
    let n = DIGEST_SIZE.min(data.len());
    let mut hash = [0u8; DIGEST_SIZE];
    hash[..n].copy_from_slice(&data[..n]);
    hash
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
/// Type of dataset's elements with an optional index
pub struct Element<E> {
    /// Set element data
    pub data: E,
    /// ID of the element
    pub index: Option<u64>,
}

impl<E: AsRef<[u8]> + From<Vec<u8>>> Element<E> {
    /// Creates an `Element` from raw bytes with an explicit index
    pub fn from_bytes_with_index(raw_bytes: &[u8], index: u64) -> Option<Self> {
        Some(Element {
            data: E::from(raw_bytes.to_vec()),
            index: Some(index),
        })
    }

    /// Creates an `Element` from raw bytes without an explicit index
    pub fn from_bytes_no_index(raw_bytes: &[u8]) -> Option<Self> {
        Some(Element {
            data: E::from(raw_bytes.to_vec()),
            index: None,
        })
    }
}

impl<E: AsRef<[u8]>> AsRef<[u8]> for Element<E> {
    fn as_ref(&self) -> &[u8] {
        self.data.as_ref()
    }
}
