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

pub(crate) const DATA_LENGTH: usize = 48;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
/// Type of dataset's elements with an optional index
pub struct Element {
    /// Set element data
    pub data: Vec<u8>,
    /// ID of the element
    pub index: Option<u64>,
}

impl Element {
    /// Create an `Element` from bytes without an index
    pub fn from_bytes(data: &[u8]) -> Option<Self> {
        if data.len() != DATA_LENGTH {
            return None;
        }
        Some(Element {
            data: data.to_vec(),
            index: None,
        })
    }

    /// Create an `Element` from bytes with an index
    pub fn from_bytes_with_index(data: &[u8], index: u64) -> Option<Self> {
        if data.len() != DATA_LENGTH {
            return None;
        }
        Some(Element {
            data: data.to_vec(),
            index: Some(index),
        })
    }

    /// Converts a generic `Vec<Vec<u8>>` into `Vec<Element>` (no index)
    pub fn element_list_from_bytes(raw_data: Vec<Vec<u8>>) -> Vec<Element> {
        raw_data
            .into_iter()
            .filter_map(|data| Element::from_bytes(&data))
            .collect()
    }

    /// Converts a generic `Vec<Vec<u8>>` into `Vec<Element>` with indices
    pub fn element_list_from_bytes_with_index(raw_data: Vec<Vec<u8>>) -> Vec<Element> {
        raw_data
            .into_iter()
            .enumerate()
            .filter_map(|(i, data)| Element::from_bytes_with_index(&data, i as u64))
            .collect()
    }
}

impl AsRef<[u8]> for Element {
    fn as_ref(&self) -> &[u8] {
        &self.data
    }
}

/// Indexable trait
pub trait Indexable {
    /// Trait function returning the index of an element
    fn index(&self) -> Option<u64>;
}

impl Indexable for Element {
    fn index(&self) -> Option<u64> {
        self.index
    }
}
