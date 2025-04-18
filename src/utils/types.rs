//! Types, traits and their implementation

use super::errors::ElementError;
use std::cmp::Ordering;

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

#[derive(Debug, Clone)]
/// Type of dataset's elements with an optional index
pub struct Element<E> {
    /// Set element data
    pub data: E,
    /// ID of the element
    pub index: Option<u64>,
}

impl<E: AsRef<[u8]> + Clone> Element<E> {
    /// Create a new element for given data and index
    pub fn new(data: E, index: Option<u64>) -> Self {
        Self { data, index }
    }

    /// Sort given list of elements
    /// # Errors
    ///
    /// Returns an `ElementError`
    pub fn sort_elements(elements: &[Element<E>]) -> Result<Vec<Element<E>>, ElementError> {
        if elements.is_empty() {
            return Ok(vec![]);
        }

        let all_have_index = elements[0].index.is_some();
        if elements.iter().any(|e| e.index.is_some() != all_have_index) {
            // Mixed Some and None
            return Err(ElementError::InconsistentElements);
        }

        let mut sorted = elements.to_vec();

        let mut unique_elements = true;
        sorted.sort_unstable_by(|a, b| {
            a.as_ref()
                .cmp(b.as_ref())
                .then(if let (Some(a_i), Some(b_i)) = (a.index, b.index) {
                    if a_i.cmp(&b_i).is_eq() {
                        unique_elements = false;
                    }
                    a_i.cmp(&b_i)
                } else {
                    // Should not happen
                    unique_elements = false;
                    Ordering::Equal
                })
        });

        if unique_elements {
            Ok(sorted)
        } else {
            Err(ElementError::RepeatedElements)
        }
    }

    /// Return true if the elements is sorted
    pub fn is_sorted(elements: &[Element<E>]) -> bool {
        elements.is_sorted_by(|a, b| {
            (a.as_ref() < b.as_ref())
                || (a.as_ref() == b.as_ref()
                    && a.index
                        .is_some_and(|a_i| b.index.is_some_and(|b_i| a_i < b_i)))
        })
    }

    /// Return true if the elements are unique
    pub fn is_unique(elements: &[Element<E>]) -> bool {
        !matches!(
            Element::sort_elements(elements),
            Err(ElementError::RepeatedElements)
        )
    }
}

impl<E: AsRef<[u8]>> AsRef<[u8]> for Element<E> {
    fn as_ref(&self) -> &[u8] {
        self.data.as_ref()
    }
}
