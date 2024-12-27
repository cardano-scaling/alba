use super::types::Element;

/// Returns true iff all elements in the slice are distinct.
pub(crate) fn check_distinct(elements: &[Element]) -> bool {
    let mut elements = elements.to_vec();
    elements.sort();
    elements.is_sorted_by(|a, b| a < b)
}
