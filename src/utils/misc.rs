/// Returns true iff all elements in the slice are distinct.
pub(crate) fn check_distinct<E: AsRef<[u8]> + Clone + Ord>(elements: &[E]) -> bool {
    let mut elements = elements.to_vec();
    elements.sort_unstable();
    elements.is_sorted_by(|a, b| a < b)
}
