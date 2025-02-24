/// Returns true iff all elements in the slice are distinct.
pub(crate) fn check_distinct<E: AsRef<[u8]> + Clone>(elements: &[E]) -> bool {
    let mut elements = elements.to_vec();
    elements.sort_unstable_by(|a, b| a.as_ref().cmp(b.as_ref()));
    elements.is_sorted_by(|a, b| a.as_ref() < b.as_ref())
}
