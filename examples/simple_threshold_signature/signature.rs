use blst::min_sig::Signature as BLSSignature;

/// Single signature
#[derive(Debug, Clone)]
pub(crate) struct Signature {
    /// Signature of type bls signature
    pub(crate) signature: BLSSignature,
    /// Verification index of the signer
    pub(crate) index: usize,
}
