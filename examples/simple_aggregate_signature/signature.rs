use blst::min_sig::{PublicKey, Signature as BLSSignature};
use blst::BLST_ERROR;

/// Single signature
#[derive(Debug, Clone)]
pub(crate) struct Signature {
    /// Signature of type bls signature
    pub(crate) signature: BLSSignature,
    /// Verification index of the signer
    pub(crate) index: usize,
}

impl Signature {
    /// Verify signature against `commitment = Hash(checksum || msg)`
    pub(crate) fn verify(&self, msg: &[u8], verification_key: &PublicKey) -> bool {
        let result = self
            .signature
            .verify(false, msg, &[], &[], verification_key, false);
        result == BLST_ERROR::BLST_SUCCESS
    }
}
