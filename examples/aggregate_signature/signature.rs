use crate::aggregate_signature::helpers::get_commitment;
use blst::min_sig::{PublicKey, Signature};
use blst::BLST_ERROR;

/// Single signature
#[derive(Debug, Clone)]
pub(crate) struct IndividualSignature {
    /// Signature of type bls signature
    pub(crate) signature: Signature,
    /// Registration index of the signer
    pub(crate) index: usize,
}

impl IndividualSignature {
    /// Verify signature against `commitment = Hash(checksum || msg)`
    pub(crate) fn verify<const N: usize>(
        &self,
        checksum: &[u8],
        msg: &[u8],
        verification_key: &PublicKey,
    ) -> bool {
        let commitment = get_commitment::<N>(checksum, msg);
        let result = self
            .signature
            .verify(false, &commitment, &[], &[], verification_key, false);
        result == BLST_ERROR::BLST_SUCCESS
    }
}
