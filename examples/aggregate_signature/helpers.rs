use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use crate::{AlbaThresholdSignature, Element};
use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use blst::min_sig::{AggregatePublicKey, AggregateSignature, PublicKey, Signature};
use blst::BLST_ERROR;
use std::collections::HashMap;

// Helpers
pub(crate) fn get_commitment<const N: usize>(checksum: &[u8], msg: &[u8]) -> [u8; N] {
    let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
    let mut commitment = [0u8; N];

    hasher.update(checksum);
    hasher.update(msg);
    hasher.finalize_variable(&mut commitment).unwrap();
    commitment
}

pub(crate) fn collect_valid_signatures<const N: usize>(
    signature_list: &[IndividualSignature],
    registration: &Registration,
    msg: &[u8],
) -> HashMap<Element, usize> {
    let mut valid_signatures = HashMap::new();
    for sig in signature_list {
        if let Some(verification_key) = registration.registered_keys.get(&sig.index) {
            if sig.verify::<N>(
                &registration.checksum.clone().unwrap(),
                &msg,
                &verification_key,
            ) {
                valid_signatures.insert(sig.signature.to_bytes(), sig.index);
            }
        }
    }
    valid_signatures
}

pub(crate) fn validate_signatures(
    alba_threshold_signature: &AlbaThresholdSignature,
    registration: &Registration,
    msg: &[u8],
) -> bool {
    let mut signatures = Vec::with_capacity(alba_threshold_signature.proof.element_sequence.len());
    for sig_bytes in &alba_threshold_signature.proof.element_sequence {
        let signature = Signature::from_bytes(sig_bytes.as_slice()).unwrap();
        signatures.push(signature);
    }
    let signature_refs: Vec<&Signature> = signatures.iter().collect();
    let public_key_refs: Vec<&PublicKey> = alba_threshold_signature
        .indices
        .iter()
        .filter_map(|index| registration.registered_keys.get(index))
        .collect::<Vec<&PublicKey>>();

    let aggregate_signature = AggregateSignature::aggregate(signature_refs.as_slice(), false)
        .unwrap()
        .to_signature();
    let aggregate_public_key = AggregatePublicKey::aggregate(public_key_refs.as_slice(), false)
        .unwrap()
        .to_public_key();
    let result = aggregate_signature.verify(false, msg, &[], &[], &aggregate_public_key, false);
    result == BLST_ERROR::BLST_SUCCESS
}
