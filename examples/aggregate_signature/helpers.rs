use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use crate::AlbaThresholdSignature;

use digest::{Digest, FixedOutput};
use std::collections::HashMap;

use blake2::{
    digest::{Update, VariableOutput},
    Blake2bVar,
};
use blst::min_sig::{AggregatePublicKey, AggregateSignature, PublicKey, Signature};
use blst::BLST_ERROR;

/// Helper function to compute a commitment by hashing `(checksum || msg)`
pub(crate) fn get_commitment<const N: usize>(checksum: &[u8], msg: &[u8]) -> [u8; N] {
    let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
    let mut commitment = [0u8; N];

    hasher.update(checksum);
    hasher.update(msg);
    hasher
        .finalize_variable(&mut commitment)
        .expect("Hash finalization failed");
    commitment
}

/// Collect valid signatures by verifying each individual signature.
/// Returns a hashmap of valid signature bytes to their corresponding index.
pub(crate) fn collect_valid_signatures<const N: usize>(
    signature_list: &[IndividualSignature],
    registration: &Registration,
    msg: &[u8],
) -> HashMap<[u8; 48], usize> {
    let mut valid_signatures = HashMap::new();

    match &registration.checksum {
        Some(checksum) => {
            for sig in signature_list {
                if let Some(verification_key) = registration.registered_keys.get(&sig.index) {
                    if sig.verify::<N>(checksum, msg, verification_key) {
                        valid_signatures.insert(sig.signature.to_bytes(), sig.index);
                    }
                } else {
                    println!("Warning: No verification key found for index {}", sig.index);
                }
            }
        }
        None => {
            println!("Error: Registration is not closed. Cannot verify signatures.");
        }
    }
    valid_signatures
}

/// Validate the signatures in `alba_threshold_signature` against the provided message and registration.
/// Returns `true` if all signatures are valid and correctly aggregated, `false` otherwise.
pub(crate) fn validate_signatures<H: Digest + FixedOutput>(
    alba_threshold_signature: &AlbaThresholdSignature<H>,
    registration: &Registration,
    commitment: &[u8],
) -> bool {
    let mut signatures = Vec::with_capacity(alba_threshold_signature.proof.element_sequence.len());
    for sig_bytes in &alba_threshold_signature.proof.element_sequence {
        if let Ok(signature) = Signature::from_bytes(sig_bytes.as_slice()) {
            signatures.push(signature);
        } else {
            println!("Error: Failed to parse signature from bytes.");
            return false;
        }
    }
    let signature_refs: Vec<&Signature> = signatures.iter().collect();
    let aggregate_signature =
        if let Ok(agg_sig) = AggregateSignature::aggregate(signature_refs.as_slice(), false) {
            agg_sig.to_signature()
        } else {
            println!("Error: Failed to aggregate signatures.");
            return false;
        };

    let public_key_refs: Vec<&PublicKey> = alba_threshold_signature
        .indices
        .iter()
        .filter_map(|index| registration.registered_keys.get(index))
        .collect();

    if public_key_refs.len() != signature_refs.len() {
        println!("Error: Mismatch between public keys and signatures count.");
        return false;
    }

    let aggregate_public_key =
        if let Ok(agg_pk) = AggregatePublicKey::aggregate(public_key_refs.as_slice(), false) {
            agg_pk.to_public_key()
        } else {
            println!("Error: Failed to aggregate public keys.");
            return false;
        };

    let result =
        aggregate_signature.verify(false, commitment, &[], &[], &aggregate_public_key, false);
    if result == BLST_ERROR::BLST_SUCCESS {
        true
    } else {
        println!("Error: Aggregate signature verification failed.");
        false
    }
}

pub(crate) fn ats_size<H: Digest + FixedOutput, const N: usize>(
    ats: &AlbaThresholdSignature<H>,
) -> usize {
    let nb_elements = ats.indices.len();
    let size_indices = nb_elements.saturating_mul(8);
    let size_elements = nb_elements.saturating_mul(N);
    let size_commitment = ats.commitment.len();

    size_indices
        .saturating_add(size_elements)
        .saturating_add(size_commitment)
        .saturating_add(16)
}
