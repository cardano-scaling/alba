use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signer::VerificationKey;
use blake2::digest::{Update, VariableOutput};
use blake2::{digest::consts::U16, Blake2b, Blake2bVar, Digest};
use blst::min_sig::Signature as BlstSignature;
use blst::{blst_p1, blst_p2, p1_affines, p2_affines, BLST_ERROR};

use unsafe_helpers::*;

/// Signature, which is a wrapper over the `BlstSignature` type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Signature(pub(crate) BlstSignature);

/// Individual Signature.
/// It includes a BLS signature, using the blst library, its verification key, and the registration index of the signer.
#[derive(Debug, Clone)]
pub(crate) struct IndividualSignature {
    /// Individual signature of type blst `Signature`
    pub(crate) signature: Signature,
    /// Verification key (wrapper over the blst `PublicKey`) of the individual signature.
    pub(crate) verification_key: VerificationKey,
}

impl Signature {
    pub fn verify(&self, msg: &[u8], verification_key: &VerificationKey) -> BLST_ERROR {
        self.0
            .verify(false, msg, &[], &[], &verification_key.0, false)
    }
    /// Convert an `Signature` to its byte representation.
    pub fn to_bytes(self) -> [u8; 48] {
        self.0.to_bytes()
    }

    fn aggregate(sigs: &[Signature], vks: &[VerificationKey]) -> Option<(Self, VerificationKey)> {
        if vks.len() != sigs.len() || vks.is_empty() {
            return None;
        }

        if vks.len() < 2 {
            return Some((sigs[0], vks[0]));
        }

        let mut hashed_sigs = Blake2b::<U16>::new();
        for sig in sigs {
            Digest::update(&mut hashed_sigs, sig.to_bytes());
        }

        // First we generate the scalars
        let mut scalars = Vec::with_capacity(vks.len() * 128);
        let mut signatures = Vec::with_capacity(vks.len());
        for (index, sig) in sigs.iter().enumerate() {
            let mut hasher = hashed_sigs.clone();
            Digest::update(&mut hasher, index.to_be_bytes());
            signatures.push(sig.0);
            scalars.extend_from_slice(hasher.finalize().as_slice());
        }

        let transmuted_vks: Vec<blst_p2> = vks.iter().map(|vk| vk_from_p2_affine(*vk)).collect();

        let transmuted_sigs: Vec<blst_p1> = signatures
            .iter()
            .map(|sig| sig_to_p1(Signature(*sig)))
            .collect();

        let grouped_vks = p2_affines::from(transmuted_vks.as_slice());
        let grouped_sigs = p1_affines::from(transmuted_sigs.as_slice());

        let aggr_vk: VerificationKey = p2_affine_to_vk(&grouped_vks.mult(&scalars, 128));
        let aggr_sig: Signature = p1_affine_to_sig(&grouped_sigs.mult(&scalars, 128));

        Some((aggr_sig, aggr_vk))
    }

    pub fn verify_aggregate(
        signatures: &[Signature],
        verification_keys: &[VerificationKey],
        msg: &[u8],
    ) -> BLST_ERROR {
        if let Some((signature, key)) = Signature::aggregate(signatures, verification_keys) {
            signature.verify(msg, &key)
        } else {
            BLST_ERROR::BLST_VERIFY_FAIL // Return a failure error if aggregation fails
        }
    }
}

impl IndividualSignature {
    /// Verify a signature
    /// First, validate that the signer's verification key is actually registered.
    /// Then, verify the blst signature against the given `commitment` (Hash(checksum||msg)).
    pub fn verify<const N: usize>(&self, registration: &Registration, msg: &[u8]) -> bool {
        let commitment: [u8; N] = match registration.get_commitment::<N>(msg) {
            Some(commitment) => commitment,
            None => return false,
        };
        if self.verification_key.is_registered(registration) {
            let result = self.signature.verify(&commitment, &self.verification_key);
            return result == BLST_ERROR::BLST_SUCCESS;
        };
        false
    }

    /// Return the hash of the signature and its public key
    /// This function is used to create the `prover_set` of Alba protocol.
    pub fn to_element<const N: usize>(&self) -> [u8; N] {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut element = [0u8; N];

        hasher.update(&self.signature.to_bytes());
        hasher.update(&self.verification_key.to_bytes());
        hasher.finalize_variable(&mut element).unwrap();

        element
    }
}

#[allow(unsafe_code)]
mod unsafe_helpers {
    use super::*;
    use blst::{
        blst_p1_affine, blst_p1_from_affine, blst_p1_to_affine, blst_p2_affine,
        blst_p2_from_affine, blst_p2_to_affine,
    };

    pub(crate) fn vk_from_p2_affine(vk: VerificationKey) -> blst_p2 {
        unsafe {
            let mut projective_p2 = blst_p2::default();
            blst_p2_from_affine(
                &mut projective_p2,
                &std::mem::transmute::<VerificationKey, blst_p2_affine>(vk),
            );
            projective_p2
        }
    }
    pub(crate) fn sig_to_p1(sig: Signature) -> blst_p1 {
        unsafe {
            let mut projective_p1 = blst_p1::default();
            blst_p1_from_affine(
                &mut projective_p1,
                &std::mem::transmute::<Signature, blst_p1_affine>(sig),
            );
            projective_p1
        }
    }

    pub(crate) fn p2_affine_to_vk(grouped_vks: &blst_p2) -> VerificationKey {
        unsafe {
            let mut affine_p2 = blst_p2_affine::default();
            blst_p2_to_affine(&mut affine_p2, grouped_vks);
            std::mem::transmute::<blst_p2_affine, VerificationKey>(affine_p2)
        }
    }
    pub(crate) fn p1_affine_to_sig(grouped_sigs: &blst_p1) -> Signature {
        unsafe {
            let mut affine_p1 = blst_p1_affine::default();
            blst_p1_to_affine(&mut affine_p1, grouped_sigs);
            std::mem::transmute::<blst_p1_affine, Signature>(affine_p1)
        }
    }
}
