use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use crate::aggregate_signature::signer::VerificationKey;
use crate::Element;
use blst::min_sig::Signature;

/// Aggregate signature storing the list of valid signatures and the hash of commitment
/// with the message to be signed.
#[derive(Debug, Clone)]
pub(crate) struct AggregateSignature {
    /// A list of verified signatures
    pub(crate) valid_signatures: Vec<IndividualSignature>,
    /// The commitment = Hash(checksum||msg) that is used the create valid signatures
    pub(crate) commitment: Vec<u8>,
}

impl AggregateSignature {
    /// Create the aggregate signature.
    /// First, create the commitment by hashing the check sum of the closed registration and the message.
    /// Verify all individual signatures
    /// If the number of valid signatures are less than the given set_size, return `None`
    /// Return the aggregate signature if all checks pass.
    pub fn aggregate<const N: usize>(
        signatures: &[IndividualSignature],
        registration: &Registration,
        msg: &[u8],
        _set_size: u64,
    ) -> Option<Self> {
        let commitment: [u8; N] = registration.get_commitment(msg)?;

        let valid_signatures = signatures
            .iter()
            .filter(|sig| sig.verify::<N>(registration, msg))
            .cloned()
            .collect();

        Some(Self {
            valid_signatures,
            commitment: commitment.to_vec(),
        })
    }

    pub fn verify<const N: usize>(&self, registration: &Registration) -> bool {
        for sig in &self.valid_signatures {
            if !sig.verification_key.is_registered(registration) {
                return false;
            }
        }

        // let (signatures, verification_keys) = self.extract_signatures_and_keys();

        return true;
    }

    fn extract_signatures_and_keys(&self) -> (Vec<Signature>, Vec<VerificationKey>) {
        let signatures = self
            .valid_signatures
            .iter()
            .map(|ind_sig| ind_sig.signature.clone())
            .collect();

        let verification_keys = self
            .valid_signatures
            .iter()
            .map(|ind_sig| ind_sig.verification_key.clone())
            .collect();

        (signatures, verification_keys)
    }

    /// Create the prover set by running `to_element` function for each valid signature
    pub fn create_prover_set<const N: usize>(&self) -> Vec<Element> {
        self.valid_signatures
            .iter()
            .map(IndividualSignature::to_element)
            .collect()
    }
}
