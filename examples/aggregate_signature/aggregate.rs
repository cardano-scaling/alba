use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use crate::Element;

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
        set_size: u64,
    ) -> Option<Self> {
        let commitment: [u8; N] = Registration::get_commitment(&registration.check_sum, msg);
        let valid_signatures = AggregateSignature::collect_valid_signatures::<N>(
            signatures,
            registration,
            &commitment,
        );

        if valid_signatures.len() < set_size as usize {
            None
        } else {
            Some(Self {
                valid_signatures,
                commitment: commitment.to_vec(),
            })
        }
    }

    /// Collect the verified individual signatures
    pub fn collect_valid_signatures<const N: usize>(
        signatures: &[IndividualSignature],
        registration: &Registration,
        commitment: &[u8],
    ) -> Vec<IndividualSignature> {
        signatures
            .iter()
            .filter(|sig| sig.verify::<N>(commitment, registration))
            .cloned()
            .collect()
    }

    /// Create the prover set by running `to_element` function for each valid signature
    pub fn create_prover_set<const N: usize>(&self) -> Vec<Element> {
        self.valid_signatures
            .iter()
            .map(IndividualSignature::to_element)
            .collect()
    }
}
