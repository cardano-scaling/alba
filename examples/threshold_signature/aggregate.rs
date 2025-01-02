use crate::threshold_signature::registration::ClosedRegistration;
use crate::threshold_signature::signature::IndividualSignature;
use crate::Element;

/// Aggregate signature storing the list of valid signatures and the hash of commitment with the message to be signed.
#[derive(Debug, Clone)]
pub(crate) struct AggregateSignature {
    pub(crate) valid_signatures: Vec<IndividualSignature>,
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
        closed_registration: &ClosedRegistration,
        msg: &[u8],
        set_size: u64,
    ) -> Option<Self> {
        let commitment: [u8; N] =
            ClosedRegistration::get_commitment(&closed_registration.check_sum, msg);
        let valid_signatures = AggregateSignature::collect_valid_signatures::<N>(
            signatures,
            closed_registration,
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
        closed_registration: &ClosedRegistration,
        commitment: &[u8],
    ) -> Vec<IndividualSignature> {
        signatures
            .iter()
            .filter(|sig| sig.verify::<N>(commitment, closed_registration))
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
