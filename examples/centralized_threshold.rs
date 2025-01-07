//! Example

mod aggregate_signature;

use alba::centralized_telescope::params::Params;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::CentralizedTelescope;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use crate::aggregate_signature::signer::{RegisteredSigner, Signer};
use aggregate_signature::aggregate::AggregateSignature;

const DATA_LENGTH: usize = 32;
pub(crate) type Element = [u8; DATA_LENGTH];

/// Alba proof with aggregate signature
#[derive(Debug, Clone)]
pub struct AlbaThresholdSignature {
    /// Aggregate signature
    pub(crate) aggregate: AggregateSignature,
    /// Centralized telescope proof
    pub(crate) proof: Proof,
}

impl AlbaThresholdSignature {
    /// Create an Alba proof for given list of `IndividualSignature`s
    /// - Create the prover's set: Collect the set elements by converting each valid
    /// signature to an element
    /// - If the size of the prover's set is less than given `set_size`, abort
    /// - Create the alba proof
    /// - Produce the aggregate signature by using the subset of signatures whose
    /// element versions are included in the alba proof.
    pub(crate) fn prove<const N: usize>(
        params: &Params,
        signatures: &[IndividualSignature],
        set_size: u64,
        registration: &Registration,
        msg: &[u8],
    ) -> Option<Self> {
        let prover_set =
            AlbaThresholdSignature::collect_set_elements::<N>(signatures, registration, msg);
        if prover_set.len() < set_size as usize {
            println!("Not enough signatures.");
            return None;
        }
        let alba = CentralizedTelescope::create(params);
        alba.prove(&prover_set)
            .and_then(|alba_proof| {
                let commitment: [u8; N] = registration.get_commitment(msg)?;
                Some(Self {
                    aggregate: AggregateSignature::aggregate(
                        signatures,
                        &prover_set,
                        &alba_proof.element_sequence,
                        &commitment,
                    ),
                    proof: alba_proof,
                })
            })
            .or_else(|| None)
    }

    /// Verify each signature and convert the valid ones to elements
    fn collect_set_elements<const N: usize>(
        signatures: &[IndividualSignature],
        registration: &Registration,
        msg: &[u8],
    ) -> Vec<Element> {
        signatures
            .iter()
            .filter(|sig| sig.verify::<N>(registration, msg))
            .map(IndividualSignature::to_element)
            .collect()
    }

    /// Verify Alba threshold signature.
    /// Create the commitment by hashing the checksum of the closed registration and the message.
    /// If the computed commitment is different from the commitment of the given aggregate signature, abort.
    /// Verify aggregate signature
    /// Return true if the Alba proof is verified and all checks passed.
    pub(crate) fn verify<const N: usize>(
        &self,
        params: &Params,
        registration: &Registration,
        msg: &[u8],
    ) -> bool {
        let commitment: [u8; N] = match registration.get_commitment::<N>(msg) {
            Some(commitment) => commitment,
            None => return false,
        };
        if commitment != self.aggregate.commitment.as_slice() {
            return false;
        }

        if !self.aggregate.verify::<N>(registration) {
            return false;
        }
        let alba = CentralizedTelescope::create(params);
        alba.verify(&self.proof)
    }
}

fn main() {
    let mut rng = ChaCha20Rng::from_seed(Default::default());
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);
    let set_size = 1_000;
    let params = Params {
        soundness_param: 10.0,
        completeness_param: 10.0,
        set_size: 80 * set_size / 100,
        lower_bound: 20 * set_size / 100,
    };

    // Create a list of candidates (signers) of the size `set_size`
    let signers: Vec<Signer> = (0..set_size).map(|_| Signer::init(&mut rng)).collect();

    // Create a new key registration
    let mut registration = Registration::new();

    // Register the candidates
    for signer in &signers {
        registration.register(signer.verification_key);
    }
    registration.close::<DATA_LENGTH>();

    // Create the threshold signature signers from the candidates if they are registered
    let registered_signers: Vec<RegisteredSigner> = signers
        .into_iter()
        .filter_map(|candidate| candidate.new_signer::<DATA_LENGTH>(registration.clone()))
        .collect();

    // Collect the individual signatures, ignoring any that failed (None)
    let signatures = registered_signers
        .iter()
        .filter_map(|signer| signer.sign::<DATA_LENGTH>(&msg))
        .collect::<Vec<IndividualSignature>>();

    // Create the threshold signature.
    // - Aggregate the valid signatures
    // - Create the `prover_set` with the list of valid signatures
    // - Create the threshold signature out of the `prover_set`
    let result = AlbaThresholdSignature::prove::<DATA_LENGTH>(
        &params,
        &signatures,
        set_size,
        &registration,
        &msg,
    );
    if result.is_some() {
        let alba = result.unwrap();
        // Verify the proof
        let verify_result = alba.verify::<DATA_LENGTH>(&params, &registration, &msg);
        print!("Threshold signature verifies: {verify_result}");
    } else {
        println!("No threshold signature were successfully generated.");
    }
}
