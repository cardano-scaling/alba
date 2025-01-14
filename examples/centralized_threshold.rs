//! Centralized Telescope example with BLS multi-signature
use crate::aggregate_signature::helpers::{
    collect_valid_signatures, get_commitment, validate_signatures,
};
use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use crate::aggregate_signature::signer::{RegisteredSigner, Signer};
use alba::centralized_telescope::params::Params;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::CentralizedTelescope;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

mod aggregate_signature;
const DATA_LENGTH: usize = 48;
pub(crate) type Element = [u8; DATA_LENGTH];

#[derive(Debug, Clone)]
pub(crate) struct AlbaThresholdSignature {
    /// Centralized telescope proof
    pub(crate) proof: Proof,
    /// Registration indices of the element sequence signers
    pub(crate) indices: Vec<usize>,
    /// Commitment `Hash(checksum || msg)`
    pub(crate) commitment: Vec<u8>,
}

impl AlbaThresholdSignature {
    /// Create AlbaThresholdSignature. Validate and collect signatures in byte representation.
    /// Create Alba proof and extract indices of proof elements.
    /// Return proof, commitment, and indices.
    fn prove<const N: usize>(
        params: &Params,
        signature_list: &[IndividualSignature],
        registration: &Registration,
        msg: &[u8],
        set_size: usize,
    ) -> Option<Self> {
        // Ensure that the commitment can be created
        let checksum = match &registration.checksum {
            Some(checksum) => checksum,
            None => {
                println!("Error: Registration is not closed.");
                return None;
            }
        };

        let commitment = get_commitment::<N>(checksum, msg).to_vec();

        // Collect valid individual signatures' byte representation into a hashmap
        let valid_signatures = collect_valid_signatures::<N>(signature_list, registration, msg);

        // Check if there are enough valid signatures
        if valid_signatures.len() < set_size {
            println!(
                "Error: Not enough signatures! Expected at least {}, but got {}.",
                set_size,
                valid_signatures.len()
            );
            return None;
        }

        // Collect the byte representation of valid signatures into a Vec
        let prover_set: Vec<Element> = valid_signatures.keys().cloned().collect();

        // Create the Alba proof using the prover set
        let alba = CentralizedTelescope::create(params);
        let proof = alba.prove(&prover_set)?;

        // Extract the registration indices of the elements that form the proof element sequence
        let indices: Vec<usize> = proof
            .element_sequence
            .iter()
            .filter_map(|element| valid_signatures.get(element.as_slice()).copied())
            .collect();

        // Return the constructed AlbaThresholdSignature
        Some(Self {
            indices,
            proof,
            commitment,
        })
    }

    /// Verify AlbaThresholdSignature. Validate individual signatures and verify Alba proof.
    fn verify<const N: usize>(
        &self,
        params: &Params,
        registration: &Registration,
        msg: &[u8],
    ) -> bool {
        // Check if the checksum exists in the registration
        let checksum = match &registration.checksum {
            Some(checksum) => checksum,
            None => {
                println!("Error: Registration is not closed.");
                return false;
            }
        };

        // Compute the commitment and compare with the stored commitment
        let commitment = get_commitment::<N>(checksum, msg).to_vec();
        if commitment != self.commitment {
            println!("Error: Commitment mismatch.");
            return false;
        }

        // Validate the individual signatures
        if !validate_signatures(self, registration, &commitment) {
            println!("Error: Signature validation failed.");
            return false;
        }

        // Create the Alba proof and verify it
        let alba = CentralizedTelescope::create(params);
        let result = alba.verify(&self.proof);

        if result {
            println!("Success: Alba proof verification passed.");
        } else {
            println!("Error: Alba proof verification failed.");
        }

        result
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

    // Initialize signers
    let signers: Vec<Signer> = (0..set_size as usize)
        .map(|_| Signer::init(&mut rng))
        .collect();

    // Start new registration process
    let mut registration = Registration::new();

    // Register signers
    for (index, signer) in signers.iter().enumerate() {
        registration.register(signer.verification_key, index);
    }
    // Close registration to compute the checksum
    registration.close::<DATA_LENGTH>();

    // Filter out registered signers and create RegisteredSigner instances
    let registered_signers: Vec<RegisteredSigner> = signers
        .iter()
        .filter_map(|signer| signer.new_signer::<DATA_LENGTH>(&registration))
        .collect();

    // Create individual signatures
    let signature_list: Vec<IndividualSignature> = registered_signers
        .iter()
        .map(|signer| signer.sign::<DATA_LENGTH>(&msg))
        .collect();

    // Generate AlbaThresholdSignature proof
    if let Some(alba_threshold_signature) = AlbaThresholdSignature::prove::<DATA_LENGTH>(
        &params,
        &signature_list,
        &registration,
        &msg,
        set_size as usize,
    ) {
        // Verify the proof
        alba_threshold_signature.verify::<DATA_LENGTH>(&params, &registration, &msg);
    }
}
