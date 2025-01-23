//! Centralized Telescope example with BLS multi-signature
use crate::aggregate_signature::helpers::{
    collect_valid_signatures, get_commitment, validate_signatures,
};
use crate::aggregate_signature::registration::Registration;
use crate::aggregate_signature::signature::IndividualSignature;
use crate::aggregate_signature::signer::Signer;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::Telescope;
use rand::Rng;
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;
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
        alba: &Telescope,
        signature_list: &[IndividualSignature],
        registration: &Registration,
        msg: &[u8],
    ) -> Option<Self> {
        if let Some(checksum) = &registration.checksum {
            // Collect valid individual signatures' byte representation into a hashmap
            let valid_signatures = collect_valid_signatures::<N>(signature_list, registration, msg);
            println!("-- Collected {} valid signatures. ", valid_signatures.len());

            // Check if there are enough valid signatures
            if valid_signatures.len() < alba.get_set_size() as usize {
                println!(
                    "Error: Not enough valid signatures to create an ATS! Expected at least {} signatures, but got {}.",
                    alba.get_set_size(),
                    valid_signatures.len()
                );
                return None;
            }

            // Collect the byte representation of valid signatures into a Vec
            let prover_set: Vec<Element> = valid_signatures.keys().copied().collect();

            println!("-- Creating alba proof. ");
            let proof = alba.prove(&prover_set)?;
            println!("-- Alba proof created: ");
            println!(
                " - Numbers of retries done to find the proof: {}",
                proof.retry_counter
            );
            println!(
                " - Index of the searched subtree to find the proof: {}",
                proof.search_counter
            );
            println!(
                " - Number of elements in the proof sequence: {}",
                proof.element_sequence.len()
            );

            // Extract the registration indices of the elements that form the proof element sequence
            let indices: Vec<usize> = proof
                .element_sequence
                .iter()
                .filter_map(|element| valid_signatures.get(element.as_slice()).copied())
                .collect();

            let commitment = get_commitment::<N>(checksum, msg).to_vec();

            // Return the constructed AlbaThresholdSignature
            Some(Self {
                proof,
                indices,
                commitment,
            })
        } else {
            println!("Error: Registration is not closed.");
            None
        }
    }

    /// Verify AlbaThresholdSignature. Validate individual signatures and verify Alba proof.
    fn verify<const N: usize>(
        &self,
        alba: &Telescope,
        registration: &Registration,
        msg: &[u8],
    ) -> bool {
        if let Some(checksum) = &registration.checksum {
            let commitment = get_commitment::<N>(checksum, msg).to_vec();

            if commitment != self.commitment {
                println!("Error: Commitment mismatch.");
                return false;
            }

            println!("-- Validating proof elements. ");
            // Verify the signers were registered, aggregate the signatures and verify them at once
            if !validate_signatures(self, registration, &commitment) {
                println!("Error: Signature validation failed.");
                return false;
            }
            println!(
                "-- {} proof elements are validated. ",
                self.proof.element_sequence.len()
            );

            println!("-- Verifying alba proof. ");
            let result = alba.verify(&self.proof);

            if result {
                println!("-- Success: Alba proof verification passed.");
            } else {
                println!("Error: Alba proof verification failed.");
            }
            result
        } else {
            println!("Error: Registration is not closed.");
            false
        }
    }
}

fn main() {
    // Initialize RNG
    let mut rng = ChaCha20Rng::from_seed(Default::default());
    let sentence = "ALBA Rocks!";
    let msg = sentence.as_bytes();

    println!("\n--------------- ALBA Threshold Signature ---------------");
    println!("--------------------------------------------------------");

    // Telescope parameters
    println!("-- Telescope parameters:");
    let nb_signers: u64 = 3_000;
    let nb_elements: u64 = 1_000;
    let soundness_param = 128.0;
    let completeness_param = 128.0;
    let set_size = nb_elements.saturating_mul(80).div_ceil(100);
    let lower_bound = nb_elements.saturating_mul(50).div_ceil(100);
    let alba = Telescope::create(soundness_param, completeness_param, set_size, lower_bound);

    println!(" - Soundness parameter: {soundness_param}");
    println!(" - Completeness parameter: {completeness_param}");
    println!(" - Prover set size: {set_size}");
    println!(" - Lower bound: {lower_bound}");

    // Initialize signers
    println!("--------------------------------------------------------");
    let mut signers: Vec<Signer> = (0..nb_signers as usize)
        .map(|_| Signer::init(&mut rng))
        .collect();
    println!("-- {} signers initialized.", signers.len());

    // Start new registration process
    let mut registration = Registration::new();
    println!("-- Registration is opened.");

    // Register signer candidates
    let mut registered_count: u64 = 0;
    let register_range = nb_elements;
    for signer in signers.iter_mut().take(register_range as usize) {
        if signer.register(&mut registration) {
            registered_count = registered_count.saturating_add(1);
        }
    }
    println!("-- {registered_count} signers are registered.");

    // Close the registration process
    registration.close::<DATA_LENGTH>();
    println!("-- Registration is closed.");

    for signer in signers.iter_mut().take(registered_count as usize) {
        signer.get_closed_registration(&registration);
    }

    // Create individual signatures
    let signature_range = rng.gen_range(set_size..registered_count);
    let signature_list: Vec<IndividualSignature> = signers
        .iter()
        .take(signature_range as usize)
        .filter_map(|signer| signer.sign::<DATA_LENGTH>(msg))
        .collect();
    println!("-- {} signatures generated.", signature_list.len());

    println!("--------------------------------------------------------");
    println!("--------- Generating Alba threshold signature. ---------");

    // Generate AlbaThresholdSignature proof
    if let Some(alba_threshold_signature) =
        AlbaThresholdSignature::prove::<DATA_LENGTH>(&alba, &signature_list, &registration, msg)
    {
        println!("-- Alba threshold signature is generated.");
        println!("--------------------------------------------------------");
        println!("--------- Verifying Alba threshold signature. ----------");

        // Verify the proof
        if alba_threshold_signature.verify::<DATA_LENGTH>(&alba, &registration, msg) {
            println!("-- Verification successful.");
        } else {
            println!("-- Verification failed.");
        }
        println!("--------------------------------------------------------");
    } else {
        println!("-- Failed to generate Alba threshold signature.");
    }
}

// Some basic tests
#[cfg(test)]
mod tests {
    use super::*;
    use rand_core::RngCore;

    // Error: Registration is not closed. Cannot verify signatures.
    #[test]
    fn test_collect_valid_signatures_no_closed_reg() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let mut msg = [0u8; 16];
        rng.fill_bytes(&mut msg);

        let set_size = 100;
        let mut signers: Vec<Signer> = (0..set_size).map(|_| Signer::init(&mut rng)).collect();

        let mut registration = Registration::new();
        for signer in &mut signers {
            signer.register(&mut registration);
        }

        registration.close::<DATA_LENGTH>();
        for signer in &mut signers {
            signer.get_closed_registration(&registration);
        }

        // Manually set an invalid index for the last signer
        signers[99].index = 999;

        // Collect signatures from all signers
        let signature_list: Vec<IndividualSignature> = signers
            .iter()
            .filter_map(|signer| signer.sign::<DATA_LENGTH>(&msg))
            .collect();

        // Invalidate the registration by removing the checksum
        registration.checksum = None;

        // Validate the collected signatures
        let valid_signatures =
            collect_valid_signatures::<DATA_LENGTH>(&signature_list, &registration, &msg);

        // Assert that the registration is not closed and no signatures are collected
        if valid_signatures.len() != 0 {
            println!(
                "Test failed: Expected 0 valid signatures, but got {}.",
                valid_signatures.len()
            );
        }
        assert_eq!(
            valid_signatures.len(),
            0,
            "Error: Registration is not closed. Cannot verify signatures."
        );
    }

    // Warning: No verification key found for index 999
    #[test]
    fn test_collect_valid_signatures_unregistered_signer() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let mut msg = [0u8; 16];
        rng.fill_bytes(&mut msg);

        let set_size = 100;
        let mut signers: Vec<Signer> = (0..set_size).map(|_| Signer::init(&mut rng)).collect();

        let mut registration = Registration::new();
        for signer in &mut signers {
            signer.register(&mut registration);
        }

        registration.close::<DATA_LENGTH>();
        for signer in &mut signers {
            signer.get_closed_registration(&registration);
        }

        // Manually set an invalid index for the last signer
        signers[99].index = 999;

        // Collect signatures from all signers
        let signature_list: Vec<IndividualSignature> = signers
            .iter()
            .filter_map(|signer| signer.sign::<DATA_LENGTH>(&msg))
            .collect();

        // Validate the collected signatures
        let valid_signatures =
            collect_valid_signatures::<DATA_LENGTH>(&signature_list, &registration, &msg);

        // Assert the number of valid signatures is one less than the total
        assert_eq!(
            valid_signatures.len(),
            set_size - 1,
            "Expected {} signatures, got {}.",
            set_size - 1,
            valid_signatures.len()
        );

        // Ensure the unregistered signer's signature is not included in valid signatures
        let invalid_index_bytes = 999u64.to_le_bytes(); // Convert the invalid index to bytes
        assert!(
            !valid_signatures.contains_key(&invalid_index_bytes[..]),
            "Unregistered signer's signature should not be included in valid signatures."
        );
    }

    #[test]
    fn test_sign_without_checksum() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let mut msg = [0u8; 16];
        rng.fill_bytes(&mut msg);
        let mut signer = Signer::init(&mut rng);
        let mut registration = Registration::new();
        signer.register(&mut registration);

        // Attempt to sign the message without a checksum and assert it returns None
        assert!(signer.sign::<DATA_LENGTH>(&msg).is_none());
    }

    //Error: Registration is not closed.
    #[test]
    fn test_get_checksum_before_closed() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let set_size = 10;
        let mut signers: Vec<Signer> = (0..set_size).map(|_| Signer::init(&mut rng)).collect();

        let mut registration = Registration::new();
        for signer in &mut signers {
            signer.register(&mut registration);
        }

        // Attempt to get the closed registration checksum (before it's closed)
        signers[0].get_closed_registration(&registration);

        // Assert that the checksum is still None
        assert!(
            signers[0].checksum.is_none(),
            "Checksum should remain None before registration is closed."
        );
    }

    // Error: Key already registered!
    #[test]
    fn test_duplicate_registering() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let set_size = 10;
        let mut signers: Vec<Signer> = (0..set_size).map(|_| Signer::init(&mut rng)).collect();

        let mut registration = Registration::new();
        for signer in &mut signers {
            signer.register(&mut registration);
        }

        // Attempt to re-register the first signer and assert it fails
        assert_eq!(signers[0].register(&mut registration), false);
    }

    // Error: Cannot register, registration is closed!
    #[test]
    fn test_register_closed_registration() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let set_size = 10;
        let mut signers: Vec<Signer> = (0..set_size).map(|_| Signer::init(&mut rng)).collect();

        let mut registration = Registration::new();
        for signer in &mut signers {
            signer.register(&mut registration);
        }
        registration.close::<DATA_LENGTH>();

        // Attempt to register a new signer after closure
        let mut new_signer = Signer::init(&mut rng);
        assert_eq!(new_signer.register(&mut registration), false);
    }
}
