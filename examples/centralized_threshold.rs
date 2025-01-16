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
    ) -> Option<Self> {
        // Collect valid individual signatures' byte representation into a hashmap
        let valid_signatures = collect_valid_signatures::<N>(signature_list, registration, msg);
        println!("-- Collected {} valid signatures. ", valid_signatures.len());
        // Check if there are enough valid signatures
        if valid_signatures.len() < params.set_size as usize {
            println!(
                    "Error: Not enough valid signatures to create an ATS! Expected at least {} signatures, but got {}.",
                    params.set_size,
                    valid_signatures.len()
                );
            return None;
        }
        // Collect the byte representation of valid signatures into a Vec
        let prover_set: Vec<Element> = valid_signatures.keys().copied().collect();

        // Initialise Alba with the parameters and generates a proof using the prover set
        let alba = CentralizedTelescope::create(params);
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

        // Return the constructed AlbaThresholdSignature
        Some(Self { proof, indices })
    }

    /// Verify AlbaThresholdSignature. Validate individual signatures and verify Alba proof.
    fn verify<const N: usize>(
        &self,
        params: &Params,
        registration: &Registration,
        msg: &[u8],
    ) -> bool {
        // Ensure that the registration is closed, and retrieve the correct checksum
        let Some(checksum) = &registration.checksum else {
            println!("Error: Registration is not closed.");
            return false;
        };

        // Compute the commitment
        let commitment = get_commitment::<N>(checksum, msg).to_vec();

        println!("-- Validating proof elements. ");
        // Aggregate the signatures and verify them at once
        if !validate_signatures(self, registration, &commitment) {
            println!("Error: Signature validation failed.");
            return false;
        }
        println!(
            "-- {} proof elements are validated. ",
            self.proof.element_sequence.len()
        );

        // Initialise Alba with the parameters and generates a proof using the prover set
        let alba = CentralizedTelescope::create(params);
        println!("-- Verifying alba proof. ");
        let result = alba.verify(&self.proof);

        if result {
            println!("-- Success: Alba proof verification passed.");
        } else {
            println!("Error: Alba proof verification failed.");
        }
        result
    }
}

fn main() {
    let mut rng = ChaCha20Rng::from_seed(Default::default());
    let sentence = "ALBA Rocks!";
    let msg = sentence.as_bytes();

    println!("\n--------------- ALBA Threshold Signature ---------------");
    println!("--------------------------------------------------------");

    println!("-- Telescope parameters:");
    let set_size = 1_000;
    let params = Params {
        soundness_param: 10.0,
        completeness_param: 10.0,
        set_size: 80 * set_size / 100,
        lower_bound: 20 * set_size / 100,
    };
    println!(" - Soundness parameter: {}", params.soundness_param);
    println!(" - Completeness parameter: {}", params.completeness_param);
    println!(" - Prover set size: {}", params.set_size);
    println!(" - Lower bound: {}", params.lower_bound);

    // Initialize signers
    println!("--------------------------------------------------------");
    let signers: Vec<Signer> = (0..set_size as usize)
        .map(|_| Signer::init(&mut rng))
        .collect();
    println!("-- {} signers initialized.", signers.len());

    // Start new registration process
    let mut registration = Registration::new();
    println!("-- Registration is opened.");

    // Register signer candidates and create new registered signers.
    let mut registered_signers: Vec<RegisteredSigner> = signers
        .iter()
        .take(rng.gen_range(950..1000))
        .filter_map(|signer| signer.register(&mut registration))
        .collect();
    println!("-- {} signers are registered.", registered_signers.len());

    // Close the registration process
    registration.close::<DATA_LENGTH>();
    println!("-- Registration is closed.");

    // Update registered signers with the checksum of closed registration
    for registered_signer in &mut registered_signers {
        registered_signer.get_closed_registration(&registration);
    }
    println!(
        "-- {} signers get the closed registration.",
        registered_signers.len()
    );

    // Create individual signatures
    let signature_list: Vec<IndividualSignature> = registered_signers
        .iter()
        .take(rng.gen_range(900..950))
        .filter_map(|signer| signer.sign::<DATA_LENGTH>(&msg))
        .collect();
    println!("-- {} signatures generated.", signature_list.len());

    println!("--------------------------------------------------------");
    println!("--------- Generating Alba threshold signature. ---------");
    // Generate AlbaThresholdSignature proof
    if let Some(alba_threshold_signature) =
        AlbaThresholdSignature::prove::<DATA_LENGTH>(&params, &signature_list, &registration, &msg)
    {
        println!("-- Alba threshold signature is generated.");
        println!("--------------------------------------------------------");
        println!("--------- Verifying Alba threshold signature. ----------");
        // Verify the proof
        alba_threshold_signature.verify::<DATA_LENGTH>(&params, &registration, &msg);
        println!("--------------------------------------------------------");
    }
}

// Some basic tests
#[cfg(test)]
mod tests {
    use super::*;
    use rand_core::RngCore;
    #[test]
    fn test_collect_valid_signatures_no_closed_reg() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let mut msg = [0u8; 16];
        rng.fill_bytes(&mut msg);
        let set_size = 100;

        let signers: Vec<Signer> = (0..set_size as usize)
            .map(|_| Signer::init(&mut rng))
            .collect();

        let mut registration = Registration::new();
        let mut registered_signers: Vec<RegisteredSigner> = signers
            .iter()
            .filter_map(|signer| signer.register(&mut registration))
            .collect();

        registration.close::<DATA_LENGTH>();
        for registered_signer in &mut registered_signers {
            registered_signer.get_closed_registration(&registration);
        }

        let signature_list: Vec<IndividualSignature> = registered_signers
            .iter()
            .filter_map(|signer| signer.sign::<DATA_LENGTH>(&msg))
            .collect();

        registration.checksum = None;

        let valid_signatures =
            collect_valid_signatures::<DATA_LENGTH>(&signature_list, &registration, &msg);

        println!("{}", valid_signatures.len());
    }

    #[test]
    fn test_collect_valid_signatures_unregistered_signer() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let mut msg = [0u8; 16];
        rng.fill_bytes(&mut msg);
        let set_size = 100;

        let signers: Vec<Signer> = (0..set_size as usize)
            .map(|_| Signer::init(&mut rng))
            .collect();

        let mut registration = Registration::new();
        let mut registered_signers: Vec<RegisteredSigner> = signers
            .iter()
            .filter_map(|signer| signer.register(&mut registration))
            .collect();

        registration.close::<DATA_LENGTH>();
        for registered_signer in &mut registered_signers {
            registered_signer.get_closed_registration(&registration);
        }
        registered_signers[99].index = 999;

        let signature_list: Vec<IndividualSignature> = registered_signers
            .iter()
            .filter_map(|signer| signer.sign::<DATA_LENGTH>(&msg))
            .collect();

        let valid_signatures =
            collect_valid_signatures::<DATA_LENGTH>(&signature_list, &registration, &msg);

        println!("{}", valid_signatures.len());
    }

    #[test]
    fn test_sign_without_checksum() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let mut msg = [0u8; 16];
        rng.fill_bytes(&mut msg);
        let signer = Signer::init(&mut rng);
        let mut registration = Registration::new();
        let registered_signer = signer.register(&mut registration).unwrap();
        assert!(registered_signer.sign::<DATA_LENGTH>(&msg).is_none());
    }

    #[test]
    fn test_get_checksum_before_closed() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let set_size = 10;
        let signers: Vec<Signer> = (0..set_size as usize)
            .map(|_| Signer::init(&mut rng))
            .collect();
        let mut registration = Registration::new();

        let mut registered_signers: Vec<RegisteredSigner> = signers
            .iter()
            .filter_map(|signer| signer.register(&mut registration))
            .collect();
        registered_signers[0].get_closed_registration(&registration);
        println!("{:?}", registered_signers[0].checksum);
    }

    #[test]
    fn test_duplicate_registering() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let set_size = 10;
        let signers: Vec<Signer> = (0..set_size as usize)
            .map(|_| Signer::init(&mut rng))
            .collect();
        let mut registration = Registration::new();

        let _registered_signers: Vec<RegisteredSigner> = signers
            .iter()
            .filter_map(|signer| signer.register(&mut registration))
            .collect();

        assert!(signers[0].register(&mut registration).is_none());
    }

    #[test]
    fn test_register_closed_registration() {
        let mut rng = ChaCha20Rng::from_seed(Default::default());
        let set_size = 10;
        let signers: Vec<Signer> = (0..set_size as usize)
            .map(|_| Signer::init(&mut rng))
            .collect();
        let mut registration = Registration::new();

        let _registered_signers: Vec<RegisteredSigner> = signers
            .iter()
            .filter_map(|signer| signer.register(&mut registration))
            .collect();

        registration.close::<DATA_LENGTH>();
        let signer = Signer::init(&mut rng);
        assert!(signer.register(&mut registration).is_none());
    }
}
