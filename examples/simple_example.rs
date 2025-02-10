//! Centralized Telescope example with BLS signatures

mod simple_threshold_signature;

use crate::simple_threshold_signature::signature::Signature;
use crate::simple_threshold_signature::signer::Signer;
use crate::simple_threshold_signature::threshold_signature::ThresholdSignature;
use alba::centralized_telescope::Telescope;
use blst::min_sig::PublicKey;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

fn main() {
    let mut rng = ChaCha20Rng::from_seed(Default::default());
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    println!("\n-------------- ALBA with Multi-Signature ---------------");
    println!("--------------------------------------------------------");

    // Define telescope parameters
    let nb_elements: u64 = 1_000;
    let soundness_param = 128.0;
    let completeness_param = 128.0;
    let set_size = nb_elements.saturating_mul(80).div_ceil(100);
    let lower_bound = nb_elements.saturating_mul(20).div_ceil(100);

    println!(" - Soundness parameter: {soundness_param}");
    println!(" - Completeness parameter: {completeness_param}");
    println!(" - Prover set size: {set_size}");
    println!(" - Lower bound: {lower_bound}");

    // Create the telescope structure
    let alba = Telescope::create(soundness_param, completeness_param, set_size, lower_bound);

    let mut public_key_list: Vec<(usize, PublicKey)> = Vec::with_capacity(nb_elements as usize);
    let mut signature_list: Vec<Signature> = Vec::with_capacity(set_size as usize);

    // Generate `nb_elements` signers and `set_size` signatures
    for i in 0..nb_elements as usize {
        let signer = Signer::new(&mut rng);
        public_key_list.push((i, signer.verification_key));
        if i < set_size as usize {
            signature_list.push(signer.sign(&msg, i));
        }
    }
    println!("--------------------------------------------------------");
    println!(" -- {nb_elements} (sk, pk) are generated.");
    println!(" -- {set_size} signatures are generated.");

    println!("--------------------------------------------------------");
    println!("----------- Generating Alba multi-signature. -----------");
    let (threshold_signature, indices) =
        ThresholdSignature::aggregate(&signature_list, &alba, &public_key_list);
    println!("-- Alba multi-signature is generated.");
    println!("--------------------------------------------------------");
    println!("----------- Verifying Alba multi-signature. ------------");

    if threshold_signature.verify(&msg, &alba, &public_key_list, &indices) {
        println!("-- Verification successful.");
    } else {
        println!("-- Verification failed.");
    }
    println!("--------------------------------------------------------");
}
