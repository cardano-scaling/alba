//! Centralized Telescope example with BLS signatures

mod simple_aggregate_signature;

use crate::simple_aggregate_signature::signature::Signature;
use crate::simple_aggregate_signature::signer::Signer;
use crate::simple_aggregate_signature::threshold_signature::ThresholdSignature;
use alba::centralized_telescope::params::Params;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use std::collections::HashMap;

const DATA_LENGTH: usize = 48;
pub(crate) type Element = [u8; DATA_LENGTH];

fn main() {
    let mut rng = ChaCha20Rng::from_seed(Default::default());
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);
    let set_size = 1000;
    let params = Params {
        soundness_param: 10.0,
        completeness_param: 10.0,
        set_size: 80 * set_size / 100,
        lower_bound: 20 * set_size / 100,
    };

    let mut key_list = HashMap::with_capacity(set_size as usize);
    let mut signature_list: Vec<Signature> = Vec::with_capacity(set_size as usize);

    for i in 0..set_size as usize {
        let signer = Signer::new(&mut rng);
        key_list.insert(i, signer.verification_key);
        signature_list.push(signer.sign(&msg, i));
    }
    let threshold_signature = ThresholdSignature::aggregate(&signature_list, &params, &key_list);

    print!("{:?}", threshold_signature.verify(&msg, &params));
}
