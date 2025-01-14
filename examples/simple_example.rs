//! Centralized Telescope example with BLS signatures

use alba::centralized_telescope::params::Params;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::CentralizedTelescope;
use blst::min_sig::{AggregatePublicKey, AggregateSignature, PublicKey, SecretKey, Signature};
use blst::BLST_ERROR;
use rand_chacha::ChaCha20Rng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use std::collections::HashMap;

const DATA_LENGTH: usize = 48;
pub(crate) type Element = [u8; DATA_LENGTH];

struct AlbaSigner {
    signing_key: SecretKey,
    verification_key: PublicKey,
}

struct ThresholdSignature {
    proof: Proof,
    key_list: Vec<PublicKey>,
}

impl AlbaSigner {
    fn new(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        let sk = SecretKey::key_gen(&ikm, &[])
            .expect("Error occurs when the length of ikm < 32. This will not happen here.");
        let pk: PublicKey = sk.sk_to_pk();
        Self {
            signing_key: sk,
            verification_key: pk,
        }
    }

    fn sign<const N: usize>(&self, msg: &[u8]) -> [u8; N] {
        let mut signature_to_byte = [0u8; N];
        signature_to_byte.copy_from_slice(&self.signing_key.sign(msg, &[], &[]).to_bytes());
        signature_to_byte
    }
}

impl ThresholdSignature {
    fn aggregate<const N: usize>(
        alba_signatures: &HashMap<Element, usize>,
        params: &Params,
        key_list: &HashMap<usize, PublicKey>,
    ) -> Self {
        let prover_set: Vec<Element> = alba_signatures.keys().copied().collect();
        let alba = CentralizedTelescope::create(params);
        let proof = alba.prove(&prover_set).unwrap();
        let signatures = proof.element_sequence.clone();
        let mut public_keys = Vec::with_capacity(signatures.len());

        for sig in signatures {
            public_keys.push(
                *key_list
                    .get(alba_signatures.get(sig.as_slice()).unwrap())
                    .unwrap(),
            );
        }
        Self {
            proof,
            key_list: public_keys,
        }
    }

    /// Validates individual signatures in the threshold signature
    fn validate_signatures(&self, msg: &[u8]) -> bool {
        let mut signatures = Vec::with_capacity(self.proof.element_sequence.len());
        for sig_bytes in &self.proof.element_sequence {
            if let Ok(signature) = Signature::from_bytes(sig_bytes.as_slice()) {
                signatures.push(signature);
            } else {
                println!("Error: Failed to parse signature from bytes.");
                return false;
            }
        }
        let signature_refs: Vec<&Signature> = signatures.iter().collect();
        let aggregate_signature =
            if let Ok(agg_sig) = AggregateSignature::aggregate(signature_refs.as_slice(), false) {
                agg_sig.to_signature()
            } else {
                println!("Error: Failed to aggregate signatures.");
                return false;
            };

        let public_key_refs: Vec<&PublicKey> = self.key_list.iter().collect();
        let aggregate_public_key =
            if let Ok(agg_pk) = AggregatePublicKey::aggregate(public_key_refs.as_slice(), false) {
                agg_pk.to_public_key()
            } else {
                println!("Error: Failed to aggregate public keys.");
                return false;
            };

        let result =
            aggregate_signature.verify(false, &msg, &[], &[], &aggregate_public_key, false);
        result == BLST_ERROR::BLST_SUCCESS
    }

    fn verify(&self, msg: &[u8], params: &Params) -> bool {
        if self.validate_signatures(msg) {
            let alba = CentralizedTelescope::create(params);
            return alba.verify(&self.proof);
        }
        false
    }
}

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
    let mut signature_list = HashMap::with_capacity(set_size as usize);

    for i in 0..set_size as usize {
        let signer = AlbaSigner::new(&mut rng);
        key_list.insert(i, signer.verification_key);
        signature_list.insert(signer.sign::<DATA_LENGTH>(&msg), i);
    }
    let threshold_signature =
        ThresholdSignature::aggregate::<DATA_LENGTH>(&signature_list, &params, &key_list);

    print!("{:?}", threshold_signature.verify(&msg, &params));
}
