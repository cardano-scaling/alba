//! Example

// REMOVE!!!!!!!!!!!!!!!!
#![allow(dead_code)]

use alba::centralized_telescope::params::Params;
use alba::centralized_telescope::proof::Proof;
use alba::centralized_telescope::CentralizedTelescope;
use blake2::digest::{Update, VariableOutput};
use blake2::Blake2bVar;
use blst::min_sig::{PublicKey, SecretKey, Signature};
use blst::BLST_ERROR;
use rand_chacha::ChaCha20Rng;
use rand_core::{CryptoRng, RngCore, SeedableRng};
use std::collections::HashMap;

const DATA_LENGTH: usize = 32;
pub(crate) type Element = [u8; DATA_LENGTH];

/// Key pair including blst `SecretKey` and blst `PublicKey`
#[derive(Debug)]
pub struct Candidate {
    signing_key: SecretKey,
    /// Verification key
    pub verification_key: PublicKey,
}

/// Registered Alba signer. It includes signer's keypair, registration index and the commitment.
#[derive(Debug)]
pub struct Signer {
    signing_key: SecretKey,
    verification_key: PublicKey,
    signer_index: usize,
    registration_check_sum: Vec<u8>,
}

/// A hash table for storing BLS public keys with their indices
#[derive(Debug, Clone)]
pub struct RegisteredKeys {
    keys: HashMap<usize, Vec<u8>>,
}

/// Closed registration including registered keys and the commitment
#[derive(Debug, Clone)]
pub struct ClosedRegistration {
    registered_keys: RegisteredKeys,
    check_sum: Vec<u8>,
}

/// Individual Signature including blst `PublicKey` and blst `Signature`
#[derive(Debug, Clone)]
pub struct IndividualSignature {
    signature: Signature,
    verification_key: PublicKey,
    signer_index: usize,
}

/// Aggregate signature storing the list of valid signatures and the hash of commitment with the message to be signed.
#[derive(Debug, Clone)]
pub struct AggregateSignature {
    valid_signatures: Vec<IndividualSignature>,
    commitment: Vec<u8>,
}

/// Alba proof with aggregate signature
#[derive(Debug, Clone)]
pub struct AlbaThresholdProof {
    /// Aggregate signature
    pub aggregate: AggregateSignature,
    /// Centralized telescope proof
    pub proof: Proof,
}

impl Candidate {
    /// Generate a new candidate, i.e., a bls signer
    pub fn new(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let mut ikm = [0u8; 32];
        rng.fill_bytes(&mut ikm);
        let sk = SecretKey::key_gen(&ikm, &[])
            .expect("Error occurs when the length of ikm < 32. This will not happen here.");
        let vk = sk.sk_to_pk();
        Self {
            signing_key: sk,
            verification_key: vk,
        }
    }

    /// Extract the verification key.
    pub fn verification_key(&self) -> PublicKey {
        self.verification_key
    }

    /// Create a new signer from candidate and closed registration
    pub fn new_signer<const N: usize>(
        &self,
        closed_registration: &ClosedRegistration,
    ) -> Option<Signer> {
        closed_registration
            .registered_keys
            .get_index::<N>(&self.verification_key())
            .map(|index| Signer {
                signing_key: self.signing_key.clone(),
                verification_key: self.verification_key,
                signer_index: index,
                registration_check_sum: closed_registration.check_sum.clone(),
            })
    }
}

impl Signer {
    /// Create a bls signature for given message
    fn sign<const N: usize>(&self, msg: &[u8]) -> IndividualSignature {
        let commitment: [u8; N] =
            ClosedRegistration::get_commitment::<N>(&self.registration_check_sum, msg);

        IndividualSignature {
            signature: self.signing_key.sign(&commitment, &[], &[]),
            verification_key: self.verification_key,
            signer_index: self.signer_index,
        }
    }
}

impl RegisteredKeys {
    /// Creates a new hash table with a specified hash size
    pub fn new() -> Self {
        Self {
            keys: HashMap::new(),
        }
    }

    /// Inserts a list of public keys into the hash table
    pub fn insert_keys<const N: usize>(&mut self, keys: &[PublicKey]) {
        for (index, key) in keys.iter().enumerate() {
            let key_hash = RegisteredKeys::hash_key::<N>(key);
            self.keys.insert(index, key_hash);
        }
    }

    /// Insert a public key into the hash table
    pub fn insert_key<const N: usize>(&mut self, key: &PublicKey, index: usize) {
        let key_hash = RegisteredKeys::hash_key::<N>(key);
        self.keys.insert(index, key_hash);
    }

    /// Finds the index of the given public key, if it exists
    pub fn get_index<const N: usize>(&self, key: &PublicKey) -> Option<usize> {
        let key_hash = RegisteredKeys::hash_key::<N>(key);
        self.keys.iter().find_map(|(&index, stored_hash)| {
            if *stored_hash == key_hash {
                Some(index)
            } else {
                None
            }
        })
    }

    /// Checks if the hash of the given `PublicKey` matches the hash stored at the specified index
    pub fn is_key_at_index<const N: usize>(&self, key: &PublicKey, index: usize) -> bool {
        match self.keys.get(&index) {
            Some(stored_hash) => &RegisteredKeys::hash_key::<N>(key) == stored_hash,
            None => false, // Index not found
        }
    }

    /// Hashes a public key using Blake2b
    fn hash_key<const N: usize>(key: &PublicKey) -> Vec<u8> {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut hash_output = vec![0u8; N];

        hasher.update(&key.to_bytes());
        hasher.finalize_variable(&mut hash_output).unwrap();

        hash_output
    }
}

impl Default for RegisteredKeys {
    fn default() -> Self {
        Self::new()
    }
}

impl ClosedRegistration {
    /// Close the registration and create the hash of all registration data.
    pub fn close<const N: usize>(registration: &RegisteredKeys) -> Self {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut hash_output = vec![0u8; N];

        for reg in registration.clone().keys {
            hasher.update(&reg.1.clone());
        }
        hasher.finalize_variable(&mut hash_output).unwrap();

        Self {
            registered_keys: registration.clone(),
            check_sum: hash_output.clone(),
        }
    }

    /// Compute the commitment by hashing check sum of closed registration and the message
    pub fn get_commitment<const N: usize>(check_sum: &[u8], msg: &[u8]) -> [u8; N] {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut commitment: [u8; N] = [0u8; N];

        hasher.update(check_sum);
        hasher.update(msg);
        hasher.finalize_variable(&mut commitment).unwrap();
        commitment
    }
}

impl IndividualSignature {
    /// Verify a signature
    pub fn verify<const N: usize>(
        &self,
        commitment: &[u8],
        closed_registration: &ClosedRegistration,
    ) -> bool {
        if closed_registration
            .registered_keys
            .is_key_at_index::<N>(&self.verification_key, self.signer_index)
        {
            let result =
                self.signature
                    .verify(false, commitment, &[], &[], &self.verification_key, false);
            return result == BLST_ERROR::BLST_SUCCESS;
        };
        false
    }

    /// Return the hash of the signature and its public key
    pub fn to_element<const N: usize>(&self) -> [u8; N] {
        let mut hasher = Blake2bVar::new(N).expect("Invalid hash size");
        let mut element = [0u8; N];

        hasher.update(&self.signature.to_bytes());
        hasher.update(&self.verification_key.to_bytes());
        hasher.finalize_variable(&mut element).unwrap();

        element
    }
}

impl AggregateSignature {
    /// Collect valid individual signatures and create the commitment with message
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
        msg: &[u8],
    ) -> Vec<IndividualSignature> {
        signatures
            .iter()
            .filter(|sig| sig.verify::<N>(msg, closed_registration))
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

impl AlbaThresholdProof {
    /// Prove
    pub fn prove<const N: usize>(
        params: &Params,
        signatures: &[IndividualSignature],
        set_size: u64,
        closed_registration: &ClosedRegistration,
        msg: &[u8],
    ) -> Option<Self> {
        let try_aggregate =
            AggregateSignature::aggregate::<N>(signatures, closed_registration, msg, set_size);
        if let Some(aggregate) = try_aggregate {
            let prover_set: Vec<Element> = aggregate.create_prover_set::<N>();
            let alba = CentralizedTelescope::create(params);
            let try_proof = alba.prove(&prover_set);
            if let Some(proof) = try_proof {
                Some(Self { aggregate, proof })
            } else {
                println!("Proof generation failed.");
                None
            }
        } else {
            println!("Aggregation failed.");
            None
        }
    }

    /// Verify
    pub fn verify<const N: usize>(
        &self,
        params: &Params,
        closed_registration: &ClosedRegistration,
        msg: &[u8],
    ) -> bool {
        let commitment: [u8; N] =
            ClosedRegistration::get_commitment(&closed_registration.check_sum, msg);

        if commitment != self.aggregate.commitment.as_slice() {
            return false;
        }

        for sig in self.aggregate.valid_signatures.clone() {
            if !sig.verify::<N>(&commitment, closed_registration) {
                return false;
            }
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

    let mut candidates: Vec<Candidate> = Vec::with_capacity(set_size as usize);
    for _ in 0..set_size {
        candidates.push(Candidate::new(&mut rng));
    }
    let mut registration = RegisteredKeys::new();

    for (index, candidate) in candidates.iter().enumerate() {
        registration.insert_key::<DATA_LENGTH>(&candidate.verification_key, index);
    }

    let closed_registration = ClosedRegistration::close::<DATA_LENGTH>(&registration);

    let mut signers: Vec<Signer> = Vec::new();
    for candidate in candidates {
        match candidate.new_signer::<DATA_LENGTH>(&closed_registration) {
            Some(signer) => signers.push(signer),
            None => continue,
        }
    }
    let signatures = signers
        .iter()
        .map(|signer| signer.sign::<DATA_LENGTH>(&msg))
        .collect::<Vec<IndividualSignature>>();

    let result = AlbaThresholdProof::prove::<DATA_LENGTH>(
        &params,
        &signatures,
        set_size,
        &closed_registration,
        &msg,
    );
    if result.is_some() {
        let alba = result.unwrap();
        let verify_result = alba.verify::<DATA_LENGTH>(&params, &closed_registration, &msg);
        print!("{verify_result}");
    } else {
        println!("Proof is not generated.");
    }
}
