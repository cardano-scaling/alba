use digest::{Digest, FixedOutput, FixedOutputReset, Output, OutputSizeUser, Reset};
use sha2::Sha512;

pub struct DomainedHash<H> {
    pub domain_separation : [u8; 32],
    pub hash : H
}

impl<H: Digest + FixedOutput + OutputSizeUser> DomainedHash<H> {
    fn new_with_domain(domain_separation: [u8;32]) -> Self {
        Self {
            domain_separation,
            hash: H::new().update(domain_separation)
        }
    }
}

impl<H: Digest + FixedOutput + OutputSizeUser + Reset> Digest for DomainedHash<H> {
    // Required methods
    fn new() -> Self {
        Self {
            domain_separation : [0u8; 32],
            hash: H::new().update([0u8; 32])
        }
    }

    fn new_with_prefix(data: impl AsRef<[u8]>) -> Self{
        Self {
            domain_separation : [0u8; 32],
            hash: H::new_with_prefix(data)
        }
    }

    fn update(&mut self, data: impl AsRef<[u8]>) {
        self.hash.update(data)
    }

    fn chain_update(self, data: impl AsRef<[u8]>) -> Self {
        Self {
            domain_separation: self.domain_separation,
            hash: self.hash.chain_update(data)
        }
    }

    fn finalize(self) -> Output<Self> {
        self.hash.finalize()
    }
    
    fn finalize_into(self, out: &mut Output<Self>) {
        self.hash.finalize_into(out)
    }

    fn finalize_reset(&mut self) -> Output<Self>
       where Self: FixedOutputReset {
        self.hash.finalize_reset()
       }

    fn finalize_into_reset(&mut self, out: &mut Output<Self>)
       where Self: FixedOutputReset {
        self.hash.finalize_into_reset(out);
       }

    fn reset(&mut self)
       where Self: Reset {
        self.hash.reset()
       }

    fn output_size() -> usize {
        <H as Digest>::output_size()
    }

    fn digest(data: impl AsRef<[u8]>) -> Output<Self> {
        self.hash.digest(data)
    }

}
