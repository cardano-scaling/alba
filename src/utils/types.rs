//! Types and implementation
use std::mem;

/// Digest size for internal hashes
pub(crate) const DIGEST_SIZE: usize = 32;

/// Hash type for internal hashes
pub(crate) type Hash = [u8; DIGEST_SIZE];

/// Trait to represent input as bytes
pub trait ToBytes {
    /// Reference on unsized array of u8
    type ByteArray: AsRef<[u8]>;
    /// Returns the byte representation of the element
    fn to_be_bytes(&self) -> Self::ByteArray;
}

// Implementing ToBytes for integer types
macro_rules! impl_ToBytes {
    (for $($t:ty),+) => {
        $(impl ToBytes for $t {
            type ByteArray = [u8; mem::size_of::<Self>()];

            fn to_be_bytes(&self) -> Self::ByteArray {
                return Self::to_be_bytes(*self);
            }
        })*
    }
}
impl_ToBytes!(for u8, u16, u32, u64, u128);
impl_ToBytes!(for i8, i16, i32, i64, i128);

// Implementing ToBytes for byte arrays
macro_rules! impl_u8ToBytes {
    (for $($t:ty),+) => {
        $(impl ToBytes for $t {
            type ByteArray = Self;

            fn to_be_bytes(&self) -> Self::ByteArray {
                *self
            }
        })*
    }
}
impl_u8ToBytes!(for [u8;32], [u8;48], [u8;64], [u8;128], [u8;448]);
