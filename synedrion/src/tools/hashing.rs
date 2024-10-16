use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::vec::Vec;

use digest::{Digest, ExtendableOutput, Update};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use sha3::{Shake256, Shake256Reader};

use crate::curve::Scalar;
use crate::tools::serde_bytes;

/// A digest object that takes byte slices or decomposable ([`Hashable`]) objects.
pub trait Chain: Sized {
    /// Hash raw bytes.
    ///
    /// Note: only for impls in specific types, do not use directly.
    fn chain_raw_bytes(self, bytes: &[u8]) -> Self;

    /// Hash a bytestring that is known to be constant-sized
    /// (e.g. byte representation of a built-in integer).
    fn chain_constant_sized_bytes(self, bytes: &(impl AsRef<[u8]> + ?Sized)) -> Self {
        self.chain_raw_bytes(bytes.as_ref())
    }

    /// Hash raw bytes in a collision-resistant way.
    fn chain_bytes(self, bytes: &(impl AsRef<[u8]> + ?Sized)) -> Self {
        // Hash the length too to prevent hash conflicts. (e.g. H(AB|CD) == H(ABC|D)).
        // Not strictly necessary for fixed-size arrays, but it's easier to just always do it.
        let len = (bytes.as_ref().len() as u64).to_be_bytes();
        self.chain_raw_bytes(&len).chain_raw_bytes(bytes.as_ref())
    }

    fn chain<T: Hashable>(self, hashable: &T) -> Self {
        hashable.chain(self)
    }

    fn chain_type<T: HashableType>(self) -> Self {
        T::chain_type(self)
    }

    fn chain_slice<T: Hashable>(self, hashable: &[T]) -> Self {
        // Hashing the length too to prevent collisions.
        let len = hashable.len() as u64;
        let mut digest = self.chain(&len);
        for elem in hashable {
            digest = digest.chain(elem);
        }
        digest
    }
}

pub(crate) type BackendDigest = Sha256;

/// Wraps a fixed output hash for easier replacement, and standardizes the use of DST.
pub(crate) struct Hash(BackendDigest);

impl Chain for Hash {
    fn chain_raw_bytes(self, bytes: &[u8]) -> Self {
        Self(self.0.chain_update(bytes))
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub(crate) struct HashOutput(
    #[serde(with = "serde_bytes::as_hex")] [u8; 32], // Length of the BackendDigest output. Unfortunately we can't get it in compile-time.
);

impl AsRef<[u8]> for HashOutput {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl Hash {
    fn new() -> Self {
        Self(BackendDigest::new())
    }

    pub fn new_with_dst(dst: &[u8]) -> Self {
        Self::new().chain_bytes(dst)
    }

    pub(crate) fn finalize(self) -> HashOutput {
        HashOutput(self.0.finalize().into())
    }

    pub fn finalize_to_scalar(self) -> Scalar {
        Scalar::from_digest(self.0)
    }
}

/// Wraps an extendable output hash for easier replacement, and standardizes the use of DST.
pub struct XofHash(Shake256);

impl Chain for XofHash {
    fn chain_raw_bytes(self, bytes: &[u8]) -> Self {
        let mut digest = self.0;
        digest.update(bytes);
        Self(digest)
    }
}

impl XofHash {
    fn new() -> Self {
        Self(Shake256::default())
    }

    pub fn new_with_dst(dst: &[u8]) -> Self {
        Self::new().chain_bytes(dst)
    }

    pub fn finalize_to_reader(self) -> Shake256Reader {
        self.0.finalize_xof()
    }
}

/// A trait allowing hashing of types without having access to their instances.
pub trait HashableType {
    fn chain_type<C: Chain>(digest: C) -> C;
}

/// A trait allowing complex objects to give access to their contents for hashing purposes
/// without the need of a conversion to a new form (e.g. serialization).
pub trait Hashable {
    fn chain<C: Chain>(&self, digest: C) -> C;
}

// NOTE: we *do not* want to implement Hashable for `usize` to prevent hashes being different
// on different targets.
impl Hashable for u32 {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_constant_sized_bytes(&self.to_be_bytes())
    }
}

impl Hashable for u64 {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_constant_sized_bytes(&self.to_be_bytes())
    }
}

impl Hashable for u8 {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_constant_sized_bytes(&self.to_be_bytes())
    }
}

// TODO (#61): we use it for Vec<bool>. Inefficient, but works for now.
// Replace with packing boolean vectors into bytes, perhaps? Maybe there is a crate for that.
impl Hashable for bool {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_constant_sized_bytes(if *self { b"\x01" } else { b"\x00" })
    }
}

impl Hashable for Box<[u8]> {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_bytes(self)
    }
}

impl Hashable for &[u8] {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_bytes(self)
    }
}

impl<const N: usize> Hashable for [u8; N] {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_bytes(self)
    }
}

impl<T1: Hashable, T2: Hashable> Hashable for (&T1, &T2) {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain(self.0).chain(self.1)
    }
}

impl<T1: Hashable, T2: Hashable, T3: Hashable> Hashable for (&T1, &T2, &T3) {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain(self.0).chain(self.1).chain(self.2)
    }
}

impl<T: Hashable> Hashable for Vec<T> {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_slice(self)
    }
}

impl<K: Hashable, V: Hashable> Hashable for BTreeMap<K, V> {
    fn chain<C: Chain>(&self, digest: C) -> C {
        // Hashing the map length too to prevent collisions.
        let len = self.len() as u64;
        let mut digest = digest.chain(&len);
        // The iteration is ordered (by keys)
        for (key, value) in self {
            digest = digest.chain(key);
            digest = digest.chain(value);
        }
        digest
    }
}

impl Hashable for HashOutput {
    fn chain<C: Chain>(&self, digest: C) -> C {
        digest.chain_constant_sized_bytes(&self.0)
    }
}
