use alloc::format;
use alloc::string::String;
use alloc::{vec, vec::Vec};
use core::default::Default;
use core::ops::{Add, Mul, Sub};

use digest::Digest;
use k256::elliptic_curve::group::ff::PrimeField;
use k256::elliptic_curve::{
    bigint::U256, // Note that this type is different from typenum::U256
    generic_array::typenum::marker_traits::Unsigned,
    generic_array::GenericArray,
    ops::Reduce,
    point::AffineCoordinates,
    sec1::{EncodedPoint, FromEncodedPoint, ModulusSize, ToEncodedPoint},
    subtle::CtOption,
    Field,
    FieldBytesSize,
    NonZeroScalar,
};
use k256::{ecdsa::VerifyingKey, Secp256k1};
use rand_core::CryptoRngCore;
use serde::{de::Error as SerdeDeError, Deserialize, Deserializer, Serialize, Serializer};

use crate::tools::hashing::{Chain, Hashable};

pub(crate) type BackendScalar = k256::Scalar;
pub(crate) type BackendPoint = k256::ProjectivePoint;
pub(crate) type CompressedPointSize =
    <FieldBytesSize<Secp256k1> as ModulusSize>::CompressedPointSize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Scalar(BackendScalar);

impl Scalar {
    pub const ZERO: Self = Self(BackendScalar::ZERO);
    pub const ONE: Self = Self(BackendScalar::ONE);

    pub fn random(rng: &mut impl CryptoRngCore) -> Self {
        Self(BackendScalar::random(rng))
    }

    pub fn random_nonzero(rng: &mut impl CryptoRngCore) -> Self {
        Self(*NonZeroScalar::<k256::Secp256k1>::random(rng).as_ref())
    }

    pub fn random_in_range_j(rng: &mut impl CryptoRngCore) -> Self {
        // TODO: find out what the range `\mathcal{J}` is.
        Self(BackendScalar::random(rng))
    }

    pub fn mul_by_generator(&self) -> Point {
        &Point::GENERATOR * self
    }

    pub fn pow(&self, exp: usize) -> Self {
        let mut result = Self::ONE;
        for _ in 0..exp {
            result = &result * self;
        }
        result
    }

    pub fn invert(&self) -> CtOption<Self> {
        self.0.invert().map(Self)
    }

    pub fn from_digest(d: impl Digest<OutputSize = FieldBytesSize<k256::Secp256k1>>) -> Self {
        // There's currently no way to make the required digest output size
        // depend on the target scalar size, so we are hardcoding it to 256 bit
        // (that is, equal to the scalar size).
        Self(<BackendScalar as Reduce<U256>>::reduce_bytes(&d.finalize()))
    }

    /// Convert a 32-byte hash digest into a scalar as per SEC1:
    /// <https://www.secg.org/sec1-v2.pdf< Section 4.1.3 steps 5-6 page 45
    ///
    /// SEC1 specifies to subtract the secp256k1 modulus when the byte array
    /// is larger than the modulus.
    pub fn try_from_reduced_bytes(bytes: &[u8; 32]) -> Result<Self, String> {
        let arr = GenericArray::<u8, FieldBytesSize<Secp256k1>>::from(*bytes);
        Ok(Self(<BackendScalar as Reduce<U256>>::reduce_bytes(&arr)))
    }

    pub fn to_be_bytes(self) -> k256::FieldBytes {
        // TODO: add a test that it really is a big endian representation - docs don't guarantee it.
        self.0.to_bytes()
    }

    pub fn repr_len() -> usize {
        <FieldBytesSize<Secp256k1> as Unsigned>::to_usize()
    }

    pub(crate) fn to_backend(self) -> BackendScalar {
        self.0
    }

    // TODO: replace with try_from_be_array()
    pub(crate) fn try_from_be_bytes(bytes: &[u8]) -> Result<Self, String> {
        let arr =
            GenericArray::<u8, FieldBytesSize<Secp256k1>>::from_exact_iter(bytes.iter().cloned())
                .ok_or("Invalid length of a curve scalar")?;

        BackendScalar::from_repr_vartime(arr)
            .map(Self)
            .ok_or_else(|| "Invalid curve scalar representation".into())
    }

    pub(crate) fn split(&self, rng: &mut impl CryptoRngCore, num: usize) -> Vec<Scalar> {
        // CHECK: do all the parts have to be non-zero?
        if num == 1 {
            return vec![*self];
        }

        let mut parts = (0..(num - 1))
            .map(|_| Scalar::random(rng))
            .collect::<Vec<_>>();
        let partial_sum: Scalar = parts.iter().sum();
        parts.push(self - &partial_sum);
        parts
    }
}

impl From<&NonZeroScalar<k256::Secp256k1>> for Scalar {
    fn from(val: &NonZeroScalar<k256::Secp256k1>) -> Self {
        Self(*val.as_ref())
    }
}

impl Serialize for Scalar {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serde_bytes::serialize(AsRef::<[u8]>::as_ref(&self.to_be_bytes()), serializer)
    }
}

impl<'de> Deserialize<'de> for Scalar {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let bytes: &[u8] = serde_bytes::deserialize(deserializer)?;
        Self::try_from_be_bytes(bytes).map_err(D::Error::custom)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Point(BackendPoint);

impl Point {
    pub const GENERATOR: Self = Self(BackendPoint::GENERATOR);

    pub const IDENTITY: Self = Self(BackendPoint::IDENTITY);

    // TODO: technically it can be any hash function from Point to Scalar, right?
    // so we can just rename it to `to_scalar()` or something.
    pub fn x_coordinate(&self) -> Scalar {
        let bytes = self.0.to_affine().x();
        Scalar(<BackendScalar as Reduce<U256>>::reduce_bytes(&bytes))
    }

    pub fn to_verifying_key(self) -> Option<VerifyingKey> {
        VerifyingKey::from_affine(self.0.to_affine()).ok()
    }

    pub(crate) fn try_from_compressed_bytes(bytes: &[u8]) -> Result<Self, String> {
        let ep = EncodedPoint::<Secp256k1>::from_bytes(bytes).map_err(|err| format!("{err}"))?;

        // Unwrap CtOption into Option
        let cp_opt: Option<BackendPoint> = BackendPoint::from_encoded_point(&ep).into();
        cp_opt
            .map(Self)
            .ok_or_else(|| "Invalid curve point representation".into())
    }

    pub(crate) fn to_compressed_array(self) -> GenericArray<u8, CompressedPointSize> {
        *GenericArray::<u8, CompressedPointSize>::from_slice(
            self.0.to_affine().to_encoded_point(true).as_bytes(),
        )
    }

    pub(crate) fn to_backend(self) -> BackendPoint {
        self.0
    }
}

impl Serialize for Point {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serde_bytes::serialize(
            AsRef::<[u8]>::as_ref(&self.to_compressed_array()),
            serializer,
        )
    }
}

impl<'de> Deserialize<'de> for Point {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let bytes: &[u8] = serde_bytes::deserialize(deserializer)?;
        Self::try_from_compressed_bytes(bytes).map_err(D::Error::custom)
    }
}

impl Hashable for Point {
    fn chain<C: Chain>(&self, digest: C) -> C {
        let arr = self.to_compressed_array();
        let arr_ref: &[u8] = arr.as_ref();
        digest.chain(&arr_ref)
    }
}

impl Default for Point {
    fn default() -> Self {
        Point::IDENTITY
    }
}

impl From<usize> for Scalar {
    fn from(val: usize) -> Self {
        // TODO: add a check that usize <= u64?
        Self(BackendScalar::from(val as u64))
    }
}

impl core::ops::Neg for Scalar {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

impl<'a> core::ops::Neg for &'a Scalar {
    type Output = Scalar;
    fn neg(self) -> Self::Output {
        Scalar(-self.0)
    }
}

impl Add<Scalar> for Scalar {
    type Output = Scalar;

    fn add(self, other: Scalar) -> Scalar {
        Scalar(self.0.add(other.0))
    }
}

impl Add<&Scalar> for &Scalar {
    type Output = Scalar;

    fn add(self, other: &Scalar) -> Scalar {
        Scalar(self.0.add(&(other.0)))
    }
}

impl Add<Point> for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point(self.0.add(other.0))
    }
}

impl Add<&Point> for &Point {
    type Output = Point;

    fn add(self, other: &Point) -> Point {
        Point(self.0.add(&(other.0)))
    }
}

impl Sub<Scalar> for Scalar {
    type Output = Scalar;

    fn sub(self, other: Scalar) -> Scalar {
        Scalar(self.0.sub(&(other.0)))
    }
}

impl Sub<&Scalar> for &Scalar {
    type Output = Scalar;

    fn sub(self, other: &Scalar) -> Scalar {
        Scalar(self.0.sub(&(other.0)))
    }
}

impl Mul<&Scalar> for &Point {
    type Output = Point;

    fn mul(self, other: &Scalar) -> Point {
        Point(self.0.mul(&(other.0)))
    }
}

impl Mul<Scalar> for Scalar {
    type Output = Scalar;

    fn mul(self, other: Scalar) -> Scalar {
        Scalar(self.0.mul(&(other.0)))
    }
}

impl Mul<&Scalar> for &Scalar {
    type Output = Scalar;

    fn mul(self, other: &Scalar) -> Scalar {
        Scalar(self.0.mul(&(other.0)))
    }
}

impl core::iter::Sum for Scalar {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(core::ops::Add::add).unwrap_or(Self::ZERO)
    }
}

impl<'a> core::iter::Sum<&'a Self> for Scalar {
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.cloned().sum()
    }
}

impl core::iter::Product for Scalar {
    fn product<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(core::ops::Mul::mul).unwrap_or(Self::ONE)
    }
}

impl<'a> core::iter::Product<&'a Self> for Scalar {
    fn product<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.cloned().product()
    }
}

impl core::iter::Sum for Point {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        iter.reduce(core::ops::Add::add).unwrap_or(Self::IDENTITY)
    }
}

impl<'a> core::iter::Sum<&'a Self> for Point {
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.cloned().sum()
    }
}
