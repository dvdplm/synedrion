use core::fmt;

use bincode::{
    config::{
        BigEndian, Bounded, RejectTrailing, VarintEncoding, WithOtherEndian, WithOtherIntEncoding,
        WithOtherLimit, WithOtherTrailing,
    },
    DefaultOptions, Options,
};
use js_sys::Error;
use rand_core::OsRng;
use serde::{Serialize, Serializer};
use synedrion::k256::ecdsa;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen_derive::{try_from_js_array, try_from_js_option, TryFromJsValue};

use synedrion::TestParams;

extern crate alloc;

/// Max message length allowed to be (de)serialized
const MAX_MSG_LEN: u64 = 1000 * 1000; // 1 MB

#[wasm_bindgen]
extern "C" {
    /// A type alias for optional `SigningKey`
    #[wasm_bindgen(typescript_type = "SigningKey | undefined")]
    pub type OptionalSigningKey;

    /// A type alias for optional `SigningKey`
    #[wasm_bindgen(typescript_type = "VerifyingKey[]")]
    pub type VerifyingKeyList;

}

fn map_js_err<T: fmt::Display>(err: T) -> Error {
    Error::new(&format!("{err}"))
}

/// Secp256k1 signing key.
#[derive(TryFromJsValue)]
#[wasm_bindgen]
#[derive(Clone)]
pub struct SigningKey(ecdsa::SigningKey);

#[wasm_bindgen]
impl SigningKey {
    /// Creates the object from the serialized big-endian scalar
    #[wasm_bindgen(js_name = fromBeBytes)]
    pub fn from_be_bytes(bytes: &[u8]) -> Result<SigningKey, Error> {
        ecdsa::SigningKey::from_slice(bytes)
            .map(Self)
            .map_err(|err| Error::new(&format!("{}", err)))
    }
}

#[derive(TryFromJsValue)]
#[wasm_bindgen]
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VerifyingKey(ecdsa::VerifyingKey);

#[wasm_bindgen]
impl VerifyingKey {
    #[wasm_bindgen]
    pub fn random() -> Self {
        Self(*ecdsa::SigningKey::random(&mut OsRng).verifying_key())
    }
}

impl Serialize for VerifyingKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_bytes(self.0.to_encoded_point(true).as_bytes())
    }
}

/// Synedrion key share.
#[derive(TryFromJsValue)]
#[wasm_bindgen]
#[derive(Clone)]
pub struct KeyShare(synedrion::KeyShare<TestParams, VerifyingKey>);

#[wasm_bindgen]
impl KeyShare {
    /// Serializes the key share to bytes using standard Entropy format.
    #[wasm_bindgen(js_name = toBytes)]
    pub fn to_bytes(&self) -> Result<Vec<u8>, Error> {
        // TODO (#71): can we ensure consistency here?
        // Should `entropy-core` expose the serialization function?
        bincoder()
            .serialize(&self.0)
            .map_err(|err| Error::new(&format!("{}", err)))
    }

    /// Creates a set of key shares.
    #[wasm_bindgen(js_name = newCentralized)]
    pub fn new_centralized(
        parties: &VerifyingKeyList,
        signing_key: &OptionalSigningKey,
    ) -> Result<Vec<KeyShare>, Error> {
        let sk = try_from_js_option::<SigningKey>(signing_key).map_err(map_js_err)?;
        let backend_sk = sk.map(|sk| sk.0);
        let parties = try_from_js_array::<VerifyingKey>(parties).map_err(map_js_err)?;

        let shares = synedrion::KeyShare::<TestParams, VerifyingKey>::new_centralized(
            &mut OsRng,
            &parties,
            backend_sk.as_ref(),
        );
        Ok(shares.into_vec().into_iter().map(KeyShare).collect())
    }
}

/// Prepares a `bincode` serde backend with our preferred config
/// This is copied from `entropy-core/crypto/kvdb/src/kv_manager/helpers.rs`
/// In the hope that it gives us shares which are encoded just the same as
/// the are in the key value store there.
#[allow(clippy::type_complexity)]
fn bincoder() -> WithOtherTrailing<
    WithOtherIntEncoding<
        WithOtherEndian<WithOtherLimit<DefaultOptions, Bounded>, BigEndian>,
        VarintEncoding,
    >,
    RejectTrailing,
> {
    DefaultOptions::new()
        .with_limit(MAX_MSG_LEN)
        .with_big_endian() // big endian representation for integers
        .with_varint_encoding() // saves a lot of space in smaller messages
        .reject_trailing_bytes() // do not ignore extra bytes at the end of the buffer
}
