use alloc::boxed::Box;
use alloc::string::ToString;

use serde::{Deserialize, Serialize};
use signature::hazmat::{PrehashSigner, PrehashVerifier};

use super::error::{MyFault, TheirFault};
use crate::tools::hashing::{Chain, Hash, HashOutput};

pub(crate) fn serialize_message(message: &impl Serialize) -> Result<Box<[u8]>, MyFault> {
    rmp_serde::encode::to_vec(message)
        .map(|serialized| serialized.into_boxed_slice())
        .map_err(MyFault::SerializationError)
}

pub(crate) fn deserialize_message<M: for<'de> Deserialize<'de>>(
    message_bytes: &[u8],
) -> Result<M, rmp_serde::decode::Error> {
    rmp_serde::decode::from_slice(message_bytes)
}

fn message_hash(stage: u8, payload: &[u8]) -> HashOutput {
    Hash::new_with_dst(b"SignedMessage")
        .chain(&stage)
        .chain(&payload)
        .finalize()
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SignedMessage<Sig> {
    // TODO
    //session_id: SessionId,
    stage: u8,
    payload: Box<[u8]>, // TODO: add serialization attribute to avoid serializing as Vec<u8>
    signature: Sig,
}

impl<Sig> SignedMessage<Sig> {
    pub(crate) fn verify(
        self,
        verifier: &impl PrehashVerifier<Sig>,
    ) -> Result<VerifiedMessage<Sig>, TheirFault> {
        verifier
            .verify_prehash(
                message_hash(self.stage, &self.payload).as_ref(),
                &self.signature,
            )
            .map_err(|err| TheirFault::VerificationFail(err.to_string()))?;
        Ok(VerifiedMessage(self))
    }
}

pub(crate) struct VerifiedMessage<Sig>(SignedMessage<Sig>);

impl<Sig> VerifiedMessage<Sig> {
    pub(crate) fn new(
        signer: &impl PrehashSigner<Sig>,
        stage: u8,
        message_bytes: &[u8],
    ) -> Result<Self, MyFault> {
        // In order for the messages be impossible to reuse by a malicious third party,
        // we need to sign, besides the message itself, the session and the stage in this session
        // it belongs to.
        // We also need the exact way we sign this to be a part of the public ABI,
        // so that these signatures could be verified by a third party.

        let signature = signer
            .sign_prehash(message_hash(stage, message_bytes).as_ref())
            .map_err(|err| MyFault::SigningError(err.to_string()))?;
        Ok(Self(SignedMessage {
            stage,
            payload: message_bytes.into(),
            signature,
        }))
    }

    pub fn into_unverified(self) -> SignedMessage<Sig> {
        self.0
    }

    pub fn payload(&self) -> &[u8] {
        &self.0.payload
    }

    pub fn stage(&self) -> u8 {
        self.0.stage
    }
}
