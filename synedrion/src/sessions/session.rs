use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::format;
use alloc::vec::Vec;
use core::fmt::Debug;

use rand_core::CryptoRngCore;
use serde::{Deserialize, Serialize};
use signature::{
    hazmat::{PrehashVerifier, RandomizedPrehashSigner},
    Keypair,
};

use super::combined_message::{CheckedCombinedMessage, CombinedMessage, VerifiedCombinedMessage};
use super::echo::{EchoAccum, EchoRound};
use super::error::{Error, LocalError, ProvableError, RemoteError, RemoteErrorEnum};
use super::signed_message::{MessageType, SessionId, SignedMessage, VerifiedMessage};
use super::type_erased::{
    self, AccumAddError, DynArtifact, DynFinalizable, DynPayload, DynRoundAccum, ReceiveError,
};
use crate::rounds::{self, FirstRound, PartyIdx, ProtocolResult, Round};
use crate::tools::collections::HoleRange;

/// A protocol result with internal party IDs mapped to external ones.
pub trait MappedResult<Verifier>: ProtocolResult {
    /// [`ProtocolResult::Success`] with external party IDs.
    type MappedSuccess;

    /// Map internal party IDs for the protocol success result.
    fn map_success(inner: Self::Success, verifiers: &[Verifier]) -> Self::MappedSuccess;
}

struct Context<Signer, Verifier> {
    signer: Signer,
    verifiers: Vec<Verifier>,
    session_id: SessionId,
    party_idx: PartyIdx,
    verifier_to_idx: BTreeMap<Verifier, PartyIdx>,
}

enum SessionType<Res, Sig> {
    Normal {
        this_round: Box<dyn DynFinalizable<Res>>,
        broadcast: Option<SignedMessage<Sig>>,
    },
    Echo {
        next_round: Box<dyn DynFinalizable<Res>>,
        echo_round: EchoRound<Sig>,
    },
}

/// The session state where it is ready to send messages.
pub struct Session<Res, Sig, Signer, Verifier> {
    tp: SessionType<Res, Sig>,
    context: Context<Signer, Verifier>,
}

enum MessageFor {
    ThisRound,
    NextRound,
}

fn route_message_normal<Res: ProtocolResult, Sig>(
    round: &dyn DynFinalizable<Res>,
    message: &CheckedCombinedMessage<Sig>,
) -> Result<MessageFor, RemoteErrorEnum> {
    let this_round = round.round_num();
    let next_round = round.next_round_num();
    let requires_echo = round.requires_echo();

    let message_round = message.round();
    let message_is_echo = message.is_echo();

    if message_round == this_round && !message_is_echo {
        return Ok(MessageFor::ThisRound);
    }

    let for_next_round =
    // This is a normal round, and the next round exists, and the message is for it
    (!requires_echo && next_round.is_some() && message_round == next_round.unwrap() && !message_is_echo) ||
    // This is an echo round, and the message is from the echo round
    (requires_echo && message_round == this_round && message_is_echo);

    if for_next_round {
        return Ok(MessageFor::NextRound);
    }

    Err(RemoteErrorEnum::OutOfOrderMessage)
}

fn route_message_echo<Res: ProtocolResult, Sig>(
    next_round: &dyn DynFinalizable<Res>,
    message: &CheckedCombinedMessage<Sig>,
) -> Result<MessageFor, RemoteErrorEnum> {
    let next_round = next_round.round_num();
    let message_round = message.round();
    let message_is_echo = message.is_echo();

    if message_round == next_round - 1 && message_is_echo {
        return Ok(MessageFor::ThisRound);
    }

    if message_round == next_round && !message_is_echo {
        return Ok(MessageFor::NextRound);
    }

    Err(RemoteErrorEnum::OutOfOrderMessage)
}

fn wrap_receive_result<Res: ProtocolResult, Verifier: Clone, T>(
    from: &Verifier,
    result: Result<T, ReceiveError<Res>>,
) -> Result<T, Error<Res, Verifier>> {
    // TODO (#43): we need to attach all the necessary messages here,
    // to make sure that every provable error can be independently verified
    // given the party's verifying key.
    result.map_err(|err| match err {
        ReceiveError::InvalidContents(msg) => Error::Remote(RemoteError {
            party: from.clone(),
            error: RemoteErrorEnum::InvalidContents(msg),
        }),
        ReceiveError::CannotDeserialize(msg) => Error::Provable {
            party: from.clone(),
            error: ProvableError::CannotDeserialize(msg),
        },
        ReceiveError::Protocol(err) => Error::Provable {
            party: from.clone(),
            error: ProvableError::Protocol(err),
        },
    })
}

/// Possible outcomes of successfully finalizing a round.
pub enum FinalizeOutcome<Res: MappedResult<Verifier>, Sig, Signer, Verifier> {
    /// The protocol result is available.
    Success(Res::MappedSuccess),
    /// Starting the next round.
    AnotherRound {
        /// The new session object.
        session: Session<Res, Sig, Signer, Verifier>,
        /// The messages for the new round received during the previous round.
        cached_messages: Vec<PreprocessedMessage<Sig>>,
    },
}

impl<Res, Sig, Signer, Verifier> Session<Res, Sig, Signer, Verifier>
where
    Res: MappedResult<Verifier>,
    Signer: RandomizedPrehashSigner<Sig> + Keypair<VerifyingKey = Verifier>,
    Verifier: Debug + Clone + PrehashVerifier<Sig> + Ord,
    Sig: Clone + Serialize + for<'de> Deserialize<'de> + PartialEq + Eq,
{
    pub(crate) fn new<R: FirstRound + DynFinalizable<Res> + Round<Result = Res> + 'static>(
        rng: &mut impl CryptoRngCore,
        shared_randomness: &[u8],
        signer: Signer,
        verifiers: &[Verifier],
        inputs: R::Inputs,
    ) -> Result<Self, LocalError> {
        let verifier_to_idx = verifiers
            .iter()
            .enumerate()
            .map(|(idx, verifier)| (verifier.clone(), PartyIdx::from_usize(idx)))
            .collect::<BTreeMap<_, _>>();
        let party_idx = *verifier_to_idx
            .get(&signer.verifying_key())
            .ok_or(LocalError(
                "The given signer's verifying key is not among the verifiers".into(),
            ))?;

        // TODO (#3): Is this enough? Do we need to hash in e.g. the verifier public keys?
        //            Need to specify the requirements for the shared randomness in the docstring.
        let session_id = SessionId::from_seed(shared_randomness);
        let typed_round = R::new(rng, shared_randomness, verifiers.len(), party_idx, inputs)
            .map_err(|err| LocalError(format!("Failed to initialize the protocol: {err:?}")))?;
        let round: Box<dyn DynFinalizable<Res>> = Box::new(typed_round);
        let context = Context {
            signer,
            verifiers: verifiers.into(),
            session_id,
            party_idx,
            verifier_to_idx,
        };
        Self::new_internal(rng, context, round)
    }

    fn new_internal(
        rng: &mut impl CryptoRngCore,
        context: Context<Signer, Verifier>,
        round: Box<dyn DynFinalizable<Res>>,
    ) -> Result<Self, LocalError> {
        let broadcast = round.make_broadcast_message(rng)?;

        let signed_broadcast = if let Some(payload) = broadcast {
            Some(
                VerifiedMessage::new(
                    rng,
                    &context.signer,
                    &context.session_id,
                    round.round_num(),
                    MessageType::Broadcast,
                    &payload,
                )?
                .into_unverified(),
            )
        } else {
            None
        };

        Ok(Self {
            tp: SessionType::Normal {
                this_round: round,
                broadcast: signed_broadcast,
            },
            context,
        })
    }

    /// This session's verifier object.
    pub fn verifier(&self) -> Verifier {
        self.context.signer.verifying_key()
    }

    /// Returns a pair of the current round index and whether it is an echo round.
    pub fn current_round(&self) -> (u8, bool) {
        match &self.tp {
            SessionType::Normal { this_round, .. } => (this_round.round_num(), false),
            SessionType::Echo { next_round, .. } => (next_round.round_num() - 1, true),
        }
    }

    /// Create an accumulator to store message creation and processing results of this round.
    pub fn make_accumulator(&self) -> RoundAccumulator<Sig> {
        RoundAccumulator::new(
            self.context.verifiers.len(),
            self.context.party_idx,
            self.is_echo_round(),
        )
    }

    /// Returns `true` if the round can be finalized.
    pub fn can_finalize(&self, accum: &RoundAccumulator<Sig>) -> Result<bool, LocalError> {
        match &self.tp {
            SessionType::Normal { this_round, .. } => Ok(this_round.can_finalize(&accum.processed)),
            SessionType::Echo { .. } => Ok(accum
                .echo_accum
                .as_ref()
                .ok_or(LocalError(
                    "This is an echo round, but the accumulator is in an invalid state".into(),
                ))?
                .can_finalize()),
        }
    }

    /// Returns a list of parties whose messages for this round have not been received yet.
    pub fn missing_messages(
        &self,
        accum: &RoundAccumulator<Sig>,
    ) -> Result<Vec<Verifier>, LocalError> {
        let missing = match &self.tp {
            SessionType::Normal { this_round, .. } => Ok(this_round
                .missing_payloads(&accum.processed)
                .into_iter()
                .collect()),
            SessionType::Echo { .. } => {
                let echo_accum = accum.echo_accum.as_ref().ok_or(LocalError(
                    "This is an echo round, but the accumulator is in an invalid state".into(),
                ))?;
                Ok(echo_accum.missing_messages())
            }
        };

        missing.map(|set| {
            set.into_iter()
                .map(|idx| self.context.verifiers[idx.as_usize()].clone())
                .collect()
        })
    }

    fn is_echo_round(&self) -> bool {
        match &self.tp {
            SessionType::Normal { .. } => false,
            SessionType::Echo { .. } => true,
        }
    }

    /// Returns the party indices to which the messages of this round should be sent.
    pub fn message_destinations(&self) -> Vec<Verifier> {
        match &self.tp {
            SessionType::Normal { this_round, .. } => this_round
                .message_destinations()
                .iter()
                .map(|idx| self.context.verifiers[idx.as_usize()].clone())
                .collect(),
            SessionType::Echo { .. } => {
                // TODO (#82): technically we should remember the range
                // to which the initial broadcasts were sent to and use that.
                let range = HoleRange::new(
                    self.context.verifiers.len(),
                    self.context.party_idx.as_usize(),
                );
                range
                    .map(|idx| self.context.verifiers[idx].clone())
                    .collect()
            }
        }
    }

    /// Returns the message for the given destination
    /// (must be one of those returned by [`Self::message_destinations`].
    pub fn make_message(
        &self,
        rng: &mut impl CryptoRngCore,
        destination: &Verifier,
    ) -> Result<(CombinedMessage<Sig>, Artifact<Verifier>), LocalError> {
        let destination_idx = *self
            .context
            .verifier_to_idx
            .get(destination)
            .ok_or(LocalError(format!("Verifier not found: {destination:?}")))?;

        match &self.tp {
            SessionType::Normal {
                this_round,
                broadcast,
            } => {
                let round_num = this_round.round_num();
                let (payload, artifact) = this_round.make_direct_message(rng, destination_idx)?;

                let direct_message = if let Some(payload) = payload {
                    Some(
                        VerifiedMessage::new(
                            rng,
                            &self.context.signer,
                            &self.context.session_id,
                            round_num,
                            MessageType::Direct,
                            &payload,
                        )?
                        .into_unverified(),
                    )
                } else {
                    None
                };

                let message = match (broadcast, direct_message) {
                    (Some(broadcast), Some(direct)) => CombinedMessage::Both {
                        broadcast: broadcast.clone(),
                        direct,
                    },
                    (None, Some(direct)) => CombinedMessage::One(direct),
                    (Some(broadcast), None) => CombinedMessage::One(broadcast.clone()),
                    (None, None) => return Err(LocalError("The round must send messages".into())),
                };

                Ok((
                    message,
                    Artifact {
                        destination: destination.clone(),
                        destination_idx,
                        artifact,
                    },
                ))
            }
            SessionType::Echo {
                next_round,
                echo_round,
            } => {
                let round_num = next_round.round_num() - 1;
                let payload = echo_round.make_broadcast();
                let artifact = DynArtifact::null();
                let message = VerifiedMessage::new(
                    rng,
                    &self.context.signer,
                    &self.context.session_id,
                    round_num,
                    MessageType::Echo,
                    &payload,
                )?
                .into_unverified();
                Ok((
                    CombinedMessage::One(message),
                    Artifact {
                        destination: destination.clone(),
                        destination_idx,
                        artifact,
                    },
                ))
            }
        }
    }

    fn route_message(
        &self,
        from: &Verifier,
        message: &CheckedCombinedMessage<Sig>,
    ) -> Result<MessageFor, Error<Res, Verifier>> {
        let message_for = match &self.tp {
            SessionType::Normal { this_round, .. } => {
                route_message_normal(this_round.as_ref(), message)
            }
            SessionType::Echo { next_round, .. } => {
                route_message_echo(next_round.as_ref(), message)
            }
        };

        message_for.map_err(|err| {
            Error::Remote(RemoteError {
                party: from.clone(),
                error: err,
            })
        })
    }

    /// Perform quick checks on a received message.
    pub fn preprocess_message(
        &self,
        accum: &mut RoundAccumulator<Sig>,
        from: &Verifier,
        message: CombinedMessage<Sig>,
    ) -> Result<Option<PreprocessedMessage<Sig>>, Error<Res, Verifier>> {
        let checked = message.check().map_err(|msg| {
            Error::Remote(RemoteError {
                party: from.clone(),
                error: RemoteErrorEnum::InvalidContents(msg),
            })
        })?;

        // This is an unprovable fault (may be a replay attack)
        if checked.session_id() != &self.context.session_id {
            return Err(Error::Remote(RemoteError {
                party: from.clone(),
                error: RemoteErrorEnum::UnexpectedSessionId,
            }));
        }

        let message_for = self.route_message(from, &checked)?;

        let verified_message = checked.verify(from).map_err(|err| {
            Error::Remote(RemoteError {
                party: from.clone(),
                error: RemoteErrorEnum::InvalidSignature(err),
            })
        })?;

        let from_idx = *self
            .context
            .verifier_to_idx
            .get(from)
            .ok_or(Error::Local(LocalError(format!(
                "Verifier not found: {from:?}"
            ))))?;

        if from_idx == self.context.party_idx {
            return Err(Error::Local(LocalError(
                "Cannot take a message from myself".into(),
            )));
        }

        let preprocessed = PreprocessedMessage {
            from_idx,
            message: verified_message,
        };

        Ok(match message_for {
            MessageFor::ThisRound => {
                if accum.is_already_processed(&preprocessed) {
                    return Err(Error::Remote(RemoteError {
                        party: from.clone(),
                        error: RemoteErrorEnum::DuplicateMessage,
                    }));
                }
                Some(preprocessed)
            }
            MessageFor::NextRound => {
                if accum.is_already_cached(&preprocessed) {
                    return Err(Error::Remote(RemoteError {
                        party: from.clone(),
                        error: RemoteErrorEnum::DuplicateMessage,
                    }));
                }
                accum.add_cached_message(preprocessed);
                None
            }
        })
    }

    /// Process a received message from another party.
    pub fn process_message(
        &self,
        preprocessed: PreprocessedMessage<Sig>,
    ) -> Result<ProcessedMessage<Sig, Verifier>, Error<Res, Verifier>> {
        let from_idx = preprocessed.from_idx;
        let from = self.context.verifiers[preprocessed.from_idx.as_usize()].clone();
        let message = preprocessed.message;
        match &self.tp {
            SessionType::Normal { this_round, .. } => {
                let result = this_round.verify_message(
                    from_idx,
                    message.broadcast_payload(),
                    message.direct_payload(),
                );
                let payload = wrap_receive_result(&from, result)?;
                Ok(ProcessedMessage {
                    from: from.clone(),
                    from_idx,
                    message: ProcessedMessageEnum::Payload { payload, message },
                })
            }
            SessionType::Echo { echo_round, .. } => {
                echo_round
                    .verify_broadcast(from_idx, message.echo_payload().unwrap())
                    .map_err(|err| Error::Provable {
                        party: from.clone(),
                        error: ProvableError::Echo(err),
                    })?;
                Ok(ProcessedMessage {
                    from: from.clone(),
                    from_idx,
                    message: ProcessedMessageEnum::Bc,
                })
            }
        }
    }

    /// Try to finalize the round.
    pub fn finalize_round(
        self,
        rng: &mut impl CryptoRngCore,
        accum: RoundAccumulator<Sig>,
    ) -> Result<FinalizeOutcome<Res, Sig, Signer, Verifier>, Error<Res, Verifier>> {
        match self.tp {
            SessionType::Normal { this_round, .. } => {
                Self::finalize_regular_round(self.context, this_round, rng, accum)
            }
            SessionType::Echo { next_round, .. } => {
                Self::finalize_bc_round(self.context, next_round, rng, accum)
            }
        }
    }

    fn finalize_regular_round(
        context: Context<Signer, Verifier>,
        round: Box<dyn DynFinalizable<Res>>,
        rng: &mut impl CryptoRngCore,
        accum: RoundAccumulator<Sig>,
    ) -> Result<FinalizeOutcome<Res, Sig, Signer, Verifier>, Error<Res, Verifier>> {
        let requires_echo = round.requires_echo();

        let outcome = round
            .finalize(rng, accum.processed)
            .map_err(|err| match err {
                type_erased::FinalizeError::Protocol(err) => match err {
                    rounds::FinalizeError::Init(err) => Error::Local(LocalError(format!(
                        "Failed to initialize the protocol: {err:?}"
                    ))),
                    rounds::FinalizeError::Proof(proof) => Error::Proof { proof },
                },
                type_erased::FinalizeError::Accumulator(err) => {
                    Error::Local(LocalError(format!("Failed to finalize: {err:?}")))
                }
            })?;

        match outcome {
            type_erased::FinalizeOutcome::Success(res) => Ok(FinalizeOutcome::Success(
                Res::map_success(res, &context.verifiers),
            )),
            type_erased::FinalizeOutcome::AnotherRound(next_round) => {
                if requires_echo {
                    let broadcasts = accum
                        .received_messages
                        .iter()
                        .map(|(idx, combined)| {
                            (*idx, combined.broadcast_message().unwrap().clone())
                        })
                        .collect::<Vec<_>>();

                    let echo_round = EchoRound::new(broadcasts);
                    let session = Session {
                        tp: SessionType::Echo {
                            next_round,
                            echo_round,
                        },
                        context,
                    };
                    Ok(FinalizeOutcome::AnotherRound {
                        session,
                        cached_messages: accum.cached_messages.into_values().collect(),
                    })
                } else {
                    let session =
                        Session::new_internal(rng, context, next_round).map_err(Error::Local)?;
                    Ok(FinalizeOutcome::AnotherRound {
                        session,
                        cached_messages: accum.cached_messages.into_values().collect(),
                    })
                }
            }
        }
    }

    fn finalize_bc_round(
        context: Context<Signer, Verifier>,
        round: Box<dyn DynFinalizable<Res>>,
        rng: &mut impl CryptoRngCore,
        accum: RoundAccumulator<Sig>,
    ) -> Result<FinalizeOutcome<Res, Sig, Signer, Verifier>, Error<Res, Verifier>> {
        let echo_accum = accum.echo_accum.ok_or(Error::Local(LocalError(
            "The accumulator is in the invalid state for the echo round".into(),
        )))?;

        echo_accum
            .finalize()
            .ok_or(Error::Local(LocalError("Cannot finalize".into())))?;

        let session = Session::new_internal(rng, context, round).map_err(Error::Local)?;

        Ok(FinalizeOutcome::AnotherRound {
            session,
            cached_messages: accum.cached_messages.into_values().collect(),
        })
    }
}

/// A mutable accumulator created for each round to assemble processed messages from other parties.
pub struct RoundAccumulator<Sig> {
    received_messages: Vec<(PartyIdx, VerifiedCombinedMessage<Sig>)>,
    processed: DynRoundAccum,
    cached_messages: BTreeMap<PartyIdx, PreprocessedMessage<Sig>>,
    echo_accum: Option<EchoAccum>,
}

impl<Sig> RoundAccumulator<Sig> {
    fn new(num_parties: usize, party_idx: PartyIdx, is_echo_round: bool) -> Self {
        // TODO (#68): can return an error if party_idx is out of bounds
        Self {
            received_messages: Vec::new(),
            processed: DynRoundAccum::new(),
            cached_messages: BTreeMap::new(),
            echo_accum: if is_echo_round {
                Some(EchoAccum::new(num_parties, party_idx))
            } else {
                None
            },
        }
    }

    /// Save an artifact produced by [`Session::make_message`].
    pub fn add_artifact<Verifier: Debug>(
        &mut self,
        artifact: Artifact<Verifier>,
    ) -> Result<(), LocalError> {
        self.processed
            .add_artifact(artifact.destination_idx, artifact.artifact)
            .map_err(|err| match err {
                AccumAddError::SlotTaken => LocalError(format!(
                    "Artifact for the destination {:?} was already added",
                    artifact.destination
                )),
            })
    }

    /// Save a processed message produced by [`Session::process_message`].
    pub fn add_processed_message<Verifier>(
        &mut self,
        pm: ProcessedMessage<Sig, Verifier>,
    ) -> Result<Result<(), RemoteError<Verifier>>, LocalError> {
        match pm.message {
            ProcessedMessageEnum::Payload { payload, message } => {
                if let Err(AccumAddError::SlotTaken) =
                    self.processed.add_payload(pm.from_idx, payload)
                {
                    return Ok(Err(RemoteError {
                        party: pm.from,
                        error: RemoteErrorEnum::DuplicateMessage,
                    }));
                }
                self.received_messages.push((pm.from_idx, message));
            }
            ProcessedMessageEnum::Bc => match &mut self.echo_accum {
                Some(accum) => {
                    if accum.add_echo_received(pm.from_idx).is_none() {
                        return Ok(Err(RemoteError {
                            party: pm.from,
                            error: RemoteErrorEnum::DuplicateMessage,
                        }));
                    }
                }
                None => return Err(LocalError("This is not an echo round".into())),
            },
        }
        Ok(Ok(()))
    }

    fn is_already_processed(&self, preprocessed: &PreprocessedMessage<Sig>) -> bool {
        if preprocessed.message.is_echo() {
            self.echo_accum
                .as_ref()
                .unwrap()
                .contains(preprocessed.from_idx)
        } else {
            self.processed.contains(preprocessed.from_idx)
        }
    }

    fn is_already_cached(&self, preprocessed: &PreprocessedMessage<Sig>) -> bool {
        self.cached_messages.contains_key(&preprocessed.from_idx)
    }

    fn add_cached_message(&mut self, preprocessed: PreprocessedMessage<Sig>) {
        self.cached_messages
            .insert(preprocessed.from_idx, preprocessed);
    }
}

/// Data produced when creating a direct message to another party
/// that has to be preserved for further processing.
pub struct Artifact<Verifier> {
    destination: Verifier,
    destination_idx: PartyIdx,
    artifact: DynArtifact,
}

/// A message that passed initial validity checks.
pub struct PreprocessedMessage<Sig> {
    from_idx: PartyIdx,
    message: VerifiedCombinedMessage<Sig>,
}

/// A processed message from another party.
pub struct ProcessedMessage<Sig, Verifier> {
    from: Verifier,
    from_idx: PartyIdx,
    message: ProcessedMessageEnum<Sig>,
}

enum ProcessedMessageEnum<Sig> {
    Payload {
        payload: DynPayload,
        message: VerifiedCombinedMessage<Sig>,
    },
    Bc,
}

#[cfg(test)]
mod tests {
    use impls::impls;
    use k256::ecdsa::{Signature, SigningKey, VerifyingKey};

    use super::{Artifact, CombinedMessage, PreprocessedMessage, ProcessedMessage, Session};
    use crate::ProtocolResult;

    #[test]
    fn test_concurrency_bounds() {
        // In order to support parallel message creation and processing we need that
        // certain generic types could be Send and/or Sync.
        //
        // Since they are generic, this depends on the exact type parameters supplied by the user,
        // so if the user does not want parallelism, they may not use Send/Sync generic parameters.
        // But we want to make sure that if the generic parameters are Send/Sync,
        // our types are too.

        #[derive(Debug)]
        struct DummyResult;

        impl ProtocolResult for DummyResult {
            type Success = ();
            type ProvableError = ();
            type CorrectnessProof = ();
        }

        assert!(impls!(Session<DummyResult, Signature, SigningKey, VerifyingKey>: Sync));
        assert!(impls!(CombinedMessage<Signature>: Send));
        assert!(impls!(Artifact<VerifyingKey>: Send));
        assert!(impls!(PreprocessedMessage<Signature>: Send));
        assert!(impls!(ProcessedMessage<Signature, VerifyingKey>: Send));
    }
}
