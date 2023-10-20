use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::format;
use alloc::vec::Vec;
use core::fmt::Debug;
use core::marker::PhantomData;

use rand_core::CryptoRngCore;
use serde::{Deserialize, Serialize};
use signature::{
    hazmat::{PrehashVerifier, RandomizedPrehashSigner},
    Keypair,
};

use super::broadcast::{BcConsensusAccum, BroadcastConsensus};
use super::error::{Error, LocalError, ProvableError, RemoteError};
use super::signed_message::{MessageType, SessionId, SignedMessage, VerifiedMessage};
use super::type_erased::{
    self, DynBcPayload, DynDmArtefact, DynDmPayload, DynFinalizable, DynRoundAccum, ReceiveError,
};
use crate::cggmp21::{self, FirstRound, PartyIdx, ProtocolResult, Round};
use crate::tools::collections::HoleRange;

struct Context<Signer, Verifier> {
    signer: Signer,
    verifiers: Vec<Verifier>,
    session_id: SessionId,
    party_idx: PartyIdx,
    verifier_to_idx: BTreeMap<Verifier, PartyIdx>,
}

enum SessionType<Res, Sig> {
    Normal(Box<dyn DynFinalizable<Res>>),
    Bc {
        next_round: Box<dyn DynFinalizable<Res>>,
        bc: BroadcastConsensus<Sig>,
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
    OutOfOrder,
}

fn route_message_normal<Res: ProtocolResult, Sig>(
    round: &dyn DynFinalizable<Res>,
    message: &SignedMessage<Sig>,
) -> MessageFor {
    let this_round = round.round_num();
    let next_round = round.next_round_num();
    let requires_bc = round.requires_broadcast_consensus();

    let message_round = message.round();
    let message_bc = message.message_type() == MessageType::BroadcastConsensus;

    if message_round == this_round && !message_bc {
        return MessageFor::ThisRound;
    }

    let for_next_round =
    // This is a non-broadcast round, and the next round exists, and the message is for it
    (!requires_bc && next_round.is_some() && message_round == next_round.unwrap() && !message_bc) ||
    // This is a broadcast round, and the message is from the broadcast consensus round
    (requires_bc && message_round == this_round && message_bc);

    if for_next_round {
        return MessageFor::NextRound;
    }

    MessageFor::OutOfOrder
}

fn route_message_bc<Res: ProtocolResult, Sig>(
    next_round: &dyn DynFinalizable<Res>,
    message: &SignedMessage<Sig>,
) -> MessageFor {
    let next_round = next_round.round_num();
    let message_round = message.round();
    let message_bc = message.message_type() == MessageType::BroadcastConsensus;

    if message_round == next_round - 1 && message_bc {
        return MessageFor::ThisRound;
    }

    if message_round == next_round && !message_bc {
        return MessageFor::NextRound;
    }

    MessageFor::OutOfOrder
}

fn wrap_receive_result<Res: ProtocolResult, Verifier: Clone, T>(
    from: &Verifier,
    result: Result<T, ReceiveError<Res>>,
) -> Result<T, Error<Res, Verifier>> {
    // TODO: we need to attach all the necessary messages here,
    // to make sure that every provable error can be independently verified
    // given the party's verifying key.
    result.map_err(|err| match err {
        ReceiveError::CannotDeserialize(msg) => Error::Provable {
            party: from.clone(),
            error: ProvableError::CannotDeserialize(msg),
        },
        ReceiveError::Protocol(err) => match err {
            crate::cggmp21::ReceiveError::Provable(err) => Error::Provable {
                party: from.clone(),
                error: ProvableError::Protocol(err),
            },
            crate::cggmp21::ReceiveError::InvalidType => {
                Error::Local(LocalError::InvalidState("Invalid state".into()))
            }
        },
    })
}

/// Possible outcomes of successfully finalizing a round.
pub enum FinalizeOutcome<Res: ProtocolResult, Sig, Signer, Verifier> {
    /// The protocol result is available.
    Success(Res::Success),
    /// Starting the next round.
    AnotherRound {
        /// The new session object.
        session: Session<Res, Sig, Signer, Verifier>,
        /// The messages for the new round received during the previous round.
        cached_messages: Vec<(Verifier, SignedMessage<Sig>)>,
    },
}

impl<Res, Sig, Signer, Verifier> Session<Res, Sig, Signer, Verifier>
where
    Res: ProtocolResult,
    Signer: RandomizedPrehashSigner<Sig> + Keypair<VerifyingKey = Verifier>,
    Verifier: Debug + Clone + PrehashVerifier<Sig> + Ord,
    Sig: Clone + Serialize + for<'de> Deserialize<'de> + PartialEq + Eq,
{
    pub(crate) fn new<R: FirstRound + DynFinalizable<Res> + Round<Result = Res> + 'static>(
        rng: &mut impl CryptoRngCore,
        shared_randomness: &[u8],
        signer: Signer,
        verifiers: &[Verifier],
        context: R::Context,
    ) -> Result<Self, Error<Res, Verifier>> {
        let verifier_to_idx = verifiers
            .iter()
            .enumerate()
            .map(|(idx, verifier)| (verifier.clone(), PartyIdx::from_usize(idx)))
            .collect::<BTreeMap<_, _>>();
        let party_idx = *verifier_to_idx
            .get(&signer.verifying_key())
            .ok_or(Error::Local(LocalError::Init(
                "The given signer's verifying key is not among the verifiers".into(),
            )))?;

        // CHECK: is this enough? Do we need to hash in e.g. the verifier public keys?
        // TODO: Need to specify the requirements for the shared randomness in the docstring.
        let session_id = SessionId::from_seed(shared_randomness);
        let typed_round = R::new(rng, shared_randomness, verifiers.len(), party_idx, context)
            .map_err(|err| Error::Local(LocalError::Init(format!("{:?}", err))))?;
        let round: Box<dyn DynFinalizable<Res>> = Box::new(typed_round);
        let context = Context {
            signer,
            verifiers: verifiers.into(),
            session_id,
            party_idx,
            verifier_to_idx,
        };
        Ok(Self {
            tp: SessionType::Normal(round),
            context,
        })
    }

    /// This session's verifier object.
    pub fn verifier(&self) -> Verifier {
        self.context.signer.verifying_key()
    }

    /// Returns a pair of the current round index and whether it is a broadcast consensus stage.
    pub fn current_round(&self) -> (u8, bool) {
        match &self.tp {
            SessionType::Normal(round) => (round.round_num(), false),
            SessionType::Bc { next_round, .. } => (next_round.round_num() - 1, true),
        }
    }

    /// Create an accumulator to store message creation and processing results of this round.
    pub fn make_accumulator(&self) -> RoundAccumulator<Res, Sig> {
        RoundAccumulator::new(
            self.context.verifiers.len(),
            self.context.party_idx,
            self.broadcast_destinations().is_some(),
            self.direct_message_destinations().is_some(),
        )
    }

    /// Returns `true` if the round can be finalized.
    pub fn can_finalize(&self, accum: &RoundAccumulator<Res, Sig>) -> bool {
        match &self.tp {
            SessionType::Normal(_) => accum.processed.can_finalize(),
            SessionType::Bc { .. } => accum.bc_accum.can_finalize(),
        }
    }

    /// Returns the party indices to which the broadcast of this round should be sent;
    /// if `None`, there is no broadcast in this round.
    pub fn broadcast_destinations(&self) -> Option<Vec<Verifier>> {
        match &self.tp {
            SessionType::Normal(round) => round.broadcast_destinations().map(|range| {
                range
                    .map(|idx| self.context.verifiers[idx].clone())
                    .collect()
            }),
            SessionType::Bc { .. } => {
                // TODO: technically we should remember the range to which the initial broadcasts
                // were sent to and use that.
                let range = HoleRange::new(
                    self.context.verifiers.len(),
                    self.context.party_idx.as_usize(),
                );
                let verifiers = range
                    .map(|idx| self.context.verifiers[idx].clone())
                    .collect();
                Some(verifiers)
            }
        }
    }

    /// Returns the current round's broadcast.
    pub fn make_broadcast(
        &self,
        rng: &mut impl CryptoRngCore,
    ) -> Result<SignedMessage<Sig>, LocalError> {
        let (round_num, payload, is_bc_consensus) = match &self.tp {
            SessionType::Normal(round) => {
                let round_num = round.round_num();
                let payload = round.make_broadcast(rng)?;
                (round_num, payload, false)
            }
            SessionType::Bc { next_round, bc } => {
                let round_num = next_round.round_num() - 1;
                let payload = bc.make_broadcast();
                (round_num, payload, true)
            }
        };

        Ok(VerifiedMessage::new(
            rng,
            &self.context.signer,
            &self.context.session_id,
            round_num,
            if is_bc_consensus {
                MessageType::BroadcastConsensus
            } else {
                MessageType::Broadcast
            },
            &payload,
        )?
        .into_unverified())
    }

    /// Returns the party indices to which the direct messages of this round should be sent;
    /// if `None`, there are no direct messages in this round.
    pub fn direct_message_destinations(&self) -> Option<Vec<Verifier>> {
        match &self.tp {
            SessionType::Normal(round) => round.direct_message_destinations().map(|range| {
                range
                    .map(|idx| self.context.verifiers[idx].clone())
                    .collect()
            }),
            _ => None,
        }
    }

    /// Returns the direct message for the given destination
    /// (must be one of those returned by [`Self::direct_message_destinations`].
    pub fn make_direct_message(
        &self,
        rng: &mut impl CryptoRngCore,
        destination: &Verifier,
    ) -> Result<(SignedMessage<Sig>, Artefact), LocalError> {
        match &self.tp {
            SessionType::Normal(round) => {
                let destination_idx = *self
                    .context
                    .verifier_to_idx
                    .get(destination)
                    .ok_or(LocalError::VerifierNotFound(format!("{:?}", destination)))?;
                let round_num = round.round_num();
                let (payload, artefact) = round.make_direct_message(rng, destination_idx)?;
                let message = VerifiedMessage::new(
                    rng,
                    &self.context.signer,
                    &self.context.session_id,
                    round_num,
                    MessageType::Direct,
                    &payload,
                )?
                .into_unverified();
                Ok((
                    message,
                    Artefact {
                        destination: destination_idx,
                        artefact,
                    },
                ))
            }
            _ => Err(LocalError::InvalidState(
                "This is a consensus broadcast round which does not send direct messages".into(),
            )),
        }
    }

    /// Process a received message from another party.
    pub fn verify_message(
        &self,
        from: &Verifier,
        message: SignedMessage<Sig>,
    ) -> Result<ProcessedMessage<Sig>, Error<Res, Verifier>> {
        // This is an unprovable fault (may be a replay attack)
        if message.session_id() != &self.context.session_id {
            return Err(Error::Remote {
                party: from.clone(),
                error: RemoteError::UnexpectedSessionId,
            });
        }

        let message_for = match &self.tp {
            SessionType::Normal(round) => route_message_normal(round.as_ref(), &message),
            SessionType::Bc { next_round, .. } => route_message_bc(next_round.as_ref(), &message),
        };

        match message_for {
            MessageFor::ThisRound => self.verify_message_inner(from, message),
            // TODO: should we cache the verified or the unverified message?
            MessageFor::NextRound => Ok(ProcessedMessage(ProcessedMessageEnum::Cache {
                from: self.context.verifier_to_idx[from],
                message,
            })),
            // This is an unprovable fault (may be a replay attack)
            MessageFor::OutOfOrder => Err(Error::Remote {
                party: from.clone(),
                error: RemoteError::OutOfOrderMessage,
            }),
        }
    }

    fn verify_message_inner(
        &self,
        from: &Verifier,
        message: SignedMessage<Sig>,
    ) -> Result<ProcessedMessage<Sig>, Error<Res, Verifier>> {
        let verified_message = message.verify(from).map_err(|err| Error::Remote {
            party: from.clone(),
            error: RemoteError::InvalidSignature(err),
        })?;

        let from_idx = *self.context.verifier_to_idx.get(from).ok_or(Error::Local(
            LocalError::VerifierNotFound(format!("{:?}", from)),
        ))?;

        match &self.tp {
            SessionType::Normal(round) => {
                match verified_message.message_type() {
                    MessageType::Direct => {
                        let result =
                            round.verify_direct_message(from_idx, verified_message.payload());
                        let payload = wrap_receive_result(from, result)?;
                        Ok(ProcessedMessage(ProcessedMessageEnum::DmPayload {
                            from: from_idx,
                            payload,
                            message: verified_message,
                        }))
                    }
                    MessageType::Broadcast => {
                        let result = round.verify_broadcast(from_idx, verified_message.payload());
                        let payload = wrap_receive_result(from, result)?;
                        Ok(ProcessedMessage(ProcessedMessageEnum::BcPayload {
                            from: from_idx,
                            payload,
                            message: verified_message,
                        }))
                    }
                    _ => {
                        // TODO: this branch will never really be reached because we already routed
                        // the message in the calling method.
                        // Can we modify the code so that this branch is eliminated?
                        Err(Error::Local(LocalError::InvalidState(
                            "Unexpected broadcast consensus message".into(),
                        )))
                    }
                }
            }
            SessionType::Bc { bc, .. } => {
                bc.verify_broadcast(from_idx, verified_message)
                    .map_err(|err| Error::Provable {
                        party: from.clone(),
                        error: ProvableError::Consensus(err),
                    })?;
                Ok(ProcessedMessage(ProcessedMessageEnum::Bc {
                    from: from_idx,
                }))
            }
        }
    }

    /// Try to finalize the round.
    pub fn finalize_round(
        self,
        rng: &mut impl CryptoRngCore,
        accum: RoundAccumulator<Res, Sig>,
    ) -> Result<FinalizeOutcome<Res, Sig, Signer, Verifier>, Error<Res, Verifier>> {
        match self.tp {
            SessionType::Normal(round) => {
                Self::finalize_regular_round(self.context, round, rng, accum)
            }
            SessionType::Bc { next_round, .. } => {
                Self::finalize_bc_round(self.context, next_round, accum)
            }
        }
    }

    fn finalize_regular_round(
        context: Context<Signer, Verifier>,
        round: Box<dyn DynFinalizable<Res>>,
        rng: &mut impl CryptoRngCore,
        accum: RoundAccumulator<Res, Sig>,
    ) -> Result<FinalizeOutcome<Res, Sig, Signer, Verifier>, Error<Res, Verifier>> {
        let requires_bc = round.requires_broadcast_consensus();

        let outcome = round
            .finalize(rng, accum.processed)
            .map_err(|err| match err {
                type_erased::FinalizeError::Protocol(err) => match err {
                    cggmp21::FinalizeError::Provable { party, error } => {
                        let party = context.verifiers[party.as_usize()].clone();
                        Error::Provable {
                            party,
                            error: ProvableError::Protocol(error),
                        }
                    }
                    cggmp21::FinalizeError::Init(err) => {
                        Error::Local(LocalError::Init(format!("{:?}", err)))
                    }
                    cggmp21::FinalizeError::Proof(proof) => Error::Proof { proof },
                },
                type_erased::FinalizeError::Accumulator(err) => {
                    Error::Local(LocalError::AccumFinalize(err))
                }
            })?;

        let cached_messages = accum
            .cached_messages
            .into_iter()
            .map(|(idx, message)| (context.verifiers[idx.as_usize()].clone(), message))
            .collect();

        match outcome {
            type_erased::FinalizeOutcome::Success(res) => Ok(FinalizeOutcome::Success(res)),
            type_erased::FinalizeOutcome::AnotherRound(next_round) => {
                if requires_bc {
                    let broadcasts = accum.received_broadcasts;
                    let bc = BroadcastConsensus::new(broadcasts);
                    let session = Session {
                        tp: SessionType::Bc { next_round, bc },
                        context,
                    };
                    Ok(FinalizeOutcome::AnotherRound {
                        session,
                        cached_messages,
                    })
                } else {
                    let session = Session {
                        tp: SessionType::Normal(next_round),
                        context,
                    };
                    Ok(FinalizeOutcome::AnotherRound {
                        session,
                        cached_messages,
                    })
                }
            }
        }
    }

    fn finalize_bc_round(
        context: Context<Signer, Verifier>,
        round: Box<dyn DynFinalizable<Res>>,
        accum: RoundAccumulator<Res, Sig>,
    ) -> Result<FinalizeOutcome<Res, Sig, Signer, Verifier>, Error<Res, Verifier>> {
        accum
            .bc_accum
            .finalize()
            .ok_or(Error::Local(LocalError::InvalidState(
                "Cannot finalize".into(),
            )))?;

        let cached_messages = accum
            .cached_messages
            .into_iter()
            .map(|(idx, message)| (context.verifiers[idx.as_usize()].clone(), message))
            .collect();

        let session = Session {
            tp: SessionType::Normal(round),
            context,
        };

        Ok(FinalizeOutcome::AnotherRound {
            session,
            cached_messages,
        })
    }
}

pub struct RoundAccumulator<Res: ProtocolResult, Sig> {
    received_direct_messages: Vec<(PartyIdx, VerifiedMessage<Sig>)>,
    received_broadcasts: Vec<(PartyIdx, VerifiedMessage<Sig>)>,
    processed: DynRoundAccum,
    cached_messages: Vec<(PartyIdx, SignedMessage<Sig>)>,
    bc_accum: BcConsensusAccum,
    phantom_res: PhantomData<Res>,
}

impl<Res: ProtocolResult, Sig> RoundAccumulator<Res, Sig> {
    fn new(num_parties: usize, party_idx: PartyIdx, is_bc_round: bool, is_dm_round: bool) -> Self {
        // TODO: can return an error if party_idx is out of bounds
        Self {
            received_direct_messages: Vec::new(),
            received_broadcasts: Vec::new(),
            processed: DynRoundAccum::new(num_parties, party_idx, is_bc_round, is_dm_round),
            cached_messages: Vec::new(),
            bc_accum: BcConsensusAccum::new(num_parties, party_idx),
            phantom_res: PhantomData,
        }
    }

    /// Save an artefact produced by [`Session::make_direct_message`].
    pub fn add_artefact(&mut self, artefact: Artefact) -> Result<(), LocalError> {
        self.processed
            .add_dm_artefact(artefact.destination, artefact.artefact)
            .map_err(LocalError::AccumAdd)
    }

    /// Save a processed message produced by [`Session::verify_message`].
    pub fn add_processed_message(&mut self, pm: ProcessedMessage<Sig>) -> Result<(), LocalError> {
        // TODO: add a check that the index is in range, and wasn't filled yet
        match pm.0 {
            ProcessedMessageEnum::BcPayload {
                from,
                payload,
                message,
            } => {
                self.processed
                    .add_bc_payload(from, payload)
                    .map_err(LocalError::AccumAdd)?;
                self.received_broadcasts.push((from, message));
            }
            ProcessedMessageEnum::DmPayload {
                from,
                payload,
                message,
            } => {
                self.processed
                    .add_dm_payload(from, payload)
                    .map_err(LocalError::AccumAdd)?;
                self.received_direct_messages.push((from, message));
            }
            ProcessedMessageEnum::Cache { from, message } => {
                // TODO: check at this stage that there are no duplicate messages,
                // without waiting for the next round
                self.cached_messages.push((from, message));
            }
            ProcessedMessageEnum::Bc { from } => {
                self.bc_accum.add_echo_received(from).unwrap();
            }
        }
        Ok(())
    }
}

pub struct Artefact {
    destination: PartyIdx,
    artefact: DynDmArtefact,
}

pub struct ProcessedMessage<Sig>(ProcessedMessageEnum<Sig>);

enum ProcessedMessageEnum<Sig> {
    BcPayload {
        from: PartyIdx,
        payload: DynBcPayload,
        message: VerifiedMessage<Sig>,
    },
    DmPayload {
        from: PartyIdx,
        payload: DynDmPayload,
        message: VerifiedMessage<Sig>,
    },
    Cache {
        from: PartyIdx,
        message: SignedMessage<Sig>,
    },
    Bc {
        from: PartyIdx,
    },
}
