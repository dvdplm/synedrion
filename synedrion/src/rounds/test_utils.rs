use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt::Debug;
use core::hash::Hash;

use displaydoc::Display;
use hashbrown::{HashMap, HashSet};
use rand_core::CryptoRngCore;
use serde::Serialize;

use super::generic::{FinalizableToNextRound, FinalizableToResult, ProtocolResult, Round};
use super::FinalizeError;

/// A simple identity type for tests.
#[derive(Copy, Clone, Debug, Hash, PartialOrd, Ord, PartialEq, Eq, Serialize)]
pub(crate) struct Id(pub(crate) u32);

#[derive(Debug, Display)]
pub(crate) enum StepError<I: Debug + Hash> {
    /// Error when finalizing the round (missing messages).
    AccumFinalize,
    /// Error when verifying a received message.
    #[displaydoc("Error when verifying a received message ({0})")]
    Receive(String),
    /// A party attempted to send a message to itself.
    #[displaydoc("A party {0:?} attempted to send a message to itself")]
    MessageToItself(I),
}

pub(crate) struct AssembledRound<I, R>
where
    I: Ord + Clone + Hash,
    R: Round<I>,
{
    round: R,
    payloads: HashMap<I, <R as Round<I>>::Payload>,
    artifacts: HashMap<I, <R as Round<I>>::Artifact>,
}

pub(crate) fn step_round<I, R>(
    rng: &mut impl CryptoRngCore,
    rounds: HashMap<I, R>,
) -> Result<HashMap<I, AssembledRound<I, R>>, StepError<I>>
where
    R: Round<I>,
    <R as Round<I>>::BroadcastMessage: Clone,
    I: Debug + Clone + Hash + Ord + PartialEq,
{
    // Collect outgoing messages

    let mut artifact_accums = rounds
        .keys()
        .cloned()
        .map(|id| (id, HashMap::new()))
        .collect::<HashMap<_, _>>();

    // `to, from, message`
    let mut messages = Vec::<(
        I,
        I,
        (
            <R as Round<I>>::BroadcastMessage,
            <R as Round<I>>::DirectMessage,
        ),
    )>::new();

    for (from, round) in rounds.iter() {
        let destinations = round.message_destinations();
        let broadcast = round.make_broadcast_message(rng);

        for to in destinations {
            if to == from {
                return Err(StepError::MessageToItself(from.clone()));
            }

            let (direct, artifact) = round.make_direct_message(rng, to);
            // Can unwrap here since the destinations list is not empty
            messages.push((
                to.clone(),
                from.clone(),
                (broadcast.clone().unwrap(), direct),
            ));
            artifact_accums
                .get_mut(from)
                .unwrap()
                .insert(to.clone(), artifact);
        }
    }

    // Deliver messages

    let mut payload_accums = rounds
        .keys()
        .cloned()
        .map(|id| (id, HashMap::new()))
        .collect::<HashMap<_, _>>();
    for (to, from, (broadcast, direct)) in messages.into_iter() {
        let round = &rounds[&to];
        let payload = round
            .verify_message(rng, &from, broadcast, direct)
            .map_err(|err| StepError::Receive(format!("{:?}", err)))?;
        payload_accums.get_mut(&to).unwrap().insert(from, payload);
    }

    // Assemble

    let mut assembled = HashMap::new();
    for (id, round) in rounds.into_iter() {
        let (_, payloads) = payload_accums.remove_entry(&id).unwrap();
        let (_, artifacts) = artifact_accums.remove_entry(&id).unwrap();
        let received = payloads.keys().cloned().collect::<HashSet<_>>();
        if !round.can_finalize(&received) {
            return Err(StepError::AccumFinalize);
        };

        assembled.insert(
            id,
            AssembledRound {
                round,
                payloads,
                artifacts,
            },
        );
    }
    Ok(assembled)
}

pub(crate) fn step_next_round<I, R>(
    rng: &mut impl CryptoRngCore,
    assembled_rounds: HashMap<I, AssembledRound<I, R>>,
) -> Result<HashMap<I, R::NextRound>, FinalizeError<R::Result>>
where
    I: Ord + Clone + Hash,
    R: FinalizableToNextRound<I>,
{
    let mut results = HashMap::new();
    for (id, assembled_round) in assembled_rounds.into_iter() {
        let next_round = assembled_round.round.finalize_to_next_round(
            rng,
            assembled_round.payloads,
            assembled_round.artifacts,
        )?;
        results.insert(id, next_round);
    }
    Ok(results)
}

#[allow(clippy::type_complexity)]
pub(crate) fn step_result<I, R>(
    rng: &mut impl CryptoRngCore,
    assembled_rounds: HashMap<I, AssembledRound<I, R>>,
) -> Result<HashMap<I, <R::Result as ProtocolResult>::Success>, FinalizeError<R::Result>>
where
    I: Ord + Clone + Hash,
    R: FinalizableToResult<I>,
{
    let mut results = HashMap::new();
    for (id, assembled_round) in assembled_rounds.into_iter() {
        let next_round = assembled_round.round.finalize_to_result(
            rng,
            assembled_round.payloads,
            assembled_round.artifacts,
        )?;
        results.insert(id, next_round);
    }
    Ok(results)
}

pub(crate) trait Without {
    type Item;
    fn without(self, item: &Self::Item) -> Self;
}

impl<T: Ord + Hash> Without for HashSet<T> {
    type Item = T;
    fn without(self, item: &Self::Item) -> Self {
        let mut set = self;
        set.remove(item);
        set
    }
}
