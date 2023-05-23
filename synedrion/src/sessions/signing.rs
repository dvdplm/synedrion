use alloc::string::String;

use rand_core::{CryptoRng, RngCore};

use super::generic::{SessionState, Stage, ToSendSerialized};
use crate::protocols::common::{PartyIdx, PresigningData, SessionId};
use crate::protocols::signing::Round1;
use crate::tools::group::{Point, Scalar, Signature};

#[derive(Clone)]
enum SigningStage {
    Round1(Stage<Round1>),
    Result(Signature),
}

#[derive(Clone)]
pub struct SigningState(SigningStage);

impl SessionState for SigningState {
    type Result = Signature;
    type Context = (PresigningData, Scalar, Point);

    fn new(
        _rng: &mut (impl RngCore + CryptoRng),
        _session_id: &SessionId,
        context: &Self::Context,
        _index: PartyIdx,
    ) -> Self {
        let (presigning_data, message, verifying_key) = context;
        let round1 = Round1::new(presigning_data, message, verifying_key);
        Self(SigningStage::Round1(Stage::new(round1)))
    }

    fn get_messages(
        &mut self,
        rng: &mut (impl RngCore + CryptoRng),
        num_parties: usize,
        index: PartyIdx,
    ) -> Result<ToSendSerialized, String> {
        Ok(match &mut self.0 {
            SigningStage::Round1(r) => r.get_messages(rng, num_parties, index)?,
            _ => return Err("Not in a sending state".into()),
        })
    }

    fn receive_current_stage(
        &mut self,
        from: PartyIdx,
        message_bytes: &[u8],
    ) -> Result<(), String> {
        match &mut self.0 {
            SigningStage::Round1(r) => r.receive(from, message_bytes),
            _ => Err("Not in a receiving stage".into()),
        }
    }

    fn is_finished_receiving(&self) -> Result<bool, String> {
        match &self.0 {
            SigningStage::Round1(r) => r.is_finished_receiving(),
            _ => Err("Not in a receiving stage".into()),
        }
    }

    fn finalize_stage(self, rng: &mut (impl RngCore + CryptoRng)) -> Result<Self, String> {
        Ok(Self(match self.0 {
            SigningStage::Round1(r) => SigningStage::Result(r.finalize(rng)?),
            _ => return Err("Not in a receiving stage".into()),
        }))
    }

    fn result(&self) -> Result<Self::Result, String> {
        match &self.0 {
            SigningStage::Result(r) => Ok(r.clone()),
            _ => Err("Not in the result stage".into()),
        }
    }

    fn is_final_stage(&self) -> bool {
        matches!(self.0, SigningStage::Result(_))
    }

    fn current_stage_num(&self) -> u8 {
        match self.0 {
            SigningStage::Round1(_) => 1,
            _ => panic!(),
        }
    }

    fn stages_num(&self) -> u8 {
        1
    }
}
