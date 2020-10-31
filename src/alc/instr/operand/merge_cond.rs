use crate::alc::instr::RawInstr;
use crate::raw::Rlp;
use crate::state::pred::Pred;
use crate::InsertInto;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("MRGC not found")]
pub struct NotFoundError;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MergeCond {
    IfTrue(Pred),
    IfFalse(Pred),
}

impl MergeCond {
    pub fn from_raw(index: usize, rlp: &Rlp) -> Option<Self> {
        if rlp.check_channel_mrgc(index) {
            let pred: Pred = rlp.psrc().into();
            if rlp.invert_mask() & 1 << index % 3 == 0 {
                Some(Self::IfTrue(pred))
            } else {
                Some(Self::IfFalse(pred))
            }
        } else {
            None
        }
    }
}

impl TryFrom<&'_ RawInstr> for MergeCond {
    type Error = NotFoundError;
    fn try_from(raw: &RawInstr) -> Result<Self, Self::Error> {
        raw.mrgc.ok_or(NotFoundError)
    }
}

impl InsertInto<RawInstr> for MergeCond {
    fn insert_into(self, raw: &mut RawInstr) {
        raw.mrgc = Some(self);
    }
}

impl fmt::Display for MergeCond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IfTrue(pred) => fmt::Display::fmt(pred, f),
            Self::IfFalse(pred) => write!(f, "~{}", pred),
        }
    }
}
