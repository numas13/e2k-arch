use crate::raw;
use crate::raw::types::Rndpred;
use crate::state::pred::Pred;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Invalid ELP {0:#x}")]
    InvalidElp(u8),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Elp {
    Pred(Pred),
    Bgrpred,
    Rndpred(Rndpred),
}

impl TryFrom<raw::Elp> for Elp {
    type Error = DecodeError;
    fn try_from(raw: raw::Elp) -> Result<Self, Self::Error> {
        if let Some(pred) = raw.pred() {
            Ok(Self::Pred(pred.into()))
        } else if raw.is_bgrpred() {
            Ok(Self::Bgrpred)
        } else if let Some(value) = raw.rndpred() {
            Ok(Self::Rndpred(Rndpred::new_truncate(value)))
        } else {
            Err(DecodeError::InvalidElp(raw.0))
        }
    }
}

impl Into<raw::Elp> for Elp {
    fn into(self) -> raw::Elp {
        let mut raw = raw::Elp::default();
        match self {
            Self::Pred(p) => raw.set_pred(p.into()),
            Self::Bgrpred => raw.set_bgrpred(),
            Self::Rndpred(p) => unsafe { raw.set_rndpred(p.get()) },
        };
        raw
    }
}

impl fmt::Display for Elp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Pred(p) => fmt::Display::fmt(p, fmt),
            Self::Bgrpred => fmt.write_str("%bgrpred"),
            Self::Rndpred(p) => fmt::Display::fmt(p, fmt),
        }
    }
}
