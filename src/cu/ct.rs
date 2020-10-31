use super::{Control0, Control1};
use crate::raw;
use crate::raw::types::Ctpr;
use crate::state::pred::Preg;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Invalid control transfer {0:#x}")]
    InvalidOpcode(u8),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CtOp {
    Explicit,
    Preg(Preg),
    LoopEnd,
    PregOrLoopEnd(Preg),
    NotPreg(Preg),
    NotLoopEnd,
    NotPregAndNotLoopEnd(Preg),
    Mlock,
    MlockOrCmp0,
    NotPregOrLoopEnd(Preg),
    PregAndNotLoopEnd(Preg),
}

impl CtOp {
    pub fn try_from(raw: raw::Ct) -> Result<Option<Self>, DecodeError> {
        let preg = Preg::new_truncate(raw.preg());
        Ok(Some(match raw.op() {
            raw::Ct::OP_NONE => return Ok(None),
            raw::Ct::OP_EXPLICIT => Self::Explicit,
            raw::Ct::OP_PREG => Self::Preg(preg),
            raw::Ct::OP_NOT_PREG => Self::NotPreg(preg),
            raw::Ct::OP_LOOP_END => Self::LoopEnd,
            raw::Ct::OP_NOT_LOOP_END => Self::NotLoopEnd,
            raw::Ct::OP_PREG_OR_LOOP_END => Self::PregOrLoopEnd(preg),
            raw::Ct::OP_NOT_PREG_AND_NOT_LOOP_END => Self::NotPregAndNotLoopEnd(preg),
            raw::Ct::OP_MLOCK => Self::Mlock,
            raw::Ct::OP_MLOCK_OR_CMP0 => Self::MlockOrCmp0,
            raw::Ct::OP_NOT_PREG_OR_LOOP_END => Self::NotPregOrLoopEnd(preg),
            raw::Ct::OP_PREG_AND_NOT_LOOP_END => Self::PregAndNotLoopEnd(preg),
            _ => return Err(DecodeError::InvalidOpcode(raw.op())),
        }))
    }
}

impl Into<raw::Ct> for CtOp {
    fn into(self) -> raw::Ct {
        let mut ct = raw::Ct::default();
        match self {
            Self::Explicit => ct.set_op(raw::Ct::OP_EXPLICIT),
            Self::Preg(p) => {
                ct.set_op(raw::Ct::OP_PREG);
                ct.set_preg(p.get());
            }
            Self::NotPreg(p) => {
                ct.set_op(raw::Ct::OP_NOT_PREG);
                ct.set_preg(p.get());
            }
            Self::LoopEnd => ct.set_op(raw::Ct::OP_LOOP_END),
            Self::NotLoopEnd => ct.set_op(raw::Ct::OP_NOT_LOOP_END),
            Self::PregOrLoopEnd(p) => {
                ct.set_op(raw::Ct::OP_PREG_OR_LOOP_END);
                ct.set_preg(p.get());
            }
            Self::NotPregAndNotLoopEnd(p) => {
                ct.set_op(raw::Ct::OP_NOT_PREG_AND_NOT_LOOP_END);
                ct.set_preg(p.get());
            }
            Self::Mlock => ct.set_op(raw::Ct::OP_MLOCK),
            Self::MlockOrCmp0 => ct.set_op(raw::Ct::OP_MLOCK_OR_CMP0),
            Self::NotPregOrLoopEnd(p) => {
                ct.set_op(raw::Ct::OP_NOT_PREG_OR_LOOP_END);
                ct.set_preg(p.get());
            }
            Self::PregAndNotLoopEnd(p) => {
                ct.set_op(raw::Ct::OP_PREG_AND_NOT_LOOP_END);
                ct.set_preg(p.get());
            }
        }
        ct
    }
}

impl fmt::Display for CtOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Explicit => Ok(()),
            Self::Preg(preg) => write!(f, " ? {}", preg),
            Self::NotPreg(preg) => write!(f, " ? ~ {}", preg),
            Self::LoopEnd => f.write_str(" ? #LOOP_END"),
            Self::NotLoopEnd => f.write_str(" ? #NOT_LOOP_END"),
            Self::PregOrLoopEnd(preg) => write!(f, " ? {} || #LOOP_END", preg),
            Self::NotPregAndNotLoopEnd(preg) => write!(f, " ? ~ {} && #NOT_LOOP_END", preg),
            Self::Mlock => f.write_str(" ? %MLOCK"),
            Self::MlockOrCmp0 => f.write_str(" ? %MLOCK || %cmp0"),
            Self::NotPregOrLoopEnd(preg) => write!(f, " ? ~ {} || #LOOP_END", preg),
            Self::PregAndNotLoopEnd(preg) => write!(f, " ? {} && #NOT_LOOP_END", preg),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Ct {
    pub ctpr: Option<Ctpr>,
    pub op: CtOp,
}
impl Ct {
    pub const fn new(ctpr: Ctpr, op: CtOp) -> Self {
        Self {
            ctpr: Some(ctpr),
            op,
        }
    }
    pub fn print(
        &self,
        fmt: &mut fmt::Formatter,
        c0: Option<&Control0>,
        c1: Option<&Control1>,
    ) -> fmt::Result {
        match (c0, c1) {
            (Some(Control0::IBranch(_)), _) => Ok(()),
            (_, Some(Control1::Call { .. })) => Ok(()),
            _ => writeln!(fmt, "{}", self),
        }
    }
    pub fn from_raw(raw: raw::Ct) -> Result<Option<Ct>, DecodeError> {
        if let Some(op) = CtOp::try_from(raw)? {
            let ctpr = Ctpr::new(raw.ctpr());
            Ok(Some(Self { ctpr, op }))
        } else {
            Ok(None)
        }
    }
}

impl Into<raw::Ct> for Ct {
    fn into(self) -> raw::Ct {
        let mut raw: raw::Ct = self.op.into();
        if let Some(ctpr) = self.ctpr {
            raw.set_ctpr(ctpr.get());
        }
        raw
    }
}

impl fmt::Display for Ct {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ctpr) = self.ctpr {
            write!(fmt, "ct {}{}", ctpr, self.op)?;
        }
        Ok(())
    }
}
