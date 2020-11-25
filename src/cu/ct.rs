//! Control transfer.

pub use crate::raw::types::{ClpIdx, DtAl};

use crate::raw;
use crate::raw::types::Ctpr;
use crate::state::pred::Preg;
use core::fmt;
use thiserror::Error;

/// Error type for control transfer decoding.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Invalid control transfer condition {0:#x}")]
    InvalidType(u8),
    #[error("Invalid clp index in %MLOCK condition")]
    InvalidClpIndex,
}

/// When condition is `true`.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum When {
    /// If source is `true`.
    True,
    /// If source is `false`.
    False,
}

impl From<bool> for When {
    fn from(value: bool) -> Self {
        match value {
            true => Self::True,
            false => Self::False,
        }
    }
}

impl Into<bool> for When {
    fn into(self) -> bool {
        match self {
            Self::True => true,
            Self::False => false,
        }
    }
}

impl fmt::Display for When {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::True => Ok(()),
            Self::False => f.write_str("~"),
        }
    }
}

/// `%MLOCK` condition for results of compare instructions.
///
/// All booleans are invert flags.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MlockCmp {
    /// Result of compare instruction on ALC0.
    Cmp0(When),
    /// Result of compare instruction on ALC1.
    Cmp1(When),
    /// Result of compare instruction on ALC3.
    Cmp3(When),
    /// Result of compare instruction on ALC4.
    Cmp4(When),
    /// Result of compare instructions on ALC0 and ALC1.
    Cmp0or1(When, When),
    /// Result of compare instructions on ALC3 and ALC4.
    Cmp3or4(When, When),
}

/// Special condition for `%MLOCK`.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MlockCond {
    /// Result of CLP/MLP instruction.
    Clp(ClpIdx, When),
    /// Results of compare instructions on AL channels.
    Cmp(MlockCmp),
    /// TODO
    DtAl(DtAl),
}

/// Control transfer type.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CtCond {
    /// Jump always.
    Explicit,
    /// Jump if predicate register is `true`.
    Preg(Preg),
    /// Jump if loop end.
    LoopEnd,
    /// Jump if predicate register is `true` or loop end.
    PregOrLoopEnd(Preg),
    /// Jump if predicate register is `false`.
    NotPreg(Preg),
    /// Jump if not loop end.
    NotLoopEnd,
    /// Jump if predicate register is `false` and not loop end.
    NotPregAndNotLoopEnd(Preg),
    /// Jump of special condition.
    Mlock(MlockCond),
    /// Jump if predicate register is `false` or loop end.
    NotPregOrLoopEnd(Preg),
    /// Jump if predicate register is `true` and not loop end.
    PregAndNotLoopEnd(Preg),
}

impl CtCond {
    /// Tries to decode a control transfer type from raw value.
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
            raw::Ct::OP_MLOCK => Self::Mlock(MlockCond::DtAl(raw.dt_al())),
            raw::Ct::OP_MLOCK_OR_CMP_CLP => {
                let cond = if raw.is_clp() {
                    let idx = raw.clp_idx().ok_or(DecodeError::InvalidClpIndex)?;
                    MlockCond::Clp(idx, When::from(!raw.inv()))
                } else if raw.is_cmp_pair() {
                    let cmp = if raw.is_cmp3_4_pair() {
                        MlockCmp::Cmp3or4(When::from(!raw.inv0_3()), When::from(!raw.inv1_4()))
                    } else {
                        MlockCmp::Cmp0or1(When::from(!raw.inv0_3()), When::from(!raw.inv1_4()))
                    };
                    MlockCond::Cmp(cmp)
                } else {
                    let when = When::from(!raw.inv());
                    let cmp = match raw.cmp_idx() {
                        0 => MlockCmp::Cmp0(when),
                        1 => MlockCmp::Cmp1(when),
                        2 => MlockCmp::Cmp3(when),
                        3 => MlockCmp::Cmp4(when),
                        _ => unreachable!(),
                    };
                    MlockCond::Cmp(cmp)
                };
                Self::Mlock(cond)
            }
            raw::Ct::OP_NOT_PREG_OR_LOOP_END => Self::NotPregOrLoopEnd(preg),
            raw::Ct::OP_PREG_AND_NOT_LOOP_END => Self::PregAndNotLoopEnd(preg),
            _ => return Err(DecodeError::InvalidType(raw.op())),
        }))
    }
}

impl Into<raw::Ct> for CtCond {
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
            Self::Mlock(cond) => match cond {
                MlockCond::DtAl(dt_al) => {
                    ct.set_op(raw::Ct::OP_MLOCK);
                    ct.set_dt_al(dt_al);
                }
                MlockCond::Clp(idx, f) => {
                    ct.set_op(raw::Ct::OP_MLOCK_OR_CMP_CLP);
                    ct.set_clp_idx(idx);
                    ct.set_inv(f.into());
                }
                MlockCond::Cmp(cmp) => {
                    ct.set_op(raw::Ct::OP_MLOCK_OR_CMP_CLP);
                    match cmp {
                        MlockCmp::Cmp0(when) => {
                            ct.set_cmp_idx(0);
                            ct.set_inv(when.into());
                        }
                        MlockCmp::Cmp1(when) => {
                            ct.set_cmp_idx(1);
                            ct.set_inv(when.into());
                        }
                        MlockCmp::Cmp3(when) => {
                            ct.set_cmp_idx(2);
                            ct.set_inv(when.into());
                        }
                        MlockCmp::Cmp4(when) => {
                            ct.set_cmp_idx(3);
                            ct.set_inv(when.into());
                        }
                        MlockCmp::Cmp0or1(when0, when1) => {
                            ct.set_cmp_pair(true);
                            ct.set_inv0_3(when0.into());
                            ct.set_inv1_4(when1.into());
                        }
                        MlockCmp::Cmp3or4(when3, when4) => {
                            ct.set_cmp_pair(true);
                            ct.set_cmp3_4_pair(true);
                            ct.set_inv0_3(when3.into());
                            ct.set_inv1_4(when4.into());
                        }
                    }
                }
            },
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

impl fmt::Display for CtCond {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Explicit => Ok(()),
            Self::Preg(preg) => write!(f, " ? {}", preg),
            Self::NotPreg(preg) => write!(f, " ? ~ {}", preg),
            Self::LoopEnd => f.write_str(" ? #LOOP_END"),
            Self::NotLoopEnd => f.write_str(" ? #NOT_LOOP_END"),
            Self::PregOrLoopEnd(preg) => write!(f, " ? {} || #LOOP_END", preg),
            Self::NotPregAndNotLoopEnd(preg) => write!(f, " ? ~ {} && #NOT_LOOP_END", preg),
            Self::Mlock(cond) => {
                f.write_str(" ? %MLOCK")?;
                match cond {
                    MlockCond::DtAl(dt_al) => {
                        if cfg!(debug_assertions) && !dt_al.is_empty() {
                            f.write_str(" || %dt_al")?;
                            for i in 0..4 {
                                if dt_al.bits() & 1 << i != 0 {
                                    write!(f, "{}", [0, 1, 3, 4][i])?;
                                }
                            }
                        }
                    }
                    MlockCond::Clp(idx, when) => {
                        write!(f, " || {}%clp{}", when, idx)?;
                    }
                    MlockCond::Cmp(cmp) => {
                        f.write_str(" || ")?;
                        match cmp {
                            MlockCmp::Cmp0(when) => write!(f, "{}%cmp0", when)?,
                            MlockCmp::Cmp1(when) => write!(f, "{}%cmp1", when)?,
                            MlockCmp::Cmp3(when) => write!(f, "{}%cmp3", when)?,
                            MlockCmp::Cmp4(when) => write!(f, "{}%cmp4", when)?,
                            MlockCmp::Cmp0or1(when0, when1) => {
                                write!(f, "{}%cmp0 || {}%cmp1", when0, when1)?
                            }
                            MlockCmp::Cmp3or4(when3, when4) => {
                                write!(f, "{}%cmp3 || {}%cmp4", when3, when4)?
                            }
                        }
                    }
                }
                Ok(())
            }
            Self::NotPregOrLoopEnd(preg) => write!(f, " ? ~ {} || #LOOP_END", preg),
            Self::PregAndNotLoopEnd(preg) => write!(f, " ? {} && #NOT_LOOP_END", preg),
        }
    }
}

/// A control transfer instruction.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Ct {
    pub ctpr: Ctpr,
    pub cond: CtCond,
}

impl Ct {
    /// Create a control transfer instruction.
    pub const fn new(ctpr: Ctpr, cond: CtCond) -> Self {
        Self { ctpr, cond }
    }
    /// Tries to create a `Ct` from raw value.
    pub fn from_raw(raw: raw::Ct) -> Result<Option<Ct>, DecodeError> {
        if let Some(cond) = CtCond::try_from(raw)? {
            Ok(Ctpr::new(raw.ctpr()).map(|ctpr| Self::new(ctpr, cond)))
        } else {
            Ok(None)
        }
    }
}

impl Into<raw::Ct> for Ct {
    fn into(self) -> raw::Ct {
        let mut raw: raw::Ct = self.cond.into();
        raw.set_ctpr(self.ctpr.get());
        raw
    }
}

impl fmt::Display for Ct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ct {}{}", self.ctpr, self.cond)
    }
}
