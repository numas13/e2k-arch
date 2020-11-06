pub use crate::raw::types::{ClpIdx, DtAl};

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
    #[error("Invalid clp index in %MLOCK condition")]
    InvalidClpIndex,
}

/// `%MLOCK` condition if compare instruction result.
///
/// All booleans are invert flags.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MlockCmp {
    /// Result of compare instruction on ALC0.
    Cmp0(bool),
    /// Result of compare instruction on ALC1.
    Cmp1(bool),
    /// Result of compare instruction on ALC3.
    Cmp3(bool),
    /// Result of compare instruction on ALC4.
    Cmp4(bool),
    /// Result of compare instructions on ALC0 and ALC1.
    Cmp0or1(bool, bool),
    /// Result of compare instructions on ALC3 and ALC4.
    Cmp3or4(bool, bool),
}

/// Special condition for `%MLOCK`.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MlockCond {
    /// Result of CLP/MLP instruction.
    Clp(ClpIdx, bool),
    /// Result of compare instructions on AL channels.
    Cmp(MlockCmp),
    /// TODO
    DtAl(DtAl),
}

/// Control transfer type.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CtOp {
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

impl CtOp {
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
                    MlockCond::Clp(idx, raw.inv())
                } else if raw.is_cmp_pair() {
                    let cmp = if raw.is_cmp3_4_pair() {
                        MlockCmp::Cmp3or4(raw.inv0_3(), raw.inv1_4())
                    } else {
                        MlockCmp::Cmp0or1(raw.inv0_3(), raw.inv1_4())
                    };
                    MlockCond::Cmp(cmp)
                } else {
                    let inv = raw.inv();
                    let cmp = match raw.cmp_idx() {
                        0 => MlockCmp::Cmp0(inv),
                        1 => MlockCmp::Cmp1(inv),
                        2 => MlockCmp::Cmp3(inv),
                        3 => MlockCmp::Cmp4(inv),
                        _ => unreachable!(),
                    };
                    MlockCond::Cmp(cmp)
                };
                Self::Mlock(cond)
            }
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
            Self::Mlock(cond) => match cond {
                MlockCond::DtAl(dt_al) => {
                    ct.set_op(raw::Ct::OP_MLOCK);
                    ct.set_dt_al(dt_al);
                }
                MlockCond::Clp(idx, inv) => {
                    ct.set_op(raw::Ct::OP_MLOCK_OR_CMP_CLP);
                    ct.set_clp_idx(idx);
                    ct.set_inv(inv);
                }
                MlockCond::Cmp(cmp) => {
                    ct.set_op(raw::Ct::OP_MLOCK_OR_CMP_CLP);
                    match cmp {
                        MlockCmp::Cmp0(inv) => {
                            ct.set_cmp_idx(0);
                            ct.set_inv(inv);
                        }
                        MlockCmp::Cmp1(inv) => {
                            ct.set_cmp_idx(1);
                            ct.set_inv(inv);
                        }
                        MlockCmp::Cmp3(inv) => {
                            ct.set_cmp_idx(2);
                            ct.set_inv(inv);
                        }
                        MlockCmp::Cmp4(inv) => {
                            ct.set_cmp_idx(3);
                            ct.set_inv(inv);
                        }
                        MlockCmp::Cmp0or1(inv0, inv1) => {
                            ct.set_cmp_pair(true);
                            ct.set_inv0_3(inv0);
                            ct.set_inv1_4(inv1);
                        }
                        MlockCmp::Cmp3or4(inv3, inv4) => {
                            ct.set_cmp_pair(true);
                            ct.set_cmp3_4_pair(true);
                            ct.set_inv0_3(inv3);
                            ct.set_inv1_4(inv4);
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

impl fmt::Display for CtOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn inv_s(inv: bool) -> &'static str {
            if inv {
                "~"
            } else {
                ""
            }
        }

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
                    MlockCond::Clp(idx, inv) => {
                        write!(f, " || {}%clp{}", inv_s(*inv), idx)?;
                    }
                    MlockCond::Cmp(cmp) => {
                        f.write_str(" || ")?;
                        match cmp {
                            MlockCmp::Cmp0(inv) => write!(f, "{}%cmp0", inv_s(*inv))?,
                            MlockCmp::Cmp1(inv) => write!(f, "{}%cmp1", inv_s(*inv))?,
                            MlockCmp::Cmp3(inv) => write!(f, "{}%cmp3", inv_s(*inv))?,
                            MlockCmp::Cmp4(inv) => write!(f, "{}%cmp4", inv_s(*inv))?,
                            MlockCmp::Cmp0or1(inv0, inv1) => {
                                write!(f, "{}%cmp0 || {}%cmp1", inv_s(*inv0), inv_s(*inv1))?
                            }
                            MlockCmp::Cmp3or4(inv3, inv4) => {
                                write!(f, "{}%cmp3 || {}%cmp4", inv_s(*inv3), inv_s(*inv4))?
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
