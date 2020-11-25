use super::ct::CtCond;
use crate::raw::types::Ctpr;
use crate::raw::{self, Unpacked};
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeError {
    #[error("Instruction \"ldisp\" expects %ctpr2 but found {0}")]
    InvalidLDispCtpr(Ctpr),
    #[error("Instruction \"return\" expects %ctpr3 but found {0}")]
    InvalidReturnCtpr(Ctpr),
    #[error("Invalid CS0 misc type {0:#x}")]
    InvalidMiscType(u8),
    #[error("SS not found")]
    SsNotFound,
    #[error("Invalid ctpr register in SS")]
    InvalidSsCtpr,
    #[error("Failed to decode control transfer condition")]
    CondError {
        #[from]
        surce: crate::cu::ct::DecodeError,
    },
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Disp(i32) {
        const MASK = 0x7ff_ffff;
        const FMT = "{:#x}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Ipr(u8) {
        const MASK = 0x7;
        const FMT = "%ipr{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct PrefDisp(u32) {
        const MASK = 0x07f_ffff;
        const FMT = "{:#x}";
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Control0 {
    Disp(Ctpr, Disp),
    LDisp(Ctpr, Disp),
    SDisp(Ctpr, Disp),
    Return,
    Gettsd,
    IBranch(Disp, Option<CtCond>),
    Pref(Ipr, PrefDisp, bool),
    Puttsd(Disp),
    Done {
        trar: bool,
        fdam: bool,
        cond: Option<CtCond>,
    },
}

impl Control0 {
    pub fn unpack_from(raw: &Unpacked) -> Result<Self, DecodeError> {
        let cs0 = raw.cs0;
        let ret = if let Some(ctpr) = Ctpr::new(cs0.ctpr()) {
            let disp = Disp::new_truncate(cs0.disp());
            match cs0.op() {
                raw::Cs0::OP1_DISP => Self::Disp(ctpr, disp),
                raw::Cs0::OP1_LDISP => {
                    if ctpr != Ctpr::new_clamp(2) {
                        return Err(DecodeError::InvalidLDispCtpr(ctpr));
                    }
                    Self::LDisp(ctpr, disp)
                }
                raw::Cs0::OP1_SDISP => Self::SDisp(ctpr, disp),
                raw::Cs0::OP1_MISC => match cs0.misc_ty() {
                    raw::Cs0::MISC_TYPE_RETURN => {
                        if ctpr != Ctpr::new_clamp(3) {
                            return Err(DecodeError::InvalidReturnCtpr(ctpr));
                        }
                        Self::Return
                    }
                    raw::Cs0::MISC_TYPE_GETTSD => Self::Gettsd,
                    raw::Cs0::MISC_TYPE_HRET => todo!(),
                    raw::Cs0::MISC_TYPE_GLAUNCH => todo!(),
                    _ => return Err(DecodeError::InvalidMiscType(cs0.misc_ty())),
                },
                _ => unreachable!(),
            }
        } else {
            match cs0.op() {
                raw::Cs0::OP2_IBRANCH => {
                    if !raw.hs.ss() {
                        return Err(DecodeError::SsNotFound);
                    }
                    if raw.ss.ct().ctpr() != 0 {
                        return Err(DecodeError::InvalidSsCtpr);
                    }
                    Self::IBranch(
                        Disp::new_truncate(cs0.disp()),
                        CtCond::try_from(raw.ss.ct())?,
                    )
                }
                raw::Cs0::OP2_PREF => Self::Pref(
                    Ipr::new_truncate(cs0.pref_ipr()),
                    PrefDisp::new_truncate(cs0.pref_disp()),
                    cs0.pref_ipd(),
                ),
                raw::Cs0::OP2_PUTTSD => Self::Puttsd(Disp::new_truncate(cs0.disp())),
                raw::Cs0::OP2_DONE => {
                    if !raw.hs.ss() {
                        return Err(DecodeError::SsNotFound);
                    }
                    if raw.ss.ct().ctpr() != 0 {
                        return Err(DecodeError::InvalidSsCtpr);
                    }
                    Self::Done {
                        trar: cs0.done_trar(),
                        fdam: cs0.done_fdam(),
                        cond: CtCond::try_from(raw.ss.ct())?,
                    }
                }
                _ => unreachable!(),
            }
        };
        Ok(ret)
    }
    pub fn pack_into(self, raw: &mut Unpacked) {
        let cs0 = &mut raw.cs0;
        match self {
            Self::Disp(ctpr, disp) => {
                cs0.set_ctpr(ctpr.get());
                cs0.set_op(raw::Cs0::OP1_DISP);
                cs0.set_disp(disp.get());
            }
            Self::LDisp(ctpr, disp) => {
                cs0.set_ctpr(ctpr.get());
                cs0.set_op(raw::Cs0::OP1_LDISP);
                cs0.set_disp(disp.get());
            }
            Self::SDisp(ctpr, disp) => {
                cs0.set_ctpr(ctpr.get());
                cs0.set_op(raw::Cs0::OP1_SDISP);
                cs0.set_disp(disp.get());
            }
            Self::Return => {
                cs0.set_ctpr(3);
                cs0.set_op(raw::Cs0::OP1_MISC);
                cs0.set_misc_ty(raw::Cs0::MISC_TYPE_RETURN);
            }
            Self::Gettsd => {
                cs0.set_ctpr(2);
                cs0.set_op(raw::Cs0::OP1_MISC);
                cs0.set_misc_ty(raw::Cs0::MISC_TYPE_GETTSD);
            }
            Self::IBranch(disp, cond) => {
                cs0.set_op(raw::Cs0::OP2_IBRANCH);
                cs0.set_disp(disp.get());
                if let Some(cond) = cond {
                    // TODO: check ct before write?
                    raw.ss.set_ct(cond.into());
                }
            }
            Self::Pref(ipr, disp, ipd) => {
                cs0.set_op(raw::Cs0::OP2_PREF);
                cs0.set_pref_ipr(ipr.get());
                cs0.set_pref_disp(disp.get());
                cs0.set_pref_ipd(ipd);
            }
            Self::Puttsd(disp) => {
                cs0.set_op(raw::Cs0::OP2_PUTTSD);
                cs0.set_disp(disp.get());
            }
            Self::Done { trar, fdam, cond } => {
                cs0.set_op(raw::Cs0::OP2_DONE);
                cs0.set_done_trar(trar);
                cs0.set_done_fdam(fdam);
                if let Some(cond) = cond {
                    // TODO: check ct before write?
                    raw.ss.set_ct(cond.into());
                }
            }
        }
    }
}

impl fmt::Display for Control0 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Disp(ctpr, disp) => writeln!(f, "disp {}, {}", ctpr, disp),
            Self::LDisp(ctpr, disp) => writeln!(f, "ldisp {}, {}", ctpr, disp),
            Self::SDisp(ctpr, disp) => writeln!(f, "sdisp {}, {}", ctpr, disp),
            Self::Return => writeln!(f, "return %ctpr3"),
            Self::Gettsd => writeln!(f, "gettsd %ctpr2"),
            Self::IBranch(disp, cond) => {
                write!(f, "ibranch {}", disp)?;
                if let Some(cond) = cond {
                    cond.fmt(f)?;
                }
                writeln!(f)
            }
            Self::Pref(ipr, disp, ipd) => {
                writeln!(f, "pref {}, disp = {}, ipd = {}", ipr, disp, *ipd as u8)
            }
            Self::Puttsd(disp) => writeln!(f, "puttsd {}", disp),
            Self::Done { trar, fdam, cond } => {
                write!(f, "done trar = {}, fdam = {}", *trar as u8, *fdam as u8,)?;
                if let Some(cond) = cond {
                    cond.fmt(f)?;
                }
                writeln!(f)
            }
        }
    }
}
