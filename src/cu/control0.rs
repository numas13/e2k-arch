use super::Ct;
use crate::raw;
use crate::raw::types::Ctpr;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeError {
    #[error("Instruction \"ldisp\" expects %ctpr2 but found {0}")]
    InvalidLDispCtpr(Ctpr),
    #[error("Instruction \"return\" expects %ctpr3 but found {0}")]
    InvalidReturnCtpr(Ctpr),
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
    Return(Ctpr),
    Gettsd(Ctpr),
    IBranch(Disp),
    Pref(Ipr, PrefDisp, bool),
    Puttsd(Disp),
    Done { trar: bool, fdam: bool },
}

impl Control0 {
    pub fn display<'a>(&'a self, ct: Option<&'a Ct>) -> impl fmt::Display + 'a {
        struct Display<'a> {
            control0: &'a Control0,
            ct: Option<&'a Ct>,
        }

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.control0 {
                    Control0::Disp(ctpr, disp) => writeln!(f, "disp {}, {}", ctpr, disp),
                    Control0::LDisp(ctpr, disp) => writeln!(f, "ldisp {}, {}", ctpr, disp),
                    Control0::SDisp(ctpr, disp) => writeln!(f, "sdisp {}, {}", ctpr, disp),
                    Control0::Return(ctpr) => writeln!(f, "return {}", ctpr),
                    Control0::Gettsd(ctpr) => writeln!(f, "gettsd {}", ctpr),
                    Control0::IBranch(disp) => {
                        write!(f, "ibranch {}", disp)?;
                        if let Some(ct) = self.ct {
                            write!(f, "{}", ct.op)?;
                        }
                        writeln!(f)
                    }
                    Control0::Pref(ipr, disp, ipd) => {
                        writeln!(f, "pref {}, disp = {}, ipd = {}", ipr, disp, *ipd as u8)
                    }
                    Control0::Puttsd(disp) => writeln!(f, "puttsd {}", disp),
                    Control0::Done { trar, fdam } => {
                        if let Some(ct) = self.ct {
                            write!(
                                f,
                                "done trar = {}, fdam = {}{}",
                                *trar as u8, *fdam as u8, ct.op
                            )
                        } else {
                            unreachable!()
                        }
                    }
                }
            }
        }

        Display { control0: self, ct }
    }
}

impl TryFrom<raw::Cs0> for Control0 {
    type Error = DecodeError;
    fn try_from(cs0: raw::Cs0) -> Result<Self, Self::Error> {
        let ret = if let Some(ctpr) = Ctpr::new(cs0.ctpr()) {
            let disp = Disp::new_truncate(cs0.disp());
            match cs0.op() {
                0 => Self::Disp(ctpr, disp),
                1 => {
                    if ctpr != Ctpr::new_clamp(2) {
                        return Err(DecodeError::InvalidLDispCtpr(ctpr));
                    }
                    Self::LDisp(ctpr, disp)
                }
                2 => Self::SDisp(ctpr, disp),
                3 => {
                    if disp.get() & 1 == 0 {
                        if ctpr != Ctpr::new_clamp(3) {
                            return Err(DecodeError::InvalidReturnCtpr(ctpr));
                        }
                        Self::Return(ctpr)
                    } else {
                        Self::Gettsd(ctpr)
                    }
                }
                _ => unreachable!(),
            }
        } else {
            match cs0.op() {
                0 => Self::IBranch(Disp::new_truncate(cs0.disp())),
                1 => Self::Pref(
                    Ipr::new_truncate(cs0.pref_ipr()),
                    PrefDisp::new_truncate(cs0.pref_disp()),
                    cs0.pref_ipd(),
                ),
                2 => Self::Puttsd(Disp::new_truncate(cs0.disp())),
                3 => Self::Done {
                    trar: cs0.done_trar(),
                    fdam: cs0.done_fdam(),
                },
                _ => unreachable!(),
            }
        };
        Ok(ret)
    }
}

impl Into<raw::Cs0> for Control0 {
    fn into(self) -> raw::Cs0 {
        let mut cs0 = raw::Cs0::default();
        match self {
            Self::Disp(ctpr, disp) => {
                cs0.set_ctpr(ctpr.get());
                cs0.set_op(0);
                cs0.set_disp(disp.get());
            }
            Self::LDisp(ctpr, disp) => {
                cs0.set_ctpr(ctpr.get());
                cs0.set_op(1);
                cs0.set_disp(disp.get());
            }
            Self::SDisp(ctpr, disp) => {
                cs0.set_ctpr(ctpr.get());
                cs0.set_op(2);
                cs0.set_disp(disp.get());
            }
            Self::Return(ctpr) => {
                cs0.set_ctpr(ctpr.get());
                cs0.set_op(3);
            }
            Self::Gettsd(ctpr) => {
                cs0.set_ctpr(ctpr.get());
                cs0.set_op(3);
                cs0.set_disp(1);
            }
            Self::IBranch(disp) => {
                cs0.set_op(0);
                cs0.set_disp(disp.get());
            }
            Self::Pref(ipr, disp, ipd) => {
                cs0.set_op(1);
                cs0.set_pref_ipr(ipr.get());
                cs0.set_pref_disp(disp.get());
                cs0.set_pref_ipd(ipd);
            }
            Self::Puttsd(disp) => {
                cs0.set_op(2);
                cs0.set_disp(disp.get());
            }
            Self::Done { trar, fdam } => {
                cs0.set_op(3);
                cs0.set_done_trar(trar);
                cs0.set_done_fdam(fdam);
            }
        }
        cs0
    }
}
