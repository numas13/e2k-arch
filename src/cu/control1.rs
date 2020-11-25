use super::Ct;
use crate::raw;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("CS1_LTS0 not found")]
    LtsNotFound,
    #[error("unknown CS1 {0:#10x}")]
    Unknown(u32),
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Setwd {
    pub wsz: u8,
    pub nfx: bool,
    pub dbl: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Vfrpsz {
    pub rpsz: u8,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Setbp {
    pub psz: u8,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Setbn {
    pub rsz: u8,
    pub rbs: u8,
    pub rcur: u8,
}

// deleted
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Settr {
    pub ty: u16,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Sets {
    pub setwd: Option<Setwd>,
    pub vfrpsz: Option<Vfrpsz>,
    pub setbp: Option<Setbp>,
    pub setbn: Option<Setbn>,
    pub settr: Option<Settr>,
}

impl fmt::Display for Sets {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(setwd) = self.setwd {
            writeln!(
                fmt,
                "setwd wsz = {:#x}, nfx = {:#x}, dbl = {:#x}",
                setwd.wsz, setwd.nfx as u8, setwd.dbl as u8
            )?;
        }
        if let Some(vfrpsz) = self.vfrpsz {
            writeln!(fmt, "vfrpsz rpsz = {:#x}", vfrpsz.rpsz)?;
        }
        if let Some(setbp) = self.setbp {
            writeln!(fmt, "setbp psz = {:#x}", setbp.psz)?;
        }
        if let Some(setbn) = self.setbn {
            writeln!(
                fmt,
                "setbn rsz = {:#x}, rbs = {:#x}, rcur = {:#x}",
                setbn.rsz, setbn.rbs, setbn.rcur
            )?;
        }
        if let Some(settr) = self.settr {
            writeln!(fmt, "settr type = {:#x}", settr.ty)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Wait {
    pub trap: bool,
    pub ma_c: bool,
    pub fl_c: bool,
    pub ld_c: bool,
    pub st_c: bool,
    pub all_e: bool,
    pub all_c: bool,
}

impl fmt::Display for Wait {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "wait trap = {}, ma_c = {}, fl_c = {}, ld_c = {}, st_c = {}, all_e = {}, all_c = {}",
            self.trap as u8,
            self.ma_c as u8,
            self.fl_c as u8,
            self.ld_c as u8,
            self.st_c as u8,
            self.all_e as u8,
            self.all_c as u8,
        )
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Control1 {
    Sets(Sets),
    Setei(u8),
    Wait(Wait),
    Setsft,
    Call { wbs: u8 },
    Setmas([u8; 4]),
    Flush { flushr: bool, flushc: bool },
    Vfbg { umask: u8, dmask: u8, chkm4: bool },
}

impl Control1 {
    pub fn new(raw: raw::Cs1, lts0: Option<u32>) -> Result<Self, DecodeError> {
        let ret = if raw.op3() == raw::Cs1::OP3_SETWD {
            let lts = lts0
                .map(|lts| raw::Cs1Lts0(lts))
                .ok_or(DecodeError::LtsNotFound)?;
            let mut sets = Sets::default();
            sets.setwd = Setwd {
                wsz: lts.wsz(),
                nfx: lts.nfx(),
                dbl: lts.dbl(),
            }
            .into();
            if raw.vfrpsz() {
                sets.vfrpsz = Vfrpsz { rpsz: lts.rpsz() }.into();
            }
            if raw.setbp() {
                sets.setbp = Setbp { psz: raw.psz() }.into();
            }
            if raw.setbn() {
                sets.setbn = Setbn {
                    rsz: raw.rsz(),
                    rbs: raw.rbs(),
                    rcur: raw.rcur(),
                }
                .into();
            }
            if raw.settr() {
                sets.settr = Settr { ty: lts.ty() }.into();
            }
            Self::Sets(sets)
        } else if raw.op5() == raw::Cs1::OP5_SETEI {
            Self::Setei(raw.ei())
        } else if raw.op5() == raw::Cs1::OP5_SETSFT {
            Self::Setsft
        } else if raw.op5() == raw::Cs1::OP5_WAIT {
            let wait = Wait {
                all_c: raw.all_c(),
                all_e: raw.all_e(),
                st_c: raw.st_c(),
                ld_c: raw.ld_c(),
                fl_c: raw.fl_c(),
                ma_c: raw.ma_c(),
                trap: raw.trap(),
            };
            Self::Wait(wait)
        } else if raw.op4() == raw::Cs1::OP4_SETBP_SETBN {
            let mut sets = Sets::default();
            if raw.setbp() {
                sets.setbp = Setbp { psz: raw.psz() }.into();
            }
            if raw.setbn() {
                sets.setbn = Setbn {
                    rsz: raw.rsz(),
                    rbs: raw.rbs(),
                    rcur: raw.rcur(),
                }
                .into();
            }
            Self::Sets(sets)
        } else if raw.op4() == raw::Cs1::OP4_CALL {
            Self::Call { wbs: raw.wbs() }
        } else if raw.op4() == raw::Cs1::OP4_SETMAS {
            Self::Setmas([raw.mas0(), raw.mas2(), raw.mas3(), raw.mas5()])
        } else if raw.op4() == raw::Cs1::OP4_FLUSH {
            Self::Flush {
                flushr: raw.flushr(),
                flushc: raw.flushc(),
            }
        } else if raw.op4() == raw::Cs1::OP4_VFBG {
            Self::Vfbg {
                umask: raw.umask(),
                dmask: raw.dmask(),
                chkm4: raw.chkm4(),
            }
        } else {
            return Err(DecodeError::Unknown(raw.0));
        };
        Ok(ret)
    }

    pub fn into_raw(self) -> (raw::Cs1, Option<raw::Cs1Lts0>) {
        let mut raw = raw::Cs1::default();
        match self {
            Self::Sets(sets) => {
                if let Some(setbp) = sets.setbp {
                    raw.set_setbp(true);
                    raw.set_psz(setbp.psz);
                }
                if let Some(setbn) = sets.setbn {
                    raw.set_setbn(true);
                    raw.set_rsz(setbn.rsz);
                    raw.set_rbs(setbn.rbs);
                    raw.set_rcur(setbn.rcur);
                }
                if let Some(setwd) = sets.setwd {
                    let mut lts = raw::Cs1Lts0::default();
                    lts.set_wsz(setwd.wsz);
                    lts.set_nfx(setwd.nfx);
                    lts.set_dbl(setwd.dbl);
                    if let Some(vfrpsz) = sets.vfrpsz {
                        raw.set_vfrpsz(true);
                        lts.set_rpsz(vfrpsz.rpsz);
                    }
                    if let Some(settr) = sets.settr {
                        raw.set_settr(true);
                        lts.set_ty(settr.ty);
                    }
                    return (raw, Some(lts));
                } else {
                    raw.set_op4(raw::Cs1::OP4_SETBP_SETBN);
                }
            }
            Self::Setei(ei) => {
                raw.set_ei(ei);
                raw.set_op5(raw::Cs1::OP5_SETEI);
            }
            Self::Setsft => {
                raw.set_op5(raw::Cs1::OP5_SETSFT);
            }
            Self::Wait(wait) => {
                raw.set_all_c(wait.all_c);
                raw.set_all_e(wait.all_e);
                raw.set_st_c(wait.st_c);
                raw.set_ld_c(wait.ld_c);
                raw.set_fl_c(wait.fl_c);
                raw.set_ma_c(wait.ma_c);
                raw.set_trap(wait.trap);
                raw.set_op5(raw::Cs1::OP5_WAIT);
            }
            Self::Call { wbs } => {
                raw.set_wbs(wbs);
                raw.set_op4(raw::Cs1::OP4_CALL);
            }
            Self::Setmas(mas) => {
                raw.set_mas0(mas[0]);
                raw.set_mas2(mas[1]);
                raw.set_mas3(mas[2]);
                raw.set_mas5(mas[3]);
                raw.set_op4(raw::Cs1::OP4_SETMAS);
            }
            Self::Flush { flushr, flushc } => {
                raw.set_flushr(flushr);
                raw.set_flushc(flushc);
                raw.set_op4(raw::Cs1::OP4_FLUSH);
            }
            Self::Vfbg {
                umask,
                dmask,
                chkm4,
            } => {
                raw.set_umask(umask);
                raw.set_dmask(dmask);
                raw.set_chkm4(chkm4);
                raw.set_op4(raw::Cs1::OP4_VFBG);
            }
        }
        (raw, None)
    }
    pub fn display<'a>(&'a self, ct: Option<&'a Ct>) -> impl fmt::Display + 'a {
        struct Display<'a> {
            control1: &'a Control1,
            ct: Option<&'a Ct>,
        }

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.control1 {
                    Control1::Setmas([c0, c2, c3, c5]) => {
                        // TODO: print mas in alc instr
                        writeln!(f, "setmas {:x}, {:x}, {:x}, {:x}", c0, c2, c3, c5)
                    }
                    Control1::Sets(sets) => fmt::Display::fmt(&sets, f),
                    Control1::Setei(v) => writeln!(f, "setei {:#x}", v),
                    Control1::Wait(wait) => writeln!(f, "{}", wait),
                    Control1::Setsft => writeln!(f, "setsft"),
                    Control1::Call { wbs } => {
                        if let Some(ct) = self.ct {
                            // TODO: add cond to call
                            write!(f, "call {}, wbs = {:#x}{}", ct.ctpr, wbs, ct.cond)?
                        }
                        Ok(())
                    }
                    Control1::Flush { flushr, flushc } => {
                        if *flushc {
                            writeln!(f, "flushc")?;
                        }
                        if *flushr {
                            writeln!(f, "flushr")?;
                        }
                        Ok(())
                    }
                    Control1::Vfbg {
                        umask,
                        dmask,
                        chkm4,
                    } => writeln!(
                        f,
                        "vfbg umask = {}, dmask = {}, chkm4 = {}",
                        *umask, *dmask, *chkm4 as u8
                    ),
                }
            }
        }

        Display { control1: self, ct }
    }
}
