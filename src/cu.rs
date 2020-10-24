pub mod state;

use self::state::Ctpr;
use crate::plu::state::Preg;
use crate::raw::syllable::{Cs0, Cs1, Cs1Lts0, Ss};
use crate::raw::Unpacked;
use core::fmt;

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Nop(u8) {
        const MASK = 0x7;
        const FMT = "nop {}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Ipd(u8) {
        const MASK = 0x3;
        const FMT = "ipd {}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Disp(u32) {
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

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Abg {
    pub abgi: bool,
    pub abgd: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Abn {
    pub abnf: bool,
    pub abnt: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Abp {
    pub abpf: bool,
    pub abpt: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Alc {
    pub alcf: bool,
    pub alct: bool,
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
    Unknown(u8, Preg),
}

impl CtOp {
    pub fn from_raw(ss: &Ss) -> Option<Self> {
        let preg = Preg::new_truncate(ss.ct_pred());
        Some(match ss.ct_op() {
            0 => return None,
            Ss::CT_OP_EXPLICIT => Self::Explicit,
            Ss::CT_OP_PREG => Self::Preg(preg),
            Ss::CT_OP_NOT_PREG => Self::NotPreg(preg),
            Ss::CT_OP_LOOP_END => Self::LoopEnd,
            Ss::CT_OP_NOT_LOOP_END => Self::NotLoopEnd,
            Ss::CT_OP_PREG_OR_LOOP_END => Self::PregOrLoopEnd(preg),
            Ss::CT_OP_NOT_PREG_AND_NOT_LOOP_END => Self::NotPregAndNotLoopEnd(preg),
            Ss::CT_OP_MLOCK => Self::Mlock,
            _ => Self::Unknown(ss.ct_op(), preg),
        })
    }
    pub fn pack_into(self, ss: &mut Ss) {
        match self {
            Self::Explicit => ss.set_ct_op(Ss::CT_OP_EXPLICIT),
            Self::Preg(p) => {
                ss.set_ct_op(Ss::CT_OP_PREG);
                ss.set_ct_pred(p.get());
            }
            Self::NotPreg(p) => {
                ss.set_ct_op(Ss::CT_OP_NOT_PREG);
                ss.set_ct_pred(p.get());
            }
            Self::LoopEnd => ss.set_ct_op(Ss::CT_OP_LOOP_END),
            Self::NotLoopEnd => ss.set_ct_op(Ss::CT_OP_NOT_LOOP_END),
            Self::PregOrLoopEnd(p) => {
                ss.set_ct_op(Ss::CT_OP_PREG_OR_LOOP_END);
                ss.set_ct_pred(p.get());
            }
            Self::NotPregAndNotLoopEnd(p) => {
                ss.set_ct_op(Ss::CT_OP_NOT_PREG_AND_NOT_LOOP_END);
                ss.set_ct_pred(p.get());
            }
            Self::Mlock => ss.set_ct_op(Ss::CT_OP_MLOCK),
            Self::Unknown(op, p) => {
                ss.set_ct_op(op);
                ss.set_ct_pred(p.get());
            }
        }
    }
}

impl fmt::Display for CtOp {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Explicit => Ok(()),
            Self::Preg(preg) => write!(fmt, " ? {}", preg),
            Self::NotPreg(preg) => write!(fmt, " ? ~ {}", preg),
            Self::LoopEnd => fmt.write_str(" ? #LOOP_END"),
            Self::NotLoopEnd => fmt.write_str(" ? #NOT_LOOP_END"),
            Self::PregOrLoopEnd(preg) => write!(fmt, " ? {} || #LOOP_END", preg),
            Self::NotPregAndNotLoopEnd(preg) => write!(fmt, " ? ~ {} && #NOT_LOOP_END", preg),
            Self::Mlock => fmt.write_str(" ? %MLOCK"),
            Self::Unknown(op, p) => write!(fmt, " ? unknown {:02x} {}", op, p),
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
    pub fn from_raw(ss: &Ss) -> Option<Ct> {
        if let Some(op) = CtOp::from_raw(ss) {
            let ctpr = Ctpr::new(ss.ct_ctpr());
            Some(Ct { op, ctpr })
        } else {
            None
        }
    }
    pub fn into_raw(self) -> Ss {
        let mut ss = Ss::default();
        ss.set_ct_ctpr(self.ctpr.map_or(0, |ctpr| ctpr.get()));
        self.op.pack_into(&mut ss);
        ss
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
    Unknown(u32),
}

impl Control0 {
    pub fn from_raw(cs0: &Cs0) -> Self {
        if let Some(ctpr) = Ctpr::new(cs0.ctpr()) {
            let disp = Disp::new_truncate(cs0.disp());
            match cs0.op() {
                0 => Self::Disp(ctpr, disp),
                // TODO: ctpr must be %ctpr2
                1 => Self::LDisp(ctpr, disp),
                2 => Self::SDisp(ctpr, disp),
                3 => {
                    if disp.get() & 1 == 0 {
                        // TODO: ctpr must be %ctpr3
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
        }
    }
    pub fn into_raw(&self) -> Cs0 {
        let mut cs0 = Cs0::default();
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
                cs0.set_pref_ipd(*ipd);
            }
            Self::Puttsd(disp) => {
                cs0.set_op(2);
                cs0.set_disp(disp.get());
            }
            Self::Done { trar, fdam } => {
                cs0.set_op(3);
                cs0.set_done_trar(*trar);
                cs0.set_done_fdam(*fdam);
            }
            Self::Unknown(raw) => cs0.0 = *raw,
        }
        cs0
    }
    pub fn print(&self, fmt: &mut fmt::Formatter, ct: Option<&Ct>) -> fmt::Result {
        match self {
            Self::Disp(ctpr, disp) => writeln!(fmt, "disp {}, {}", ctpr, disp),
            Self::LDisp(ctpr, disp) => writeln!(fmt, "ldisp {}, {}", ctpr, disp),
            Self::SDisp(ctpr, disp) => writeln!(fmt, "sdisp {}, {}", ctpr, disp),
            Self::Return(ctpr) => writeln!(fmt, "return {}", ctpr),
            Self::Gettsd(ctpr) => writeln!(fmt, "gettsd {}", ctpr),
            Self::IBranch(disp) => {
                write!(fmt, "ibranch {}", disp)?;
                if let Some(ct) = ct {
                    write!(fmt, "{}", ct.op)?;
                }
                writeln!(fmt)
            }
            Self::Pref(ipr, disp, ipd) => {
                writeln!(fmt, "pref {}, disp = {}, ipd = {}", ipr, disp, *ipd as u8)
            }
            Self::Puttsd(disp) => writeln!(fmt, "puttsd {}", disp),
            Self::Done { trar, fdam } => {
                if let Some(ct) = ct {
                    write!(
                        fmt,
                        "done trar = {}, fdam = {}{}",
                        *trar as u8, *fdam as u8, ct.op
                    )
                } else {
                    unreachable!()
                }
            }
            Self::Unknown(value) => writeln!(fmt, "unknown CS0 {:#08x}", value),
        }
    }
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
    Unknown(u32),
}

impl Control1 {
    pub fn from_raw(bundle: &Unpacked) -> Self {
        let cs1 = &bundle.cs1;
        if cs1.op3() == Cs1::OP3_SETWD {
            let lts = Cs1Lts0(bundle.lts[0].0);
            let mut sets = Sets::default();
            sets.setwd = Setwd {
                wsz: lts.wsz(),
                nfx: lts.nfx(),
                dbl: lts.dbl(),
            }
            .into();
            if cs1.vfrpsz() {
                sets.vfrpsz = Vfrpsz { rpsz: lts.rpsz() }.into();
            }
            if cs1.setbp() {
                sets.setbp = Setbp { psz: cs1.psz() }.into();
            }
            if cs1.setbn() {
                sets.setbn = Setbn {
                    rsz: cs1.rsz(),
                    rbs: cs1.rbs(),
                    rcur: cs1.rcur(),
                }
                .into();
            }
            if cs1.settr() {
                sets.settr = Settr { ty: lts.ty() }.into();
            }
            Self::Sets(sets)
        } else if cs1.op5() == Cs1::OP5_SETEI {
            Self::Setei(cs1.ei())
        } else if cs1.op5() == Cs1::OP5_SETSFT {
            Self::Setsft
        } else if cs1.op5() == Cs1::OP5_WAIT {
            let wait = Wait {
                all_c: cs1.all_c(),
                all_e: cs1.all_e(),
                st_c: cs1.st_c(),
                ld_c: cs1.ld_c(),
                fl_c: cs1.fl_c(),
                ma_c: cs1.ma_c(),
                trap: cs1.trap(),
            };
            Self::Wait(wait)
        } else if cs1.op4() == Cs1::OP4_SETBP_SETBN {
            let mut sets = Sets::default();
            if cs1.setbp() {
                sets.setbp = Setbp { psz: cs1.psz() }.into();
            }
            if cs1.setbn() {
                sets.setbn = Setbn {
                    rsz: cs1.rsz(),
                    rbs: cs1.rbs(),
                    rcur: cs1.rcur(),
                }
                .into();
            }
            Self::Sets(sets)
        } else if cs1.op4() == Cs1::OP4_CALL {
            Self::Call { wbs: cs1.wbs() }
        } else if cs1.op4() == Cs1::OP4_SETMAS {
            Self::Setmas([cs1.mas0(), cs1.mas2(), cs1.mas3(), cs1.mas5()])
        } else if cs1.op4() == Cs1::OP4_FLUSH {
            Self::Flush {
                flushr: cs1.flushr(),
                flushc: cs1.flushc(),
            }
        } else if cs1.op4() == Cs1::OP4_VFBG {
            Self::Vfbg {
                umask: cs1.umask(),
                dmask: cs1.dmask(),
                chkm4: cs1.chkm4(),
            }
        } else {
            Self::Unknown(cs1.0)
        }
    }
    pub fn into_raw(self) -> (Cs1, Option<Cs1Lts0>) {
        let mut cs1 = Cs1::default();
        match self {
            Self::Sets(sets) => {
                if let Some(setbp) = sets.setbp {
                    cs1.set_setbp(true);
                    cs1.set_psz(setbp.psz);
                }
                if let Some(setbn) = sets.setbn {
                    cs1.set_setbn(true);
                    cs1.set_rsz(setbn.rsz);
                    cs1.set_rbs(setbn.rbs);
                    cs1.set_rcur(setbn.rcur);
                }
                if let Some(setwd) = sets.setwd {
                    let mut lts = Cs1Lts0::default();
                    lts.set_wsz(setwd.wsz);
                    lts.set_nfx(setwd.nfx);
                    lts.set_dbl(setwd.dbl);
                    if let Some(vfrpsz) = sets.vfrpsz {
                        cs1.set_vfrpsz(true);
                        lts.set_rpsz(vfrpsz.rpsz);
                    }
                    if let Some(settr) = sets.settr {
                        cs1.set_settr(true);
                        lts.set_ty(settr.ty);
                    }
                    return (cs1, Some(lts));
                } else {
                    cs1.set_op4(Cs1::OP4_SETBP_SETBN);
                }
            }
            Self::Setei(ei) => {
                cs1.set_ei(ei);
                cs1.set_op5(Cs1::OP5_SETEI);
            }
            Self::Setsft => {
                cs1.set_op5(Cs1::OP5_SETSFT);
            }
            Self::Wait(wait) => {
                cs1.set_all_c(wait.all_c);
                cs1.set_all_e(wait.all_e);
                cs1.set_st_c(wait.st_c);
                cs1.set_ld_c(wait.ld_c);
                cs1.set_fl_c(wait.fl_c);
                cs1.set_ma_c(wait.ma_c);
                cs1.set_trap(wait.trap);
                cs1.set_op5(Cs1::OP5_WAIT);
            }
            Self::Call { wbs } => {
                cs1.set_wbs(wbs);
                cs1.set_op4(Cs1::OP4_CALL);
            }
            Self::Setmas(mas) => {
                cs1.set_mas0(mas[0]);
                cs1.set_mas2(mas[1]);
                cs1.set_mas3(mas[2]);
                cs1.set_mas5(mas[3]);
                cs1.set_op4(Cs1::OP4_SETMAS);
            }
            Self::Flush { flushr, flushc } => {
                cs1.set_flushr(flushr);
                cs1.set_flushc(flushc);
                cs1.set_op4(Cs1::OP4_FLUSH);
            }
            Self::Vfbg {
                umask,
                dmask,
                chkm4,
            } => {
                cs1.set_umask(umask);
                cs1.set_dmask(dmask);
                cs1.set_chkm4(chkm4);
                cs1.set_op4(Cs1::OP4_VFBG);
            }
            Self::Unknown(raw) => cs1.0 = raw,
        }
        (cs1, None)
    }
    pub fn print(&self, fmt: &mut fmt::Formatter, ct: Option<&Ct>) -> fmt::Result {
        match self {
            Self::Setmas(_) => Ok(()),
            Self::Sets(sets) => fmt::Display::fmt(&sets, fmt),
            Self::Setei(v) => writeln!(fmt, "setei {:#x}", v),
            Self::Wait(wait) => writeln!(fmt, "{}", wait),
            Self::Setsft => writeln!(fmt, "setsft"),
            Self::Call { wbs } => {
                if let Some(ct) = ct {
                    match ct.ctpr {
                        Some(ctpr) => write!(fmt, "call {}, wbs = {:#x}{}", ctpr, wbs, ct.op),
                        None => write!(fmt, "call BAD_CTPR, wbs = {:#x}{}", wbs, ct.op),
                    }
                } else {
                    todo!()
                }
            }
            Self::Flush { flushr, flushc } => {
                if *flushc {
                    writeln!(fmt, "flushc")?;
                }
                if *flushr {
                    writeln!(fmt, "flushr")?;
                }
                Ok(())
            }
            Self::Vfbg {
                umask,
                dmask,
                chkm4,
            } => writeln!(
                fmt,
                "vfbg umask = {}, dmask = {}, chkm4 = {}",
                *umask, *dmask, *chkm4 as u8
            ),
            Self::Unknown(v) => writeln!(fmt, "unknown CS1 {:#08x}", v),
        }
    }
}

// TODO: migrate to bitlfags?
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Short {
    pub eap: bool,
    pub bap: bool,
    pub srp: bool,
    pub vfdi: bool,
    pub abg: Abg,
    pub abn: Abn,
    pub abp: Abp,
    pub alc: Alc,
}

impl Short {
    pub fn from_raw(ss: &Ss) -> Self {
        Short {
            eap: ss.eap(),
            bap: ss.bap(),
            srp: ss.srp(),
            vfdi: ss.vfdi(),
            abg: Abg {
                abgi: ss.abgi(),
                abgd: ss.abgd(),
            },
            abn: Abn {
                abnf: ss.abnf(),
                abnt: ss.abnt(),
            },
            abp: Abp {
                abpf: ss.abpf(),
                abpt: ss.abpt(),
            },
            alc: Alc {
                alcf: ss.alcf(),
                alct: ss.alct(),
            },
        }
    }
    pub fn into_raw(&self) -> Ss {
        let mut ss = Ss::default();
        ss.set_eap(self.eap);
        ss.set_bap(self.bap);
        ss.set_srp(self.srp);
        ss.set_vfdi(self.vfdi);
        ss.set_abgi(self.abg.abgi);
        ss.set_abgd(self.abg.abgd);
        ss.set_abnf(self.abn.abnf);
        ss.set_abnt(self.abn.abnt);
        ss.set_abpf(self.abp.abpf);
        ss.set_abpt(self.abp.abpt);
        ss.set_alcf(self.alc.alcf);
        ss.set_alct(self.alc.alct);
        ss
    }
}

impl fmt::Display for Short {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.eap {
            writeln!(fmt, "eap")?;
        }
        if self.bap {
            writeln!(fmt, "bap")?;
        }
        if self.srp {
            writeln!(fmt, "srp")?;
        }
        if self.vfdi {
            writeln!(fmt, "vfdi")?;
        }
        let mut print_flag_pair = |s, v0, s0, v1, s1| {
            if v0 || v1 {
                writeln!(fmt, "{} {0}{}={}, {0}{}={}", s, s0, v0 as u8, s1, v1 as u8)?;
            }
            Ok(())
        };
        print_flag_pair("abg", self.abg.abgi, "i", self.abg.abgd, "d")?;
        print_flag_pair("abn", self.abn.abnf, "f", self.abn.abnt, "t")?;
        print_flag_pair("abp", self.abp.abpf, "f", self.abp.abpt, "t")?;
        print_flag_pair("alc", self.alc.alcf, "f", self.alc.alct, "t")?;
        Ok(())
    }
}
