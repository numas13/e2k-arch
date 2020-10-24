pub mod state;

use self::state::{Pred, Preg, Rndpred};
use crate::raw::syllable::{Clp, Elp};
use crate::raw::Unpacked;
use core::fmt::{self, Write};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LoadSrc {
    Pred(Pred),
    Bgrpred,
    Rndpred(Rndpred),
}

impl LoadSrc {
    pub const PRED_MASK: u8 = 0x7f;
    pub const BGRPRED: u8 = 0xc0;
    pub const RNDPRED_FLAG: u8 = 0xc0;
    pub const RNDPRED_FLAG_MASK: u8 = 0xe0;
    pub fn from_raw(value: u8) -> Self {
        if value.leading_zeros() >= 1 {
            LoadSrc::Pred(Pred::from_raw(value & Self::PRED_MASK))
        } else if value == Self::BGRPRED {
            LoadSrc::Bgrpred
        } else if value & Self::RNDPRED_FLAG_MASK == Self::RNDPRED_FLAG {
            LoadSrc::Rndpred(Rndpred::new_truncate(value))
        } else {
            todo!()
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::Pred(p) => p.into_raw(),
            Self::Bgrpred => Self::BGRPRED,
            Self::Rndpred(p) => p.get() | Self::RNDPRED_FLAG,
        }
    }
}

impl Default for LoadSrc {
    fn default() -> Self {
        LoadSrc::Pred(Pred::Lcntex)
    }
}

impl fmt::Display for LoadSrc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Pred(p) => fmt::Display::fmt(p, fmt),
            Self::Bgrpred => fmt.write_str("%bgrpred"),
            Self::Rndpred(p) => fmt::Display::fmt(p, fmt),
        }
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Lp(u8) {
        const RANGE = 0..=6;
        const FMT = "@p{}";
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct LpSrc {
    invert: bool,
    lp: Lp,
}

impl LpSrc {
    pub const INVERT_BIT: u8 = 0x8;
    pub const LP_MASK: u8 = 0x7;
    pub fn from_raw(value: u8) -> Self {
        Self {
            invert: value & Self::INVERT_BIT != 0,
            lp: Lp::new_clamp(value & Self::LP_MASK),
        }
    }
    pub fn into_raw(self) -> u8 {
        if self.invert {
            self.lp.get() | Self::INVERT_BIT
        } else {
            self.lp.get()
        }
    }
}

impl fmt::Display for LpSrc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.invert {
            fmt.write_char('~')?;
        }
        fmt::Display::fmt(&self.lp, fmt)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Opcode {
    Andp,
    Landp,
    Movep,
    Unknown(u8),
}

impl Opcode {
    pub fn from_raw(value: u8) -> Self {
        match value {
            Clp::OP_ANDP => Self::Andp,
            Clp::OP_LANDP => Self::Landp,
            Clp::OP_MOVEP => Self::Movep,
            _ => Self::Unknown(value),
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::Andp => Clp::OP_ANDP,
            Self::Landp => Clp::OP_LANDP,
            Self::Movep => Clp::OP_MOVEP,
            Self::Unknown(v) => v,
        }
    }
}

impl Default for Opcode {
    fn default() -> Self {
        Self::Andp
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Andp => "andp",
            Self::Landp => "landp",
            Self::Movep => "movep",
            Self::Unknown(v) => return write!(fmt, "clp(unknown {})", v),
        };
        fmt.write_str(s)
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Op {
    pub opcode: Opcode,
    pub src0: LpSrc,
    pub src1: LpSrc,
    pub dst: Option<Preg>,
}

impl Op {
    pub fn from_raw(clp: Clp) -> Self {
        Self {
            opcode: Opcode::from_raw(clp.op()),
            src0: LpSrc::from_raw(clp.lpsrc0()),
            src1: LpSrc::from_raw(clp.lpsrc1()),
            dst: if clp.write() {
                Some(Preg::new_truncate(clp.pred()))
            } else {
                None
            },
        }
    }
    pub fn into_raw(self) -> Clp {
        let mut clp = Clp::default();
        clp.set_op(self.opcode.into_raw());
        clp.set_lpsrc0(self.src0.into_raw());
        clp.set_lpsrc1(self.src1.into_raw());
        if let Some(dst) = self.dst {
            clp.set_write(true);
            clp.set_pred(dst.get());
        }
        clp
    }
    pub fn has_lp_src(&self, lp: Lp) -> bool {
        self.src0.lp == lp || self.src1.lp == lp
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Plu {
    len: usize,
    loads: [LoadSrc; 4],
    ops: [Op; 3],
}

impl Plu {
    pub fn from_raw(bundle: &Unpacked) -> Self {
        let len = bundle.hs.pls_len() as usize;
        let mut loads = [LoadSrc::default(); 4];
        let mut ops = [Op::default(); 3];
        for i in 0..len {
            let pls = bundle.pls[i];
            if i < 2 {
                loads[i * 2] = LoadSrc::from_raw(pls.elp0().0);
                loads[i * 2 + 1] = LoadSrc::from_raw(pls.elp1().0);
            }
            ops[i] = Op::from_raw(pls.clp());
        }
        Plu { len, loads, ops }
    }
    pub fn pack_into(&self, bundle: &mut Unpacked) {
        bundle.hs.set_pls_len(self.len as u8);
        for (i, op) in self.ops.iter().enumerate().take(self.len) {
            if i < 2 {
                bundle.pls[i].set_elp0(Elp(self.loads[i * 2].into_raw()));
                bundle.pls[i].set_elp1(Elp(self.loads[i * 2 + 1].into_raw()));
            }
            bundle.pls[i].set_clp(op.into_raw());
        }
    }
}

impl fmt::Display for Plu {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for i in (0..self.len).rev() {
            if i < 2 {
                for (i, src) in self.loads.iter().enumerate().skip(i * 2).take(2) {
                    let lp = Lp::new_clamp(i as u8);
                    if self.ops.iter().any(|i| i.has_lp_src(lp)) {
                        writeln!(fmt, "pass {}, {}", src, lp)?;
                    }
                }
            }
            let lp_dst = Lp::new_clamp(i as u8 + 4);
            let op = &self.ops[i];
            // TODO: CLP cannot use result of MLP
            let is_used = self.ops.iter().skip(i).any(|i| i.has_lp_src(lp_dst));
            if op.dst.is_some() || is_used {
                writeln!(fmt, "{} {}, {}, {}", op.opcode, op.src0, op.src1, lp_dst)?;
            } else {
                writeln!(fmt)?;
            }
            if let Some(dst) = op.dst {
                writeln!(fmt, "pass {}, {}", lp_dst, dst)?;
            }
        }
        Ok(())
    }
}
