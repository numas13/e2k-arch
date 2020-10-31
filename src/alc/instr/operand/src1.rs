pub use crate::raw::operand::Imm5;

use crate::alc::instr::RawInstr;
use crate::raw::operand::Operand;
use crate::state::reg::{Reg, Size};
use crate::InsertInto;
use core::convert::TryFrom;
use core::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Src1 {
    Reg(Reg),
    Imm(Imm5),
}

impl Src1 {
    pub fn display<'a>(&'a self, size: Size) -> impl fmt::Display + 'a {
        struct Display<'a> {
            src1: &'a Src1,
            size: Size,
        }

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.src1 {
                    Src1::Reg(reg) => fmt::Display::fmt(&reg.display(self.size), f),
                    Src1::Imm(i) => write!(f, "{:#x}", i.get()),
                }
            }
        }

        Display { src1: self, size }
    }
}

impl From<Operand> for Src1 {
    fn from(raw: Operand) -> Self {
        if let Some(i) = raw.src1_imm5() {
            Self::Imm(i)
        } else {
            Reg::try_from(raw).map(Self::Reg).unwrap()
        }
    }
}

impl Into<Operand> for Src1 {
    fn into(self) -> Operand {
        let mut raw = Operand::default();
        match self {
            Self::Imm(i) => raw.set_src1_imm5(i),
            Self::Reg(r) => raw = r.into(),
        }
        raw
    }
}

impl InsertInto<RawInstr> for Src1 {
    fn insert_into(self, raw: &mut RawInstr) {
        raw.als.set_src1(self.into());
    }
}
