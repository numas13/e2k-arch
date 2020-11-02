use super::RawInstr;
use crate::raw::operand::{Based, Global, Imm4, Operand, Regular};
use crate::state::lit::{self, LitLoc, LitValue};
use crate::state::reg::{Reg, Size};
use crate::InsertInto;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("Failed to decode src2")]
pub struct DecodeError {
    #[from]
    source: lit::DecodeError,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Src2 {
    Reg(Reg),
    Imm(Imm4),
    Lit(LitValue),
}

impl Src2 {
    pub fn display<'a>(&'a self, size: Size) -> impl fmt::Display + 'a {
        struct Display<'a> {
            src2: &'a Src2,
            size: Size,
        }

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.src2 {
                    Src2::Reg(reg) => fmt::Display::fmt(&reg.display(self.size), f),
                    Src2::Imm(i) => write!(f, "{:#x}", i.get()),
                    Src2::Lit(lit) => fmt::Display::fmt(lit, f),
                }
            }
        }

        Display { src2: self, size }
    }
    pub fn new(raw: Operand, lts: &[Option<u32>; 4]) -> Result<Self, DecodeError> {
        if let Some(imm) = raw.src2_imm4() {
            Ok(Self::Imm(imm))
        } else if let Some(lit) = raw.src2_lit() {
            let loc = LitLoc::try_from(lit)?;
            let val = LitValue::from_slice(loc, lts)?;
            Ok(Self::Lit(val))
        } else {
            Ok(Reg::try_from(raw).map(Self::Reg).unwrap())
        }
    }
}

impl Into<Operand> for Src2 {
    fn into(self) -> Operand {
        let mut raw = Operand::default();
        match self {
            Self::Reg(reg) => raw = reg.into(),
            Self::Imm(imm) => raw.set_src2_imm4(imm),
            Self::Lit(lit) => raw.set_src2_lit(lit.into_raw()),
        }
        raw
    }
}

impl InsertInto<RawInstr> for Src2 {
    fn insert_into(self, raw: &mut RawInstr) {
        raw.als.set_src2(self.into());
        if let Self::Lit(val) = self {
            raw.lit = Some(val);
        }
    }
}

impl From<Imm4> for Src2 {
    fn from(value: Imm4) -> Self {
        Self::Imm(value)
    }
}

impl From<Reg> for Src2 {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<Based> for Src2 {
    fn from(value: Based) -> Self {
        Reg::Based(value).into()
    }
}

impl From<Regular> for Src2 {
    fn from(value: Regular) -> Self {
        Reg::Regular(value).into()
    }
}

impl From<Global> for Src2 {
    fn from(value: Global) -> Self {
        Reg::Global(value).into()
    }
}

impl From<LitValue> for Src2 {
    fn from(value: LitValue) -> Self {
        Self::Lit(value)
    }
}
