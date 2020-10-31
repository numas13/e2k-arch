use super::RawInstr;
use crate::raw::operand::Operand;
use crate::state::reg::{Reg, Size};
use crate::state::Ctpr;
use crate::InsertInto;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("Invalid dst {0:#x}")]
pub struct DecodeError(u8);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Dst {
    Reg(Reg),
    Tst,
    Tc,
    Tcd,
    Ctpr(Ctpr),
    Empty,
}

impl Dst {
    pub fn display<'a>(&'a self, size: Size) -> impl fmt::Display + 'a {
        struct Display<'a> {
            dst: &'a Dst,
            size: Size,
        }

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.dst {
                    Dst::Reg(reg) => fmt::Display::fmt(&reg.display(self.size), f),
                    Dst::Tst => f.write_str("%tst"),
                    Dst::Tc => f.write_str("%tc"),
                    Dst::Tcd => f.write_str("%tcd"),
                    Dst::Ctpr(ctpr) => fmt::Display::fmt(ctpr, f),
                    Dst::Empty => f.write_str("%empty"),
                }
            }
        }

        Display { dst: self, size }
    }
}

impl TryFrom<Operand> for Dst {
    type Error = DecodeError;
    fn try_from(raw: Operand) -> Result<Self, Self::Error> {
        if raw.dst_tst() {
            Ok(Self::Tst)
        } else if raw.dst_tc() {
            Ok(Self::Tc)
        } else if raw.dst_tcd() {
            Ok(Self::Tcd)
        } else if let Some(ctpr) = raw.dst_ctpr() {
            Ok(Self::Ctpr(ctpr))
        } else if raw.dst_empty() {
            Ok(Self::Empty)
        } else {
            Reg::try_from(raw)
                .map(Self::Reg)
                .map_err(|_| DecodeError(raw.0))
        }
    }
}

impl Into<Operand> for Dst {
    fn into(self) -> Operand {
        let mut raw = Operand::default();
        match self {
            Self::Reg(reg) => raw = reg.into(),
            Self::Ctpr(ctpr) => raw.set_dst_ctpr(ctpr),
            Self::Tst => raw.set_dst_tst(),
            Self::Tc => raw.set_dst_tc(),
            Self::Tcd => raw.set_dst_tcd(),
            Self::Empty => raw.set_dst_empty(),
        }
        raw
    }
}

impl InsertInto<RawInstr> for Dst {
    fn insert_into(self, raw: &mut RawInstr) {
        raw.als.set_dst(self.into());
    }
}
