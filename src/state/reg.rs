use crate::raw::operand::{Based, Global, Operand, Regular};
use core::convert::TryFrom;
use core::fmt::{self, Write};
use thiserror::Error;

#[derive(Debug, Error)]
#[error("Invalid register {0:#x}")]
pub struct DecodeError(u8);

pub(crate) const OP_SIZE_U: u8 = 0;
pub(crate) const OP_SIZE_D: u8 = 1;
pub(crate) const OP_SIZE_X: u8 = 2;
pub(crate) const OP_SIZE_Q: u8 = 3;

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq)]
pub enum Size {
    // undefined
    U,
    B,
    H,
    W,
    D,
    X,
    Q,
}

impl Size {
    pub(crate) fn new_dxq(val: u8) -> Size {
        match val & 0x3 {
            OP_SIZE_D => Self::D,
            OP_SIZE_X => Self::X,
            OP_SIZE_Q => Self::Q,
            _ => Self::U,
        }
    }
}

impl fmt::Display for Size {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::D => fmt.write_char('d'),
            Self::X => fmt.write_char('x'),
            Self::Q => fmt.write_char('q'),
            _ => Ok(()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Reg {
    Based(Based),
    Regular(Regular),
    Global(Global),
}

impl Reg {
    pub fn display<'a>(&'a self, size: Size) -> impl fmt::Display + 'a {
        Display { reg: self, size }
    }
}

impl TryFrom<Operand> for Reg {
    type Error = DecodeError;
    fn try_from(raw: Operand) -> Result<Self, Self::Error> {
        if let Some(r) = raw.based() {
            Ok(Self::Based(r))
        } else if let Some(r) = raw.regular() {
            Ok(Self::Regular(r))
        } else if let Some(r) = raw.global() {
            Ok(Self::Global(r))
        } else {
            Err(DecodeError(raw.0))
        }
    }
}

impl Into<Operand> for Reg {
    fn into(self) -> Operand {
        let mut raw = Operand::default();
        match self {
            Self::Based(r) => raw.set_based(r),
            Self::Regular(r) => raw.set_regular(r),
            Self::Global(r) => raw.set_global(r),
        }
        raw
    }
}

struct Display<'a> {
    reg: &'a Reg,
    size: Size,
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.reg {
            Reg::Based(r) => write!(f, "%{}b[{}]", self.size, r.get()),
            Reg::Regular(r) => write!(f, "%{}r{}", self.size, r.get()),
            Reg::Global(r) => write!(f, "%{}g{}", self.size, r.get()),
        }
    }
}
