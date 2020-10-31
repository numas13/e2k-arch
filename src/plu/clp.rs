use crate::raw;
use crate::state::pred::Preg;
use core::convert::TryFrom;
use core::fmt;
use num_enum::{TryFromPrimitive, TryFromPrimitiveError};
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Undefined intermediate predicate @p7")]
    UndefinedIntermPred,
    #[error("Invalid CLP kind {}", source.number)]
    InvalidClpKind {
        #[from]
        source: TryFromPrimitiveError<Kind>,
    },
}

newtype! {
    /// An intermediate predicate for logical operations.
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct IntermPred(u8) {
        const RANGE = 0..=6;
        const FMT = "@p{}";
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Src {
    IfTrue(IntermPred),
    IfFalse(IntermPred),
}

impl Src {
    pub fn pred(&self) -> &IntermPred {
        match self {
            Self::IfTrue(pred) => pred,
            Self::IfFalse(pred) => pred,
        }
    }
}

impl TryFrom<raw::IpSrc> for Src {
    type Error = DecodeError;
    fn try_from(raw: raw::IpSrc) -> Result<Self, Self::Error> {
        let pred = IntermPred::new(raw.ip()).ok_or(DecodeError::UndefinedIntermPred)?;
        if raw.inv() {
            Ok(Self::IfFalse(pred))
        } else {
            Ok(Self::IfTrue(pred))
        }
    }
}

impl Into<raw::IpSrc> for Src {
    fn into(self) -> raw::IpSrc {
        let (inv, pred) = match self {
            Self::IfTrue(pred) => (false, pred),
            Self::IfFalse(pred) => (true, pred),
        };
        let mut raw = raw::IpSrc::default();
        raw.set_inv(inv);
        raw.set_ip(pred.get());
        raw
    }
}

impl fmt::Display for Src {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IfTrue(p) => fmt::Display::fmt(p, fmt),
            Self::IfFalse(p) => write!(fmt, "~{}", p),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, TryFromPrimitive)]
#[repr(u8)]
pub enum Kind {
    And = 0,
    Land = 1,
    Move = 3,
}

impl Kind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::And => "andp",
            Self::Land => "landp",
            Self::Move => "movep",
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.as_str())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Clp {
    pub kind: Kind,
    pub src: [Src; 2],
    pub dst: Option<Preg>,
}

impl Clp {
    pub const fn new(kind: Kind, src0: Src, src1: Src, dst: Option<Preg>) -> Self {
        Self {
            kind,
            src: [src0, src1],
            dst,
        }
    }
}

impl TryFrom<raw::Clp> for Clp {
    type Error = DecodeError;
    fn try_from(raw: raw::Clp) -> Result<Self, Self::Error> {
        let kind = Kind::try_from(raw.op())?;
        let src0 = Src::try_from(raw.ip_src0())?;
        let src1 = Src::try_from(raw.ip_src1())?;
        let dst = if raw.write() {
            Some(Preg::new_truncate(raw.preg()))
        } else {
            None
        };
        Ok(Self::new(kind, src0, src1, dst))
    }
}

impl Into<raw::Clp> for Clp {
    fn into(self) -> raw::Clp {
        let mut raw = raw::Clp::default();
        raw.set_op(self.kind as u8);
        raw.set_ip_src0(self.src[0].into());
        raw.set_ip_src1(self.src[1].into());
        if let Some(dst) = self.dst {
            raw.set_write(true);
            raw.set_preg(dst.get());
        }
        raw
    }
}
