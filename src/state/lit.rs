use crate::raw::operand::Lit;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DecodeError {
    #[error("Invalid src2 64-bit literal location {0}")]
    InvalidLoc(u8),
    #[error("Literal value {0} not found")]
    ValueNotFound(LitLoc),
}

newtype! {
    /// A 16-bit literal value location in a bundle.
    ///
    /// The location must be within range of 0 and 1 inclusive.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LitLoc16(u8) {
        const MASK = 1;
    }
}

newtype! {
    /// A 32-bit literal value location in a bundle.
    ///
    /// The location must be within range of 0 and 3 inclusive.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LitLoc32(u8) {
        const MASK = 3;
    }
}

newtype! {
    /// A 64-bit literal value location in a bundle.
    ///
    /// The location must be within range of 0 and 2 inclusive.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LitLoc64(u8) {
        const RANGE = 0..=2;
    }
}

/// A half of literal value.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LitPart {
    Lo,
    Hi,
}

impl fmt::Display for LitPart {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Lo => fmt.write_str("lo"),
            Self::Hi => fmt.write_str("hi"),
        }
    }
}

/// A literal value location in a bundle.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LitLoc {
    /// A 16-bit literal value location.
    F16(LitLoc16, LitPart),
    /// A 32-bit literal value location.
    F32(LitLoc32),
    /// A 64-bit literal value location.
    F64(LitLoc64),
}

impl TryFrom<Lit> for LitLoc {
    type Error = DecodeError;
    fn try_from(raw: Lit) -> Result<Self, Self::Error> {
        let ret = match raw.ty() {
            0 => Self::F16(LitLoc16::new_truncate(raw.loc()), LitPart::Lo),
            1 => Self::F16(LitLoc16::new_truncate(raw.loc()), LitPart::Hi),
            2 => Self::F32(LitLoc32::new_truncate(raw.loc())),
            _ => Self::F64(LitLoc64::new(raw.loc()).ok_or(DecodeError::InvalidLoc(raw.loc()))?),
        };
        Ok(ret)
    }
}

impl Into<Lit> for LitLoc {
    fn into(self) -> Lit {
        let (ty, loc) = match self {
            Self::F16(loc, LitPart::Lo) => (0, loc.get()),
            Self::F16(loc, LitPart::Hi) => (1, loc.get()),
            Self::F32(loc) => (2, loc.get()),
            Self::F64(loc) => (3, loc.get()),
        };
        let mut raw = Lit::default();
        raw.set_ty(ty);
        raw.set_loc(loc);
        raw
    }
}

impl fmt::Display for LitLoc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::F16(l, p) => write!(fmt, "_f16s,_lts{}{}", l.get(), p),
            Self::F32(l) => write!(fmt, "_f32s,_lts{}", l.get()),
            Self::F64(l) => write!(fmt, "_f64,_lts{}", l.get()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LitValue {
    F16(LitLoc16, LitPart, u16),
    F32(LitLoc32, u32),
    F64(LitLoc64, u64),
}

impl LitValue {
    pub fn new(loc: LitLoc, lts: &[Option<u32>; 4]) -> Result<Self, DecodeError> {
        let value = match loc {
            LitLoc::F16(loc, LitPart::Lo) => {
                lts[loc.get() as usize].map(|i| Self::F16(loc, LitPart::Lo, i as u16))
            }
            LitLoc::F16(loc, LitPart::Hi) => {
                lts[loc.get() as usize].map(|i| Self::F16(loc, LitPart::Hi, (i >> 16) as u16))
            }
            LitLoc::F32(loc) => lts[loc.get() as usize].map(|i| Self::F32(loc, i)),
            LitLoc::F64(loc) => {
                let i = loc.get() as usize;
                match (lts[i], lts[i + 1]) {
                    (Some(lo), Some(hi)) => Some(Self::F64(loc, (hi as u64) << 32 | lo as u64)),
                    _ => None,
                }
            }
        };
        value.ok_or(DecodeError::ValueNotFound(loc))
    }
}

impl Into<Lit> for LitValue {
    fn into(self) -> Lit {
        let (ty, loc) = match self {
            Self::F16(loc, LitPart::Lo, _) => (0, loc.get()),
            Self::F16(loc, LitPart::Hi, _) => (1, loc.get()),
            Self::F32(loc, _) => (2, loc.get()),
            Self::F64(loc, _) => (3, loc.get()),
        };
        let mut raw = Lit::default();
        raw.set_ty(ty);
        raw.set_loc(loc);
        raw
    }
}

impl fmt::Display for LitValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::F16(l, p, v) => write!(f, "_f16s,_lts{}{} {:#x}", l.get(), p, v),
            Self::F32(l, v) => write!(f, "_f32s,_lts{} {:#x}", l.get(), v),
            Self::F64(l, v) => write!(f, "_f64,_lts{} {:#x}", l.get(), v),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_literal_location() {
        assert_eq!(LitLoc16::new(1), Some(LitLoc16(1)));
        assert_eq!(LitLoc32::new(3), Some(LitLoc32(3)));
        assert_eq!(LitLoc64::new(2), Some(LitLoc64(2)));
        assert!(LitLoc16::new(2).is_none());
        assert!(LitLoc32::new(4).is_none());
        assert!(LitLoc64::new(3).is_none());

        assert_eq!(LitLoc16::new_clamp(8).get(), 1);
        assert_eq!(LitLoc32::new_clamp(8).get(), 3);
        assert_eq!(LitLoc64::new_clamp(8).get(), 2);
    }
}
