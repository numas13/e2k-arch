use super::state::*;
use crate::cu::state::Ctpr;
use crate::plu::state::{Pred, Preg};
use core::fmt;

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Imm5(u8) {
        const MASK = 0x1f;
        const FMT = "{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Imm4(u8) {
        const MASK = 0x0f;
        const FMT = "{}";
    }
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

impl LitLoc {
    pub const LOC_MASK: u8 = 0x3;
    pub const LIT16_FLAG_MASK: u8 = 0xa;
    pub const LIT16_FLAG: u8 = 0;
    pub const LIT16_HI_BIT: u8 = 0x4;
    pub const LIT32_FLAG_MASK: u8 = 0xc;
    pub const LIT32_FLAG: u8 = 0x8;
    pub const LIT64_FLAG_MASK: u8 = 0xc;
    pub const LIT64_FLAG: u8 = 0xc;
    /// Tries to create a new `LitLoc` from the given raw value.
    pub fn from_raw(value: u8) -> Option<Self> {
        let loc = value & Self::LOC_MASK;
        if value & Self::LIT64_FLAG_MASK == Self::LIT64_FLAG {
            LitLoc64::new(loc).map(Self::F64)
        } else if value & Self::LIT32_FLAG_MASK == Self::LIT32_FLAG {
            Self::F32(LitLoc32::new_truncate(loc)).into()
        } else if value & Self::LIT16_FLAG_MASK == Self::LIT16_FLAG {
            let part = if value & Self::LIT16_HI_BIT == 0 {
                LitPart::Lo
            } else {
                LitPart::Hi
            };
            Some(Self::F16(LitLoc16::new_truncate(loc), part))
        } else {
            None
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::F16(loc, LitPart::Lo) => loc.get() | Self::LIT16_FLAG,
            Self::F16(loc, LitPart::Hi) => loc.get() | Self::LIT16_FLAG | Self::LIT16_HI_BIT,
            Self::F32(loc) => loc.get() | Self::LIT32_FLAG,
            Self::F64(loc) => loc.get() | Self::LIT64_FLAG,
        }
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
pub enum Src1 {
    Reg(Reg),
    Imm(Imm5),
}

impl Src1 {
    pub const IMM_FLAG: u8 = 0xc0;
    pub const IMM_FLAG_MASK: u8 = 0xe0;
    pub fn from_raw(value: u8) -> Option<Self> {
        if value & Self::IMM_FLAG_MASK == Self::IMM_FLAG {
            Self::Imm(Imm5::new_truncate(value)).into()
        } else {
            Reg::from_raw(value).map(Self::Reg)
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::Reg(reg) => reg.into_raw(),
            Self::Imm(imm) => imm.get() | Self::IMM_FLAG,
        }
    }
}

impl fmt::Display for Src1 {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Reg(reg) => fmt::Display::fmt(reg, fmt),
            Self::Imm(imm) => fmt::Display::fmt(imm, fmt),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Src2 {
    Reg(Reg),
    Imm(Imm4),
    Lit(LitLoc),
}

impl Src2 {
    pub const LIT_FLAG_MASK: u8 = 0xf0;
    pub const LIT_FLAG: u8 = 0xd0;
    pub const LIT_MASK: u8 = 0x0f;
    pub const IMM_FLAG: u8 = 0xc0;
    pub const IMM_FLAG_MASK: u8 = 0xf0;

    pub fn from_raw(value: u8) -> Option<Self> {
        if value & Self::IMM_FLAG_MASK == Self::IMM_FLAG {
            Self::Imm(Imm4::new_truncate(value)).into()
        } else if value & Self::LIT_FLAG_MASK == Self::LIT_FLAG {
            LitLoc::from_raw(value & Self::LIT_MASK).map(Self::Lit)
        } else {
            Reg::from_raw(value).map(Self::Reg)
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::Reg(reg) => reg.into_raw(),
            Self::Imm(imm) => imm.get() | Self::IMM_FLAG,
            Self::Lit(lit) => lit.into_raw() | Self::LIT_FLAG,
        }
    }
}

impl fmt::Display for Src2 {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Reg(reg) => fmt::Display::fmt(reg, fmt),
            Self::Imm(imm) => fmt::Display::fmt(imm, fmt),
            Self::Lit(loc) => fmt::Display::fmt(loc, fmt),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Src3(pub Reg);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Src4(pub Reg);

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Addr(Src1, Src2);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SrcPred {
    True(Pred),
    False(Pred),
}

impl fmt::Display for SrcPred {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::True(p) => fmt::Display::fmt(p, fmt),
            Self::False(p) => write!(fmt, " ~ {}", p),
        }
    }
}

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
    pub const TST: u8 = 0xcd;
    pub const TC: u8 = 0xce;
    pub const TCD: u8 = 0xcf;
    pub const CTPR_FLAG: u8 = 0xd0;
    pub const CTPR_FLAG_MASK: u8 = 0xfc;
    pub const EMPTY: u8 = 0xdf;
    pub fn from_raw(value: u8) -> Option<Self> {
        if value == Self::TST {
            Some(Self::Tst)
        } else if value == Self::TC {
            Some(Self::Tc)
        } else if value == Self::TCD {
            Some(Self::Tcd)
        } else if value & Self::CTPR_FLAG_MASK == Self::CTPR_FLAG {
            Ctpr::new(value & 0x3).map(Self::Ctpr)
        } else if value == Self::EMPTY {
            Some(Self::Empty)
        } else {
            Reg::from_raw(value).map(Self::Reg)
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::Reg(reg) => reg.into_raw(),
            Self::Ctpr(ctpr) => ctpr.get() | Self::CTPR_FLAG,
            Self::Tst => Self::TST,
            Self::Tc => Self::TC,
            Self::Tcd => Self::TCD,
            Self::Empty => Self::EMPTY,
        }
    }
}

impl Default for Dst {
    fn default() -> Self {
        Self::Reg(Reg::Based(Based::new_truncate(0)))
    }
}

impl fmt::Display for Dst {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Reg(reg) => fmt::Display::fmt(reg, fmt),
            Self::Tst => fmt.write_str("%tst"),
            Self::Tc => fmt.write_str("%tc"),
            Self::Tcd => fmt.write_str("%tcd"),
            Self::Ctpr(ctpr) => fmt::Display::fmt(ctpr, fmt),
            Self::Empty => fmt.write_str("%empty"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DstPreg(Preg);

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

    #[test]
    fn src2_literal_from_raw() {
        use LitLoc::*;
        use LitPart::*;
        use Src2::Lit;

        assert_eq!(
            Src2::from_raw(0xd0),
            Some(Lit(F16(LitLoc16::new_clamp(0), Lo)))
        );
        assert_eq!(
            Src2::from_raw(0xd1),
            Some(Lit(F16(LitLoc16::new_clamp(1), Lo)))
        );
        assert_eq!(
            Src2::from_raw(0xd4),
            Some(Lit(F16(LitLoc16::new_clamp(0), Hi)))
        );
        assert_eq!(
            Src2::from_raw(0xd5),
            Some(Lit(F16(LitLoc16::new_clamp(1), Hi)))
        );
        assert_eq!(Src2::from_raw(0xd8), Some(Lit(F32(LitLoc32::new_clamp(0)))));
        assert_eq!(Src2::from_raw(0xdb), Some(Lit(F32(LitLoc32::new_clamp(3)))));
        assert_eq!(Src2::from_raw(0xdc), Some(Lit(F64(LitLoc64::new_clamp(0)))));
        assert_eq!(Src2::from_raw(0xde), Some(Lit(F64(LitLoc64::new_clamp(2)))));
    }
}
