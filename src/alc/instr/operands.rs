use super::Raw;
use crate::aau;
use crate::alc::state::*;
use crate::cu::state::Ctpr;
use crate::plu::state::{Pred, Preg};
use crate::{Error, InsertInto};
use core::convert::TryFrom;
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

#[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
pub struct Imm8(pub u8);

impl From<u8> for Imm8 {
    fn from(value: u8) -> Self {
        Self(value)
    }
}

impl Into<u8> for Imm8 {
    fn into(self) -> u8 {
        self.0
    }
}

impl TryFrom<&'_ Raw> for Imm8 {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        Ok(Self(raw.ales.src3()))
    }
}

impl InsertInto<Raw> for Imm8 {
    fn insert_into(self, raw: &mut Raw) {
        raw.ales.set_src3(self.0)
    }
}

impl fmt::Display for Imm8 {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:#x}", self.0)
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
    const IMM_FLAG: u8 = 0xc0;
    const IMM_FLAG_MASK: u8 = 0xe0;
}

impl From<u8> for Src1 {
    fn from(value: u8) -> Self {
        if value & Self::IMM_FLAG_MASK == Self::IMM_FLAG {
            Self::Imm(Imm5::new_truncate(value))
        } else {
            Reg::from_raw(value).map(Self::Reg).unwrap()
        }
    }
}

impl Into<u8> for Src1 {
    fn into(self) -> u8 {
        match self {
            Self::Reg(reg) => reg.into_raw(),
            Self::Imm(imm) => imm.get() | Self::IMM_FLAG,
        }
    }
}

impl TryFrom<&'_ Raw> for Src1 {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        Ok(Self::from(raw.als.src1()))
    }
}

impl InsertInto<Raw> for Src1 {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_src1(self.into());
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
    const LIT_FLAG_MASK: u8 = 0xf0;
    const LIT_FLAG: u8 = 0xd0;
    const LIT_MASK: u8 = 0x0f;
    const IMM_FLAG: u8 = 0xc0;
    const IMM_FLAG_MASK: u8 = 0xf0;
    pub const fn lit16lo(loc: LitLoc16) -> Self {
        Self::Lit(LitLoc::F16(loc, LitPart::Lo))
    }
    pub const fn lit16hi(loc: LitLoc16) -> Self {
        Self::Lit(LitLoc::F16(loc, LitPart::Hi))
    }
    pub const fn lit32(loc: LitLoc32) -> Self {
        Self::Lit(LitLoc::F32(loc))
    }
    pub const fn lit64(loc: LitLoc64) -> Self {
        Self::Lit(LitLoc::F64(loc))
    }
}

impl TryFrom<u8> for Src2 {
    type Error = Error;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value & Self::IMM_FLAG_MASK == Self::IMM_FLAG {
            Ok(Self::Imm(Imm4::new_truncate(value)))
        } else if value & Self::LIT_FLAG_MASK == Self::LIT_FLAG {
            LitLoc::from_raw(value & Self::LIT_MASK)
                .map(Self::Lit)
                .ok_or(Error::Src2LitError)
        } else {
            Ok(Reg::from_raw(value).map(Self::Reg).unwrap())
        }
    }
}

impl Into<u8> for Src2 {
    fn into(self) -> u8 {
        match self {
            Self::Reg(reg) => reg.into_raw(),
            Self::Imm(imm) => imm.get() | Self::IMM_FLAG,
            Self::Lit(lit) => lit.into_raw() | Self::LIT_FLAG,
        }
    }
}

impl TryFrom<&'_ Raw> for Src2 {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        Self::try_from(raw.als.src2())
    }
}

impl InsertInto<Raw> for Src2 {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_src2(self.into());
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

impl TryFrom<&'_ Raw> for Src3 {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        Reg::from_raw(raw.ales.src3())
            .map(Self)
            .ok_or(Error::Src3Error)
    }
}

impl InsertInto<Raw> for Src3 {
    fn insert_into(self, raw: &mut Raw) {
        raw.ales.set_src3(self.0.into_raw())
    }
}

impl fmt::Display for Src3 {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, fmt)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Src4(pub Reg);

impl TryFrom<&'_ Raw> for Src4 {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        Reg::from_raw(raw.als.src4())
            .map(Self)
            .ok_or(Error::Src4Error)
    }
}

impl InsertInto<Raw> for Src4 {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_src4(self.0.into_raw())
    }
}

impl fmt::Display for Src4 {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, fmt)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Addr(pub Src1, pub Src2);

impl TryFrom<&'_ Raw> for Addr {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        Ok(Addr(Src1::try_from(raw)?, Src2::try_from(raw)?))
    }
}

impl InsertInto<Raw> for Addr {
    fn insert_into(self, raw: &mut Raw) {
        self.0.insert_into(raw);
        self.1.insert_into(raw);
    }
}

impl fmt::Display for Addr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.1 {
            Src2::Imm(i) if i.get() == 0 => write!(fmt, "[ {} ]", self.0),
            _ => write!(fmt, "[ {} + {} ]", self.0, self.1),
        }
    }
}

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
}

impl TryFrom<u8> for Dst {
    type Error = Error;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value == Self::TST {
            Ok(Self::Tst)
        } else if value == Self::TC {
            Ok(Self::Tc)
        } else if value == Self::TCD {
            Ok(Self::Tcd)
        } else if value & Self::CTPR_FLAG_MASK == Self::CTPR_FLAG {
            Ctpr::new(value & 0x3)
                .map(Self::Ctpr)
                .ok_or(Error::DstError)
        } else if value == Self::EMPTY {
            Ok(Self::Empty)
        } else {
            Reg::from_raw(value).map(Self::Reg).ok_or(Error::DstError)
        }
    }
}

impl Into<u8> for Dst {
    fn into(self) -> u8 {
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

impl TryFrom<&'_ Raw> for Dst {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        Self::try_from(raw.als.dst())
    }
}

impl InsertInto<Raw> for Dst {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_dst(self.into());
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

impl fmt::Display for DstPreg {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, fmt)
    }
}

impl TryFrom<u8> for DstPreg {
    type Error = Error;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Preg::new(value).map(Self).ok_or(Error::BadFormat)
    }
}

impl TryFrom<&'_ Raw> for DstPreg {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        Self::try_from(raw.als.cmp_dst())
    }
}

impl InsertInto<Raw> for DstPreg {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_cmp_dst(self.0.get())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SrcState(pub StateReg);

impl fmt::Display for SrcState {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, fmt)
    }
}

impl TryFrom<&'_ Raw> for SrcState {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        StateReg::try_from(raw.als.src1())
            .map(Self)
            .map_err(|_| Error::StateReg)
    }
}

impl InsertInto<Raw> for SrcState {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_src1(self.0.into())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct DstState(pub StateReg);

impl fmt::Display for DstState {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, fmt)
    }
}

impl TryFrom<&'_ Raw> for DstState {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        StateReg::try_from(raw.als.dst())
            .map(Self)
            .map_err(|_| Error::StateReg)
    }
}

impl InsertInto<Raw> for DstState {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_dst(self.0.into())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct SrcCond {
    pub invert: bool,
    pub pred: Pred,
}

impl TryFrom<&'_ Raw> for SrcCond {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        raw.cond.ok_or(Error::MrgcNotFound)
    }
}

impl InsertInto<Raw> for SrcCond {
    fn insert_into(self, raw: &mut Raw) {
        raw.cond = Some(self);
    }
}

impl fmt::Display for SrcCond {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.invert {
            fmt.write_str("~")?;
        }
        fmt::Display::fmt(&self.pred, fmt)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Tst;

impl fmt::Display for Tst {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str("%tst")
    }
}

impl TryFrom<&'_ Raw> for Tst {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        if raw.als.dst() == 0xcd {
            Ok(Self)
        } else {
            Err(Error::TstError)
        }
    }
}

impl InsertInto<Raw> for Tst {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_dst(0xcd)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AaIndex {
    Index(aau::Index),
    Sti(aau::Sti),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AddrArray {
    pub aad: aau::Aad,
    pub index: AaIndex,
    pub lts: Option<LitLoc32>,
    pub incr: Option<aau::Incr>,
}

impl fmt::Display for AddrArray {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}[ ", self.aad)?;
        match self.index {
            AaIndex::Index(i) => fmt::Display::fmt(&i, fmt)?,
            AaIndex::Sti(i) => fmt::Display::fmt(&i, fmt)?,
        }
        if let Some(lts) = self.lts {
            write!(fmt, " + _f32s,_lts{} TODO", lts.get())?;
        }
        fmt.write_str(" ]")
    }
}

impl TryFrom<&'_ Raw> for AddrArray {
    type Error = Error;
    fn try_from(raw: &Raw) -> Result<Self, Self::Error> {
        let aad = aau::Aad::new_truncate(raw.als.aa_aad());
        let index = if raw.als.aa_is_ind() {
            AaIndex::Index(aau::Index::new_truncate(raw.als.aa_ind()))
        } else {
            AaIndex::Sti(aau::Sti::new_truncate(raw.als.aa_sti()))
        };
        let lts = if raw.als.aa_lts() > 0 {
            Some(LitLoc32::new_truncate(raw.als.aa_lts() - 1))
        } else {
            None
        };
        let incr = if raw.als.aa_inc() {
            Some(aau::Incr::new_truncate(raw.als.aa_incr()))
        } else {
            None
        };
        Ok(Self {
            aad,
            index,
            lts,
            incr,
        })
    }
}

impl InsertInto<Raw> for AddrArray {
    fn insert_into(self, raw: &mut Raw) {
        raw.als.set_aa_aad(self.aad.get());
        match self.index {
            AaIndex::Index(i) => {
                raw.als.set_aa_is_ind(true);
                raw.als.set_aa_ind(i.get())
            }
            AaIndex::Sti(i) => raw.als.set_aa_sti(i.get()),
        }
        if let Some(lts) = self.lts {
            raw.als.set_aa_lts(lts.get() + 1);
        }
        if let Some(incr) = self.incr {
            raw.als.set_aa_inc(true);
            raw.als.set_aa_incr(incr.get());
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

    #[test]
    fn src2_literal_from_raw() {
        let src2 = Src2::lit16lo(LitLoc16::new_clamp(0));
        assert_eq!(Src2::try_from(0xd0), Ok(src2));
        let src2 = Src2::lit16lo(LitLoc16::new_clamp(1));
        assert_eq!(Src2::try_from(0xd1), Ok(src2));
        let src2 = Src2::lit16hi(LitLoc16::new_clamp(0));
        assert_eq!(Src2::try_from(0xd4), Ok(src2));
        let src2 = Src2::lit16hi(LitLoc16::new_clamp(1));
        assert_eq!(Src2::try_from(0xd5), Ok(src2));
        let src2 = Src2::lit32(LitLoc32::new_clamp(0));
        assert_eq!(Src2::try_from(0xd8), Ok(src2));
        let src2 = Src2::lit32(LitLoc32::new_clamp(3));
        assert_eq!(Src2::try_from(0xdb), Ok(src2));
        let src2 = Src2::lit64(LitLoc64::new_clamp(0));
        assert_eq!(Src2::try_from(0xdc), Ok(src2));
        let src2 = Src2::lit64(LitLoc64::new_clamp(2));
        assert_eq!(Src2::try_from(0xde), Ok(src2));
    }
}
