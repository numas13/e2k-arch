use bitfield::bitfield;
use bitflags::bitflags;
use core::fmt;
use core::num::NonZeroU8;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct Ctpr(NonZeroU8);

impl Ctpr {
    pub const MIN: Ctpr = unsafe { Self::new_unchecked(1) };
    pub const MAX: Ctpr = unsafe { Self::new_unchecked(3) };
    /// Creates a `Ctpr` without checking the given value.
    ///
    /// # Safety
    ///
    /// The value must be in range of 1 to 3.
    pub const unsafe fn new_unchecked(value: u8) -> Self {
        Self(NonZeroU8::new_unchecked(value))
    }
    /// Returns the value as a primitive type.
    pub const fn get(&self) -> u8 {
        self.0.get()
    }
    /// Creates a `Ctpr` if the given value within range of 1 to 3.
    pub const fn new(value: u8) -> Option<Self> {
        if Self::MIN.get() <= value && value <= Self::MAX.get() {
            Some(unsafe { Self::new_unchecked(value) })
        } else {
            None
        }
    }
    /// Creates a `Ctpr` from the given value or clamp it if out of range of 1 to 3.
    pub const fn new_clamp(value: u8) -> Self {
        if value <= Self::MAX.get() {
            if Self::MIN.get() <= value {
                unsafe { Self::new_unchecked(value) }
            } else {
                Self::MIN
            }
        } else {
            Self::MAX
        }
    }
}

impl Default for Ctpr {
    fn default() -> Self {
        Self::MIN
    }
}

impl fmt::Display for Ctpr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "%ctpr{}", self.get())
    }
}

bitfield_from_into!(Pred, u8);
bitfield! {
    #[derive(Copy, Clone, Default, PartialEq)]
    pub struct Pred(u8);
    pub u8, raw_spred, set_raw_spred: 5, 0;
    pub u8, raw_pcnt, set_raw_pcnt: 4, 0;
    pub u8, raw_preg, set_raw_preg: 4, 0;
}

impl Pred {
    const PCNT_FLAG: u8 = 0x40;
    const PCNT_FLAG_MASK: u8 = 0xe0;
    const PREG_FLAG: u8 = 0x60;
    const PREG_FLAG_MASK: u8 = 0xe0;
    pub fn is_lcntex(&self) -> bool {
        self.0 == 0
    }
    pub fn set_lcntex(&mut self) {
        self.0 = 0;
    }
    pub fn spred(&self) -> Option<u8> {
        if self.0.leading_zeros() >= 2 {
            Some(self.raw_spred())
        } else {
            None
        }
    }
    pub fn set_spred(&mut self, value: u8) {
        self.0 = 0;
        self.set_raw_spred(value);
    }
    pub fn pcnt(&self) -> Option<u8> {
        if self.0 & Self::PCNT_FLAG_MASK == Self::PCNT_FLAG {
            Some(self.raw_pcnt())
        } else {
            None
        }
    }
    pub fn set_pcnt(&mut self, value: u8) {
        self.0 = Self::PCNT_FLAG;
        self.set_raw_pcnt(value);
    }
    pub fn preg(&self) -> Option<u8> {
        if self.0 & Self::PREG_FLAG_MASK == Self::PREG_FLAG {
            Some(self.raw_preg())
        } else {
            None
        }
    }
    pub fn set_preg(&mut self, value: u8) {
        self.0 = Self::PREG_FLAG;
        self.set_raw_preg(value);
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Preg(u8) {
        const MASK = 0x1f;
        const FMT = "%pred{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Pcnt(u8) {
        const MASK = 0x1f;
        const FMT = "%pcnt{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Rndpred(u8) {
        const MASK = 0x1f;
        const FMT = "%rndpred{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Area(u8) {
        const MASK = 0x3f;
        const FMT = "area = {}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Index(u8) {
        const MASK = 0x1f;
        const FMT = "%aaind{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Sti(u8) {
        const MASK = 0x1f;
        const FMT = "%aasti{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Aad(u8) {
        const MASK = 0x3f;
        const FMT = "%aad{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Incr(u8) {
        const MASK = 0x07;
        const FMT = "%aaincr{}";
    }
}

bitflags! {
    pub struct DtAl: u8 {
        const DT_AL0 = 1 << 0;
        const DT_AL1 = 1 << 1;
        const DT_AL3 = 1 << 2;
        const DT_AL4 = 1 << 3;
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct ClpIdx(u8) {
        const RANGE = 0..=2;
        const FMT = "{}";
    }
}
