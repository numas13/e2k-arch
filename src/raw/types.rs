//! Low-level types.

use core::fmt;

pub const LIT_POS_MASK: u8 = 0x3;
pub const LIT16_FLAG_MASK: u8 = 0xa;
pub const LIT16_FLAG: u8 = 0;
pub const LIT16_HI_BIT: u8 = 0x4;
pub const LIT32_FLAG_MASK: u8 = 0xc;
pub const LIT32_FLAG: u8 = 0x8;
pub const LIT64_FLAG_MASK: u8 = 0xc;
pub const LIT64_FLAG: u8 = 0xc;

pub const SRC2_LIT_FLAG_MASK: u8 = 0xf0;
pub const SRC2_LIT_FLAG: u8 = 0xd0;
pub const SRC2_LIT_MASK: u8 = 0x0f;

macro_rules! newtype {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident($ty_vis:vis $ty:ty) {
            $($body:tt)*
        }
    ) => (
        $(#[$meta])*
        $vis struct $name($ty_vis $ty);

        impl $name {
            /// Creates a newtype wrapper without checking the given value.
            ///
            /// # Safety
            ///
            /// The value must be in the range.
            pub const unsafe fn new_unchecked(value: $ty) -> Self {
                Self(value)
            }

            /// Returns the value as a primitive type.
            pub const fn get(&self) -> $ty {
                self.0
            }
        }

        newtype! { @body $name, $ty, $($body)* }
    );
    (@body $name:ident, $ty:ty, const RANGE = 0..$max:literal; $($rest:tt)*) => (
        impl $name {
            /// Creates a newtype wrapper if the given value within the range.
            pub const fn new(value: $ty) -> Option<Self> {
                if value < $max {
                    Some(Self(value))
                } else {
                    None
                }
            }

            /// Creates a newtype wrapper from the given value or clamp it if out of
            /// the range.
            pub const fn new_clamp(value: $ty) -> Self {
                if value < $max {
                    Self(value)
                } else {
                    Self($max - 1)
                }
            }
        }

        newtype! { @body $name, $ty, $($rest)* }
    );
    (@body $name:ident, $ty:ty, const RANGE = $min:literal..$max:literal; $($rest:tt)*) => (
        impl $name {
            /// Creates a newtype wrapper if the given value within the range.
            pub const fn new(value: $ty) -> Option<Self> {
                if $min <= value && value < $max {
                    Some(Self(value))
                } else {
                    None
                }
            }

            /// Creates a newtype wrapper from the given value or clamp it if out of
            /// the range.
            pub const fn new_clamp(value: $ty) -> Self {
                if value < $max {
                    if $n <= value {
                        Self(value)
                    } else {
                        Self($min)
                    }
                } else {
                    Self($max - 1)
                }
            }
        }

        newtype! { @body $name, $ty, $($rest)* }
    );
    (@body $name:ident, $ty:ty, ) => ();
}

newtype! {
    /// A 16-bit literal value location in a bundle.
    ///
    /// The location must be within range of 0 and 1 inclusive.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LitLoc16(u8) {
        const RANGE = 0..2;
    }
}

newtype! {
    /// A 32-bit literal value location in a bundle.
    ///
    /// The location must be within range of 0 and 3 inclusive.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LitLoc32(u8) {
        const RANGE = 0..4;
    }
}

newtype! {
    /// A 64-bit literal value location in a bundle.
    ///
    /// The location must be within range of 0 and 2 inclusive.
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LitLoc64(u8) {
        const RANGE = 0..3;
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
    /// Tries to create a new `LitLoc` from the given raw value.
    pub fn from_u8(value: u8) -> Option<Self> {
        let loc = value & LIT_POS_MASK;
        if value & LIT64_FLAG_MASK == LIT64_FLAG {
            LitLoc64::new(loc).map(Self::F64)
        } else if value & LIT32_FLAG_MASK == LIT32_FLAG {
            LitLoc32::new(loc).map(Self::F32)
        } else if value & LIT16_FLAG_MASK == LIT16_FLAG {
            let part = if value & LIT16_HI_BIT == 0 {
                LitPart::Lo
            } else {
                LitPart::Hi
            };
            Some(Self::F16(LitLoc16::new(loc)?, part))
        } else {
            None
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

/// A `src2` operand for an `ALC` instruction.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Src2 {
    /// A literal value location in a bundle.
    Lit(LitLoc),
}

impl Src2 {
    /// Tries to create a `Src2` from the given raw value.
    pub fn from_u8(value: u8) -> Option<Self> {
        if value & SRC2_LIT_FLAG_MASK == SRC2_LIT_FLAG {
            LitLoc::from_u8(value & SRC2_LIT_MASK).map(Self::Lit)
        } else {
            None
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
        use LitLoc::*;
        use LitPart::*;
        use Src2::Lit;

        assert_eq!(
            Src2::from_u8(0xd0),
            Some(Lit(F16(LitLoc16::new_clamp(0), Lo)))
        );
        assert_eq!(
            Src2::from_u8(0xd1),
            Some(Lit(F16(LitLoc16::new_clamp(1), Lo)))
        );
        assert_eq!(
            Src2::from_u8(0xd4),
            Some(Lit(F16(LitLoc16::new_clamp(0), Hi)))
        );
        assert_eq!(
            Src2::from_u8(0xd5),
            Some(Lit(F16(LitLoc16::new_clamp(1), Hi)))
        );
        assert_eq!(Src2::from_u8(0xd8), Some(Lit(F32(LitLoc32::new_clamp(0)))));
        assert_eq!(Src2::from_u8(0xdb), Some(Lit(F32(LitLoc32::new_clamp(3)))));
        assert_eq!(Src2::from_u8(0xdc), Some(Lit(F64(LitLoc64::new_clamp(0)))));
        assert_eq!(Src2::from_u8(0xde), Some(Lit(F64(LitLoc64::new_clamp(2)))));
    }
}
