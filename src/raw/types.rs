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
            pub const unsafe fn new_unchecked(value: $ty) -> Self {
                Self(value)
            }
        }

        newtype! { @body $name, $ty, $($body)* }
    );
    (@body $name:ident, $ty:ty, const RANGE = 0..$m:literal; $($rest:tt)*) => (
        impl $name {
            pub const fn new(value: $ty) -> Option<Self> {
                if value < $m {
                    Some(Self(value))
                } else {
                    None
                }
            }

            pub const fn new_clamp(value: $ty) -> Self {
                if value < $m {
                    Self(value)
                } else {
                    Self($m - 1)
                }
            }

            pub const fn value(&self) -> $ty {
                self.0
            }
        }

        newtype! { @body $name, $ty, $($rest)* }
    );
    (@body $name:ident, $ty:ty, const RANGE = $n:literal..$m:literal; $($rest:tt)*) => (
        impl $name {
            pub const fn new(value: $ty) -> Option<Self> {
                if $n <= value && value < $m {
                    Some(Self(value))
                } else {
                    None
                }
            }

            pub const fn new_clamp(value: $ty) -> Self {
                if value < $m {
                    if $n <= value {
                        Self(value)
                    } else {
                        Self($n)
                    }
                } else {
                    Self($m - 1)
                }
            }
        }

        newtype! { @body $name, $ty, $($rest)* }
    );
    (@body $name:ident, $ty:ty, ) => ();
}

newtype! {
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LiteralPos16(u8) {
        const RANGE = 0..2;
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LiteralPos32(u8) {
        const RANGE = 0..4;
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(transparent)]
    pub struct LiteralPos64(u8) {
        const RANGE = 0..3;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LiteralPos {
    F16lo(LiteralPos16),
    F16hi(LiteralPos16),
    F32(LiteralPos32),
    F64(LiteralPos64),
    // TODO: literal reference
}

impl LiteralPos {
    pub fn from_raw(value: u8) -> Option<Self> {
        let pos = value & LIT_POS_MASK;
        if value & LIT64_FLAG_MASK == LIT64_FLAG {
            LiteralPos64::new(pos).map(Self::F64)
        } else if value & LIT32_FLAG_MASK == LIT32_FLAG {
            LiteralPos32::new(pos).map(Self::F32)
        } else if value & LIT16_FLAG_MASK == LIT16_FLAG {
            let pos = LiteralPos16::new(pos);
            if value & LIT16_HI_BIT != 0 {
                pos.map(Self::F16hi)
            } else {
                pos.map(Self::F16lo)
            }
        } else {
            None
        }
    }
}

impl fmt::Display for LiteralPos {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::F16lo(p) => write!(fmt, "_f16s,_lts{}lo", p.value()),
            Self::F16hi(p) => write!(fmt, "_f16s,_lts{}hi", p.value()),
            Self::F32(p) => write!(fmt, "_f32s,_lts{}", p.value()),
            Self::F64(p) => write!(fmt, "_f64,_lts{}", p.value()),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Src2 {
    Lit(LiteralPos),
}

impl Src2 {
    pub fn from_raw(value: u8) -> Option<Self> {
        if value & SRC2_LIT_FLAG_MASK == SRC2_LIT_FLAG {
            LiteralPos::from_raw(value & SRC2_LIT_MASK).map(Self::Lit)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_pos() {
        assert!(LiteralPos16::new(1).is_some());
        assert!(LiteralPos16::new(2).is_none());
        assert!(LiteralPos32::new(3).is_some());
        assert!(LiteralPos32::new(4).is_none());
        assert!(LiteralPos64::new(2).is_some());
        assert!(LiteralPos64::new(3).is_none());

        assert_eq!(LiteralPos16::new_clamp(8).value(), 1);
        assert_eq!(LiteralPos32::new_clamp(8).value(), 3);
        assert_eq!(LiteralPos64::new_clamp(8).value(), 2);
    }

    #[test]
    fn test_src2_literal() {
        use LiteralPos::*;
        use Src2::Lit;

        assert_eq!(
            Src2::from_raw(0xd0),
            Some(Lit(F16lo(LiteralPos16::new_clamp(0))))
        );
        assert_eq!(
            Src2::from_raw(0xd1),
            Some(Lit(F16lo(LiteralPos16::new_clamp(1))))
        );
        assert_eq!(
            Src2::from_raw(0xd4),
            Some(Lit(F16hi(LiteralPos16::new_clamp(0))))
        );
        assert_eq!(
            Src2::from_raw(0xd5),
            Some(Lit(F16hi(LiteralPos16::new_clamp(1))))
        );
        assert_eq!(
            Src2::from_raw(0xd8),
            Some(Lit(F32(LiteralPos32::new_clamp(0))))
        );
        assert_eq!(
            Src2::from_raw(0xdb),
            Some(Lit(F32(LiteralPos32::new_clamp(3))))
        );
        assert_eq!(
            Src2::from_raw(0xdc),
            Some(Lit(F64(LiteralPos64::new_clamp(0))))
        );
        assert_eq!(
            Src2::from_raw(0xde),
            Some(Lit(F64(LiteralPos64::new_clamp(2))))
        );
    }
}
