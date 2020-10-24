use core::fmt;

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Based(u8) {
        const MASK = 0x7f;
        const FMT = "%b[{}]";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Regular(u8) {
        const MASK = 0x3f;
        const FMT = "%r{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Global(u8) {
        const MASK = 0x1f;
        const FMT = "%g{}";
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Reg {
    Based(Based),
    Regular(Regular),
    Global(Global),
}

impl Reg {
    pub const REGULAR_FLAG: u8 = 0x80;
    pub const REGULAR_FLAG_MASK: u8 = 0xc0;
    pub const GLOBAL_FLAG: u8 = 0xe0;
    pub const GLOBAL_FLAG_MASK: u8 = 0xe0;

    pub const fn from_raw(value: u8) -> Option<Self> {
        if value.leading_zeros() >= 1 {
            Some(Self::Based(Based::new_truncate(value)))
        } else if value & Self::REGULAR_FLAG_MASK == Self::REGULAR_FLAG {
            Some(Self::Regular(Regular::new_truncate(value)))
        } else if value & Self::GLOBAL_FLAG_MASK == Self::GLOBAL_FLAG {
            Some(Self::Global(Global::new_truncate(value)))
        } else {
            None
        }
    }

    pub const fn into_raw(self) -> u8 {
        match self {
            Self::Based(b) => b.get(),
            Self::Regular(r) => r.get() | Self::REGULAR_FLAG,
            Self::Global(g) => g.get() | Self::GLOBAL_FLAG,
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Based(b) => fmt::Display::fmt(b, fmt),
            Self::Regular(r) => fmt::Display::fmt(r, fmt),
            Self::Global(g) => fmt::Display::fmt(g, fmt),
        }
    }
}

impl From<Based> for Reg {
    fn from(reg: Based) -> Self {
        Self::Based(reg)
    }
}

impl From<Regular> for Reg {
    fn from(reg: Regular) -> Self {
        Self::Regular(reg)
    }
}

impl From<Global> for Reg {
    fn from(reg: Global) -> Self {
        Self::Global(reg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reg_creation() {
        assert_eq!(Reg::from_raw(0x2c), Some(Based::new_truncate(44).into()));
        assert_eq!(Reg::from_raw(0x9c), Some(Regular::new_truncate(28).into()));
        assert_eq!(Reg::from_raw(0xef), Some(Global::new_truncate(15).into()));
        assert_eq!(Reg::from(Based::new_truncate(71)).into_raw(), 0x47);
        assert_eq!(Reg::from(Regular::new_truncate(45)).into_raw(), 0xad);
        assert_eq!(Reg::from(Global::new_truncate(7)).into_raw(), 0xe7);
    }
}
