use core::fmt::{self, Write};

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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Pred {
    Lcntex,
    Spred(u8),
    Pcnt(Pcnt),
    Preg(Preg),
}

impl Pred {
    pub const LCNTEX: u8 = 0;
    pub const SPRED_MASK: u8 = 0x3f;
    pub const PCNT_FLAG: u8 = 0x40;
    pub const PCNT_FLAG_MASK: u8 = 0xe0;
    pub const PRED_FLAG: u8 = 0x60;
    pub const PRED_FLAG_MASK: u8 = 0xe0;
    pub fn from_raw(value: u8) -> Self {
        if value == Self::LCNTEX {
            Self::Lcntex
        } else if value.leading_zeros() >= 2 {
            Self::Spred(value & Self::SPRED_MASK)
        } else if value & Self::PCNT_FLAG_MASK == Self::PCNT_FLAG {
            Self::Pcnt(Pcnt::new_truncate(value))
        } else {
            Self::Preg(Preg::new_truncate(value))
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::Lcntex => Self::LCNTEX,
            Self::Spred(v) => v,
            Self::Pcnt(p) => p.get() | Self::PCNT_FLAG,
            Self::Preg(p) => p.get() | Self::PRED_FLAG,
        }
    }
}

impl fmt::Display for Pred {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Lcntex => fmt.write_str("%lcntex"),
            Self::Spred(v) => {
                fmt.write_str("%spred")?;
                for i in 0..6 {
                    if *v & i << i != 0 {
                        let c = (b'0' + i) as char;
                        fmt.write_char(c)?;
                    }
                }
                Ok(())
            }
            Self::Pcnt(pcnt) => fmt::Display::fmt(pcnt, fmt),
            Self::Preg(preg) => fmt::Display::fmt(preg, fmt),
        }
    }
}
