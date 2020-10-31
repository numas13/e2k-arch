pub use crate::raw::types::Pcnt;
pub use crate::raw::types::Preg;

use crate::raw;
use core::fmt::{self, Write};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Pred {
    Lcntex,
    Spred(u8),
    Pcnt(Pcnt),
    Preg(Preg),
}

impl From<raw::types::Pred> for Pred {
    fn from(raw: raw::types::Pred) -> Self {
        if raw.is_lcntex() {
            Self::Lcntex
        } else if let Some(value) = raw.spred() {
            Self::Spred(value)
        } else if let Some(value) = raw.pcnt() {
            Self::Pcnt(Pcnt::new_truncate(value))
        } else if let Some(value) = raw.preg() {
            Self::Preg(Preg::new_truncate(value))
        } else {
            unreachable!()
        }
    }
}

impl Into<raw::types::Pred> for Pred {
    fn into(self) -> raw::types::Pred {
        let mut raw = raw::types::Pred::default();
        match self {
            Self::Lcntex => raw.set_lcntex(),
            Self::Spred(v) => raw.set_spred(v),
            Self::Pcnt(p) => raw.set_pcnt(p.get()),
            Self::Preg(p) => raw.set_preg(p.get()),
        }
        raw
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
