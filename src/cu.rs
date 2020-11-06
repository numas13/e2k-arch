pub mod control0;
pub mod control1;
pub mod ct;
pub mod stubs;

pub use self::control0::Control0;
pub use self::control1::Control1;
pub use self::ct::Ct;
pub use self::stubs::Stubs;

use crate::raw::Unpacked;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Failed to decode CS0")]
    Cs0Decode {
        #[from]
        source: self::control0::DecodeError,
    },
    #[error("Failed to decode CS1")]
    Cs1DecodeError {
        #[from]
        source: self::control1::DecodeError,
    },
    #[error("Failed to decode control transfer")]
    CtDecodeError {
        #[from]
        source: self::ct::DecodeError,
    },
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum EncodeError {
    #[error("CS1_LTS0 is occupied")]
    OccupiedLts,
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Ipd(u8) {
        const MASK = 0x3;
        const FMT = "ipd {}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Nop(u8) {
        const MASK = 0x7;
        const FMT = "nop {}";
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Cu {
    pub loop_mode: bool,
    pub nop: Nop,
    pub ipd: Ipd,
    pub stubs: Stubs,
    pub ct: Option<Ct>,
    pub control0: Option<Control0>,
    pub control1: Option<Control1>,
}

impl Cu {
    pub fn unpack_from(raw: &Unpacked) -> Result<Self, DecodeError> {
        let ct = Ct::from_raw(raw.ss.ct()).map_err(|e| DecodeError::CtDecodeError { source: e })?;
        let control0 = if raw.hs.cs0() {
            Some(Control0::try_from(raw.cs0)?)
        } else {
            None
        };
        let control1 = if raw.hs.cs1() {
            let control1 = Control1::new(raw.cs1, raw.lts[0])
                .map_err(|e| DecodeError::Cs1DecodeError { source: e })?;
            Some(control1)
        } else {
            None
        };
        Ok(Cu {
            loop_mode: raw.hs.loop_mode(),
            nop: Nop::new_truncate(raw.hs.raw_nop()),
            ipd: Ipd::new_truncate(raw.ss.ipd()),
            stubs: Stubs::from(raw.ss),
            ct,
            control0,
            control1,
        })
    }
    pub fn pack_into(self, raw: &mut Unpacked) -> Result<(), EncodeError> {
        raw.hs.set_loop_mode(self.loop_mode);
        raw.hs.set_raw_nop(self.nop.get());
        raw.ss = self.stubs.into();
        raw.ss.set_ipd(self.ipd.get());
        if let Some(ct) = self.ct {
            raw.ss.set_ct(ct.into());
        }
        if let Some(control0) = self.control0 {
            raw.hs.set_cs0(true);
            raw.cs0 = control0.into();
        }
        if let Some(control1) = self.control1 {
            raw.hs.set_cs1(true);
            let (cs1, lts) = control1.into_raw();
            raw.cs1 = cs1;
            if let Some(lts) = lts {
                if raw.lts[0].is_none() {
                    raw.lts[0] = Some(lts.0);
                } else {
                    return Err(EncodeError::OccupiedLts);
                }
            }
        }
        Ok(())
    }
    pub fn display_stubs<'a>(&'a self) -> impl fmt::Display + 'a {
        struct Display<'a>(&'a Cu);

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let cu = self.0;
                if cu.loop_mode {
                    writeln!(f, "loop_mode")?;
                }
                if cu.nop.get() != 0 {
                    writeln!(f, "{}", cu.nop)?;
                }
                if let Some(ct) = cu.ct {
                    writeln!(
                        f,
                        "{}",
                        ct.display(cu.control0.as_ref(), cu.control1.as_ref())
                    )?;
                }
                if cu.ipd.get() != 0 {
                    writeln!(f, "{}", cu.ipd)?;
                }
                fmt::Display::fmt(&cu.stubs, f)
            }
        }

        Display(self)
    }
    pub fn display_controls<'a>(&'a self) -> impl fmt::Display + 'a {
        struct Display<'a>(&'a Cu);

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let cu = self.0;
                if let Some(control0) = cu.control0 {
                    fmt::Display::fmt(&control0.display(cu.ct.as_ref()), f)?;
                }
                if let Some(control1) = cu.control1 {
                    fmt::Display::fmt(&control1.display(cu.ct.as_ref()), f)?;
                }
                Ok(())
            }
        }

        Display(self)
    }
}
