mod clp;
mod elp;

pub use self::clp::*;
pub use self::elp::*;

use crate::raw;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Failed to decode CLP")]
    ClpDecode {
        #[from]
        source: self::clp::DecodeError,
    },
    #[error("Failed to decode ELP")]
    ElpDecode {
        #[from]
        source: self::elp::DecodeError,
    },
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum EncodeError {
    #[error("Invalid intermediate predicate found {found}, expected @p0 .. @p{expected}")]
    InvalidPredicate { found: IntermPred, expected: u8 },
    #[error("CLP {used_by} require result of undefined CLP {clp}")]
    UndefinedClp { used_by: u8, clp: u8 },
    #[error("CLP {used_by} require result of undefined ELP {elp}")]
    UndefinedElp { used_by: u8, elp: u8 },
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Plu {
    pub elp: [Option<Elp>; 4],
    pub clp: [Option<Clp>; 3],
}

impl Plu {
    pub fn from_slice(pls: &[raw::Pls]) -> Result<Self, DecodeError> {
        let mut ret = Self::default();
        let mut uses = [false; 7];
        for (i, pls) in pls.iter().enumerate().rev().take(3) {
            let clp = Clp::try_from(pls.clp())?;
            if clp.dst.is_some() || uses[i + 4] {
                for src in &clp.src {
                    uses[src.pred().get() as usize] = true;
                }
                ret.clp[i] = Some(clp);
            }
            if i < 2 {
                let elp_id = i * 2;
                if uses[elp_id] {
                    ret.elp[elp_id] = Some(Elp::try_from(pls.elp0())?);
                }
                let elp_id = elp_id + 1;
                if uses[elp_id] {
                    ret.elp[elp_id] = Some(Elp::try_from(pls.elp1())?);
                }
            }
        }
        Ok(ret)
    }
    pub fn into_raw(self) -> Result<[Option<raw::Pls>; 3], EncodeError> {
        let mut ret = [None; 3];
        let mut uses = [u8::MAX; 7];
        for i in (0..3).rev() {
            let mut pls = raw::Pls::default();
            let i_clp = i + 4;
            if let Some(clp) = self.clp[i] {
                if clp.dst.is_some() || uses[i_clp] != u8::MAX {
                    for src in &clp.src {
                        uses[src.pred().get() as usize] = i as u8;
                    }
                    pls.set_clp(clp.into());
                }
            } else if uses[i_clp] != u8::MAX {
                return Err(EncodeError::UndefinedClp {
                    clp: i_clp as u8,
                    used_by: uses[i_clp],
                });
            }
            let i_elp = i * 2;
            if uses[i_elp] != u8::MAX {
                if let Some(elp) = self.elp[i_elp] {
                    pls.set_elp0(elp.into());
                } else {
                    return Err(EncodeError::UndefinedElp {
                        elp: i_elp as u8,
                        used_by: uses[i_elp],
                    });
                }
            }
            let i_elp = i_elp + 1;
            if uses[i_elp] != u8::MAX {
                if let Some(elp) = self.elp[i_elp] {
                    pls.set_elp1(elp.into());
                } else {
                    return Err(EncodeError::UndefinedElp {
                        elp: i_elp as u8,
                        used_by: uses[i_elp],
                    });
                }
            }
            if pls.0 != 0 {
                ret[i] = Some(pls);
            }
        }
        Ok(ret)
    }
}

impl fmt::Display for Plu {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut uses = [u8::MAX; 7];
        for i in (0..3).rev() {
            let clp_i = i + 4;
            if let Some(clp) = self.clp[i] {
                if uses[clp_i] != u8::MAX || clp.dst.is_some() {
                    for src in &clp.src {
                        uses[src.pred().get() as usize] = i as u8;
                    }
                }
            }
            for (elp_i, elp) in self.elp.iter().enumerate().skip(i * 2).take(2) {
                if uses[elp_i] != u8::MAX {
                    match elp {
                        Some(elp) => writeln!(fmt, "pass {}, @p{}", elp, elp_i)?,
                        None => writeln!(fmt, "error: undefined @p{}", elp_i)?,
                    }
                }
            }
            if let Some(clp) = self.clp[i] {
                if uses[clp_i] != u8::MAX || clp.dst.is_some() {
                    writeln!(
                        fmt,
                        "{} {}, {}, @p{}",
                        clp.kind, clp.src[0], clp.src[1], clp_i
                    )?;
                    if let Some(dst) = clp.dst {
                        writeln!(fmt, "pass @p{}, {}", clp_i, dst)?;
                    }
                } else {
                    writeln!(fmt, "CLP {} is not used", i)?;
                }
            } else if uses[clp_i] != u8::MAX {
                writeln!(
                    fmt,
                    "CLP {} uses result of undefined CLP {}",
                    i, uses[clp_i]
                )?;
            }
        }
        Ok(())
    }
}
