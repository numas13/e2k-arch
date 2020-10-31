use super::RawInstr;
use crate::raw::types::{Aad, Incr, Index, Sti};
use crate::state::lit_loc::{LitLoc32, LitValue};
use crate::InsertInto;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Not found array address _f32s,_lts{0}")]
    NotFoundLts(u8),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AaIndex {
    Index(Index),
    Sti(Sti),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AddrArray {
    pub aad: Aad,
    pub index: AaIndex,
    pub lts: Option<(LitLoc32, u32)>,
    pub incr: Option<Incr>,
}

impl fmt::Display for AddrArray {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}[ ", self.aad)?;
        match self.index {
            AaIndex::Index(i) => fmt::Display::fmt(&i, fmt)?,
            AaIndex::Sti(i) => fmt::Display::fmt(&i, fmt)?,
        }
        if let Some((loc, val)) = self.lts {
            write!(fmt, " + _f32s,_lts{} {:#x}", loc.get(), val)?;
        }
        fmt.write_str(" ]")
    }
}

impl AddrArray {
    pub fn new(raw: &RawInstr, lts: &[Option<u32>; 4]) -> Result<Self, DecodeError> {
        let aad = raw.als.aa_aad();
        let index = if raw.als.aa_is_ind() {
            AaIndex::Index(raw.als.aa_ind())
        } else {
            AaIndex::Sti(raw.als.aa_sti())
        };
        let lts = if raw.als.aa_lts() > 0 {
            let loc = raw.als.aa_lts() - 1;
            let val = match lts[loc as usize] {
                Some(val) => val,
                None => return Err(DecodeError::NotFoundLts(loc)),
            };
            Some((LitLoc32::new_truncate(loc), val))
        } else {
            None
        };
        let incr = if raw.als.aa_inc() {
            Some(raw.als.aa_incr())
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

impl InsertInto<RawInstr> for AddrArray {
    fn insert_into(self, raw: &mut RawInstr) {
        raw.als.set_aa_aad(self.aad);
        match self.index {
            AaIndex::Index(i) => {
                raw.als.set_aa_is_ind(true);
                raw.als.set_aa_ind(i)
            }
            AaIndex::Sti(i) => raw.als.set_aa_sti(i),
        }
        if let Some((loc, val)) = self.lts {
            raw.als.set_aa_lts(loc.get() + 1);
            raw.lit = Some(LitValue::F32(loc, val));
        }
        if let Some(incr) = self.incr {
            raw.als.set_aa_inc(true);
            raw.als.set_aa_incr(incr);
        }
    }
}
