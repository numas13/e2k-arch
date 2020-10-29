use crate::raw::syllable::Lts;
use crate::raw::{Packed, Unpacked};
use crate::{
    aau::Aau,
    alc::Alc,
    cu::{Control0, Control1, Ct, Ipd, Nop, Short},
    plu::Plu,
    Error, InsertInto,
};
use core::convert::TryFrom;
use core::fmt;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Bundle {
    pub loop_mode: bool,
    pub nop: Nop,
    pub ct: Option<Ct>,
    pub ipd: Ipd,
    pub short: Short,
    pub alc: Alc,
    pub control0: Option<Control0>,
    pub control1: Option<Control1>,
    pub aau: Aau,
    pub plu: Plu,
    pub lts_count: usize,
    pub lts: [u32; 4],
}

impl Bundle {
    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, &[u8]), Error> {
        let (packed, tail) = Packed::from_bytes(bytes)?;
        let bundle = Bundle::from_packed(packed)?;
        Ok((bundle, tail))
    }
    pub fn from_packed(packed: &Packed) -> Result<Self, Error> {
        Self::from_unpacked(&Unpacked::unpack(packed)?)
    }
    pub fn from_unpacked(bundle: &Unpacked) -> Result<Self, Error> {
        let hs = bundle.hs;
        let control0 = if hs.cs0() {
            Some(Control0::from_raw(&bundle.cs0))
        } else {
            None
        };
        let control1 = if hs.cs1() {
            Some(Control1::from_raw(&bundle))
        } else {
            None
        };
        let lts_count = bundle.lts_count;
        let mut lts = [0u32; 4];
        for i in 0..lts_count {
            lts[i] = bundle.lts[i].0;
        }
        Ok(Self {
            loop_mode: hs.loop_mode(),
            nop: Nop::new_truncate(hs.raw_nop()),
            ct: Ct::from_raw(&bundle.ss),
            ipd: Ipd::new_truncate(bundle.ss.ipd()),
            short: Short::from_raw(&bundle.ss),
            alc: Alc::try_from(bundle)?,
            control0,
            control1,
            aau: Aau::unpack_from(bundle),
            plu: Plu::from_raw(bundle),
            lts_count,
            lts,
        })
    }
    pub fn into_raw(self) -> Unpacked {
        let mut bundle = Unpacked::default();
        bundle.hs.set_loop_mode(self.loop_mode);
        bundle.hs.set_raw_nop(self.nop.get());
        bundle.ss.set_ipd(self.ipd.get());
        bundle.ss.0 |= self.short.into_raw().0;
        bundle.ss.0 |= self.ct.map_or(0, |ct| ct.into_raw().0);
        self.aau.pack_into(&mut bundle);
        if bundle.ss.0 != 0 {
            bundle.hs.set_ss(true);
        }
        if let Some(control0) = self.control0 {
            bundle.hs.set_cs0(true);
            bundle.cs0 = control0.into_raw();
        }
        if let Some(control1) = self.control1 {
            bundle.hs.set_cs1(true);
            let (cs1, lts0) = control1.into_raw();
            bundle.cs1 = cs1;
            if let Some(lts0) = lts0 {
                bundle.lts[bundle.lts_count].0 = lts0.0;
                bundle.lts_count += 1;
            }
        }
        self.alc.insert_into(&mut bundle);
        let e2 = bundle.ales[2].op();
        let e5 = bundle.ales[5].op();
        if (e2 == 0x00 || e2 == 0x01) && (e5 == 0x00 || e5 == 0x01) {
            bundle.ales[2].0 = 0;
            bundle.ales[5].0 = 0;
        }
        self.plu.pack_into(&mut bundle);
        bundle.lts_count = self.lts_count;
        for i in 0..self.lts_count {
            bundle.lts[i] = Lts(self.lts[i]);
        }
        bundle
    }
}

impl fmt::Display for Bundle {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.loop_mode {
            writeln!(fmt, "loop_mode")?;
        }
        if self.nop.get() != 0 {
            writeln!(fmt, "{}", self.nop)?;
        }
        if let Some(ct) = self.ct {
            ct.print(fmt, self.control0.as_ref(), self.control1.as_ref())?;
        }
        if self.ipd.get() != 0 {
            writeln!(fmt, "{}", self.ipd)?;
        }
        fmt::Display::fmt(&self.short, fmt)?;
        self.alc.print(&self.lts, fmt)?;
        if let Some(control0) = self.control0 {
            control0.print(fmt, self.ct.as_ref())?;
        }
        if let Some(control1) = self.control1 {
            control1.print(fmt, self.ct.as_ref())?;
        }
        fmt::Display::fmt(&self.aau, fmt)?;
        fmt::Display::fmt(&self.plu, fmt)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn bundle_pack_unpack() {
        for path in crate::TEST_BUNDLES_PATHS {
            let data = fs::read(path).unwrap();
            let (bundle, tail) = Bundle::from_bytes(data.as_slice()).unwrap();
            assert_eq!(tail, &[], "{}", path);
            let mut buffer = [0u8; 64];
            let (packed, _) = bundle.into_raw().pack(&mut buffer).unwrap();
            assert_eq!(packed.as_slice(), data, "{}", path);
        }
    }
}
