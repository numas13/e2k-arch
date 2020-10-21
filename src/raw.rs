pub mod syllable;

use self::syllable::{Aas, AasDst, Ales, Als, Cds, Cs0, Cs1, Hs, Lts, Pls, Ss};
use crate::{error::Error, util};

#[repr(transparent)]
pub struct Packed {
    data: [u8],
}

impl Packed {
    pub fn from_slice(slice: &[u8]) -> Result<(&Self, &[u8]), Error> {
        if slice.is_empty() {
            return Err(Error::NeedMoreBytes(8));
        }
        let len = Hs(slice[0] as u32).len();
        if slice.len() < len {
            return Err(Error::NeedMoreBytes(len - slice.len()));
        }
        let (data, tail) = slice.split_at(len);
        // safe because Bundle has repr(transparent)
        let bundle = unsafe { &*(data as *const [u8] as *const Packed) };
        Ok((bundle, tail))
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }

    pub fn hs(&self) -> Hs {
        Hs(util::u32_from_le_slice(&self.data[0..4]))
    }

    pub fn ss(&self) -> Option<Ss> {
        if self.hs().ss() {
            Some(Ss(util::u32_from_le_slice(&self.data[4..8])))
        } else {
            None
        }
    }
}

#[derive(Clone, Default)]
pub struct Bundle {
    pub hs: Hs,
    pub ss: Ss,
    pub als: [Als; 6],
    pub cs0: Cs0,
    pub cs1: Cs1,
    pub ales: [Option<Ales>; 6],
    pub aas_dst: [u8; 4],
    pub aas: [Aas; 4],
    pub lts: [Option<Lts>; 4],
    pub pls: [Pls; 3],
    pub cds: [Cds; 3],
}

impl Bundle {
    pub fn from_slice(s: &[u8]) -> Result<(Self, &[u8]), Error> {
        let (packed, tail) = Packed::from_slice(s)?;
        Ok((Self::unpack(packed)?, tail))
    }

    pub fn unpack(packed: &Packed) -> Result<Self, Error> {
        let mut bundle = Bundle::default();
        let tail = bundle.unpack_head(packed.as_slice())?;
        let tail = bundle.unpack_middle(tail)?;
        bundle.unpack_tail(tail)?;
        Ok(bundle)
    }

    fn unpack_head<'a>(&mut self, src: &'a [u8]) -> Result<&'a [u8], Error> {
        let mut offset = 0;
        let mut read_u32 = || {
            if src.len() < offset + 4 {
                return Err(Error::UnexpectedEnd);
            }
            let val = util::u32_from_le_slice(&src[offset..]);
            offset += 4;
            Ok(val)
        };
        self.hs = Hs(read_u32()?);
        if self.hs.ss() {
            self.ss = Ss(read_u32()?);
        }

        for i in 0..6 {
            if self.hs.als_mask() & 1 << i != 0 {
                self.als[i] = Als(read_u32()?);
            }
        }
        if self.hs.cs0() {
            self.cs0 = Cs0(read_u32()?);
        }
        if self.hs.is_ales25() {
            let val = read_u32()?;
            self.ales[2] = Some(Ales((val >> 16) as u16));
            self.ales[5] = Some(Ales(val as u16));
        }
        let offset = self.hs.offset();
        if self.hs.cs1() {
            if src.len() < offset {
                return Err(Error::UnexpectedEnd);
            }
            self.cs1 = Cs1(util::u32_from_le_slice(&src[offset - 4..]));
        }
        Ok(&src[offset..])
    }

    fn unpack_middle<'a>(&mut self, src: &'a [u8]) -> Result<&'a [u8], Error> {
        let mut offset = 0;
        let mut read_u16 = || {
            if src.len() < offset + 2 {
                return Err(Error::UnexpectedEnd);
            }
            let val = util::u16_from_le_slice(&src[offset ^ 2..]);
            offset += 2;
            Ok(val)
        };
        let ales_mask = self.hs.ales_mask();
        for &i in [0, 1, 3, 4].iter() {
            if ales_mask & 1 << i != 0 {
                self.ales[i] = Some(Ales(read_u16()?));
            }
        }
        let aas_mask = self.ss.aas_mask();
        for i in 0..2 {
            let o = i * 2;
            if aas_mask & 3 << o != 0 {
                let dst = AasDst(read_u16()?);
                if aas_mask & 1 << o + 1 != 0 {
                    self.aas_dst[o + 1] = dst.dst1();
                }
                if aas_mask & 1 << o != 0 {
                    self.aas_dst[o] = dst.dst0();
                }
            }
        }
        for i in 0..4 {
            if aas_mask & 1 << i != 0 {
                self.aas[i] = Aas(read_u16()?);
            }
        }
        Ok(&src[(offset + 3) & !3..])
    }

    fn unpack_tail<'a>(&mut self, src: &'a [u8]) -> Result<&'a [u8], Error> {
        let mut offset = src.len();
        let mut read_u32 = || {
            if offset < 4 {
                return Err(Error::UnexpectedEnd);
            }
            offset -= 4;
            Ok(util::u32_from_le_slice(&src[offset..]))
        };
        for i in 0..self.hs.cds_len() as usize {
            self.cds[i] = Cds(read_u32()?);
        }
        for i in 0..self.hs.pls_len() as usize {
            self.pls[i] = Pls(read_u32()?);
        }
        for i in 0..4 {
            match read_u32() {
                Ok(val) => self.lts[i] = Some(Lts(val)),
                Err(_) => break,
            }
        }
        Ok(&src[..offset])
    }
}
