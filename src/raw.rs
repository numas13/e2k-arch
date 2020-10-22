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
        // safe because Packed has repr(transparent)
        let bundle = unsafe { Self::new_unchecked(data) };
        Ok((bundle, tail))
    }

    pub unsafe fn new_unchecked(data: &[u8]) -> &Self {
        &*(data as *const [u8] as *const Self)
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
    pub lts: [Lts; 4],
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
        let lts_count = self.get_max_lts_index().map_or(0, |i| i + 1);
        for i in 0..lts_count as usize {
            self.lts[i] = Lts(read_u32()?);
        }
        Ok(&src[..offset])
    }

    pub fn pack<'a>(&self, buffer: &'a mut [u8]) -> Result<(&'a Packed, &'a mut [u8]), Error> {
        let middle_offset = self.pack_head(buffer)?;
        let tail_offset = self.pack_middle(middle_offset, buffer)?;
        let len = self.pack_tail(tail_offset, buffer)?;
        let mut hs = self.hs;
        hs.set_raw_offset(middle_offset as u8 / 4 - 1);
        hs.set_raw_len(len as u8 / 8 - 1);
        buffer[0..4].copy_from_slice(&hs.0.to_le_bytes());
        let (head, tail) = buffer.split_at_mut(len);
        let packed = unsafe { Packed::new_unchecked(head) };
        Ok((packed, tail))
    }

    fn pack_head<'a>(&self, buffer: &'a mut [u8]) -> Result<usize, Error> {
        let mut offset = 4;
        let mut write_u32 = |value: u32| {
            let end = offset + 4;
            if end <= buffer.len() {
                let bytes = value.to_le_bytes();
                buffer[offset..end].copy_from_slice(&bytes);
                offset = end;
                Ok(())
            } else {
                Err(Error::UnexpectedEnd)
            }
        };

        if self.hs.ss() {
            write_u32(self.ss.0)?;
        }

        for (i, als) in self.als.iter().enumerate() {
            if self.hs.als_mask() & 1 << i != 0 {
                write_u32(als.0)?;
            }
        }

        if self.hs.cs0() {
            write_u32(self.cs0.0)?;
        }

        if self.ales[2].is_some() || self.ales[5].is_some() {
            let ales2 = self.ales[2].map(|i| i.0).unwrap_or_default();
            let ales5 = self.ales[5].map(|i| i.0).unwrap_or_default();
            write_u32((ales2 as u32) << 16 | ales5 as u32)?;
        }

        if self.hs.cs1() {
            write_u32(self.cs1.0)?;
        }

        Ok(offset)
    }

    fn pack_middle<'a>(&self, mut offset: usize, buffer: &'a mut [u8]) -> Result<usize, Error> {
        let mut write_u16 = |value: u16| {
            let start = offset ^ 2;
            let end = start + 2;
            if end <= buffer.len() {
                buffer[start..end].copy_from_slice(&value.to_le_bytes());
                offset += 2;
                Ok(())
            } else {
                Err(Error::UnexpectedEnd)
            }
        };

        for i in &[0, 1, 3, 4] {
            if let Some(ales) = self.ales[*i] {
                if self.hs.ales_mask() & 1 << *i != 0 {
                    write_u16(ales.0)?;
                }
            }
        }

        for i in (0..4).step_by(2) {
            if self.ss.aas_mask() & 3 << i != 0 {
                let dst0 = self.aas_dst[i] as u16;
                let dst1 = self.aas_dst[i + 1] as u16;
                write_u16(dst0 << 8 | dst1)?;
            }
        }

        for i in 0..4 {
            if self.ss.aas_mask() & 1 << i != 0 {
                write_u16(self.aas[i].0)?;
            }
        }

        Ok((offset + 3) & !3)
    }

    fn pack_tail<'a>(&self, mut offset: usize, buffer: &'a mut [u8]) -> Result<usize, Error> {
        let lts_count = self.get_max_lts_index().map_or(0, |i| i + 1);
        let count = (lts_count + self.hs.pls_len() + self.hs.cds_len()) as usize;

        if (offset + count * 4) % 8 != 0 {
            offset += 4;
        }

        if buffer.len() < offset + count * 4 {
            return Err(Error::BadFormat);
        }

        let mut write_u32 = |value: u32| {
            let end = offset + 4;
            buffer[offset..end].copy_from_slice(&value.to_le_bytes());
            offset = end;
        };

        for lts in self.lts.iter().take(lts_count as usize).rev() {
            write_u32(lts.0);
        }

        for pls in self.pls.iter().take(self.hs.pls_len() as usize).rev() {
            write_u32(pls.0);
        }

        for cds in self.cds.iter().take(self.hs.cds_len() as usize).rev() {
            write_u32(cds.0);
        }

        Ok(offset)
    }

    pub fn get_max_lts_index(&self) -> Option<u8> {
        let mut ret = if self.hs.cs1() && self.cs1.is_lts0() {
            Some(0)
        } else {
            None
        };

        for (i, als) in self.als.iter().enumerate() {
            if self.hs.als_mask() & 1 << i != 0 {
                let src2 = als.src2();
                if src2 & 0xf0 == 0xd0 {
                    let lit = src2 & 0xf;
                    let index = src2 & 0x3;
                    if lit & 0xc == 0xc && index < 3 {
                        ret = Some(index + 1)
                    } else if lit & 0xc == 0x8 {
                        ret = Some(index);
                    } else if lit & 0x8 == 0 && index < 2 {
                        ret = Some(index);
                    }
                }
            }
        }
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_bundle() {
        let paths = [
            "test-data/bundle0.bin",
            "test-data/bundle1.bin",
            "test-data/bundle2.bin",
            "test-data/bundle3.bin",
            "test-data/bundle4.bin",
        ];

        for path in &paths {
            let data = fs::read(path).unwrap();
            let (bundle, _) = Bundle::from_slice(data.as_slice()).unwrap();
            let mut buffer = [0u8; 64];
            let (packed, _) = bundle.pack(&mut buffer).unwrap();
            assert_eq!(packed.as_slice(), data);
        }
    }
}
