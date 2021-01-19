//! Low-level representations of bundles.

pub mod operand;
mod packed;
mod syllable;
pub mod types;

pub use self::packed::*;
pub use self::syllable::*;

use crate::state::pred::Pred;
use crate::util;
use thiserror::Error;

/// Error type for decode errors.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Failed to pre decode bundle")]
    PreDecode {
        #[from]
        source: PreDecodeError,
    },
    #[error("Unexpected buffer end")]
    UnexpectedSourceEnd,
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum EncodeError {
    #[error("Unexpected buffer end")]
    UnexpectedBufferEnd,
    #[error("Invalid bundle format, size exceeded 64 bytes")]
    InvalidFormat,
}

/// An unpacked bundle.
#[derive(Clone, Default)]
pub struct Unpacked {
    /// The header syllable.
    pub hs: Hs,
    /// The stubs syllable.
    pub ss: Ss,
    /// The syllables for arithmetic logic channels.
    pub als: [Als; 6],
    /// The syllable for first group control commands.
    pub cs0: Cs0,
    /// The syllable for second group control commands.
    pub cs1: Cs1,
    /// The half-syllable extensions for arithmetic logic channels.
    pub ales: [Ales; 6],
    /// The destination for array access unit channels.
    pub aas_dst: [u8; 4],
    /// The half-syllables for array access unit channels.
    pub aas: [Aas; 4],
    /// The syllables for literal values.
    pub lts: [Option<u32>; 4],
    /// The syllables for logical predicate processing.
    pub pls: [Pls; 3],
    /// The syllables for conditional execution.
    pub cds: [Cds; 3],
}

impl Unpacked {
    /// Tries to parse a bundle from the `buf`.
    ///
    /// # Errors
    ///
    /// `Error::NeedMoreBytes` will be returned if the `buf` does not have enough data.
    ///
    /// `Error::BadFormat` will be returned if the bundle size exceed 64 bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # use e2k_arch::raw::{Unpacked, Hs, Als};
    /// let bytes = [0x01, 0x00, 0x00, 0x04, 0x02, 0x01, 0x00, 0x00];
    /// let mut als = Als::default();
    /// als.set_op(0x00);
    /// als.set_raw_src1(0x00);
    /// als.set_raw_src2(0x01);
    /// als.set_raw_dst(0x02);
    /// let (packed, tail) = Unpacked::from_bytes(&bytes)?;
    /// assert!(packed.hs.als0());
    /// assert_eq!(packed.als[0].0, als.0);
    /// assert_eq!(tail, &[]);
    /// # Ok::<(), e2k_arch::raw::DecodeError>(())
    /// ```
    pub fn from_bytes(buf: &[u8]) -> Result<(Self, &[u8]), DecodeError> {
        let (packed, tail) = Packed::from_bytes(buf)?;
        Ok((Self::unpack(packed)?, tail))
    }
    /// Tries to unpack the packed bundle.
    pub fn unpack(packed: &Packed) -> Result<Self, DecodeError> {
        let mut bundle = Unpacked::default();
        let tail = bundle.unpack_head(packed.as_slice())?;
        let tail = bundle.unpack_middle(tail)?;
        bundle.unpack_tail(tail)?;
        Ok(bundle)
    }
    /// Tries to unpack `HS`, `SS`, `ALS0-5`, `CS0`, `ALES2/5` and `CS1`.
    fn unpack_head<'a>(&mut self, buf: &'a [u8]) -> Result<&'a [u8], DecodeError> {
        let mut offset = 0;
        let mut read_u32 = || {
            if buf.len() < offset + 4 {
                return Err(DecodeError::UnexpectedSourceEnd);
            }
            let val = util::u32_from_le_slice(&buf[offset..]);
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
        if self.hs.is_set_ales(2) {
            self.ales[2] = Ales::DEFAULT_25_EXT;
        }
        if self.hs.is_set_ales(5) {
            self.ales[5] = Ales::DEFAULT_25_EXT;
        }
        if self.hs.is_ales25() {
            let val = read_u32()?;
            self.ales[2] = Ales((val >> 16) as u16);
            self.ales[5] = Ales(val as u16);
        }
        let offset = self.hs.offset();
        if self.hs.cs1() {
            if buf.len() < offset {
                return Err(DecodeError::UnexpectedSourceEnd);
            }
            self.cs1 = Cs1(util::u32_from_le_slice(&buf[offset - 4..]));
        }
        Ok(&buf[offset..])
    }
    /// Tries to unpack `ALES0/1/3/4` and `AAS0-5`.
    fn unpack_middle<'a>(&mut self, buf: &'a [u8]) -> Result<&'a [u8], DecodeError> {
        let mut offset = 0;
        let mut read_u16 = || {
            if buf.len() < offset + 2 {
                return Err(DecodeError::UnexpectedSourceEnd);
            }
            let val = util::u16_from_le_slice(&buf[offset ^ 2..]);
            offset += 2;
            Ok(val)
        };
        let ales_mask = self.hs.ales_mask();
        for &i in [0, 1, 3, 4].iter() {
            if ales_mask & 1 << i != 0 {
                self.ales[i] = Ales(read_u16()?);
            }
        }
        let aas_mask = self.ss.aas_mask();
        for i in 0..2 {
            let o = i * 2;
            if aas_mask & 3 << o != 0 {
                let dst = AasDst(read_u16()?);
                self.aas_dst[o] = dst.dst0();
                self.aas_dst[o + 1] = dst.dst1();
            }
        }
        for i in 0..4 {
            if aas_mask & 1 << i != 0 {
                self.aas[i] = Aas(read_u16()?);
            }
        }
        Ok(&buf[(offset + 3) & !3..])
    }
    /// Tries to unpack `LTS0-3`, `PLS0-2` and `CDS0-2`.
    fn unpack_tail<'a>(&mut self, buf: &'a [u8]) -> Result<&'a [u8], DecodeError> {
        let mut offset = buf.len();
        let mut read_u32 = || {
            if offset < 4 {
                return Err(DecodeError::UnexpectedSourceEnd);
            }
            offset -= 4;
            Ok(util::u32_from_le_slice(&buf[offset..]))
        };
        for i in 0..self.hs.cds_len() as usize {
            self.cds[i] = Cds::from_raw(read_u32()?);
        }
        for i in 0..self.hs.pls_len() as usize {
            self.pls[i] = Pls(read_u32()?);
        }
        for i in 0..4 {
            match read_u32() {
                Ok(lts) => self.lts[i] = Some(lts),
                Err(_) => break,
            }
        }
        Ok(&buf[..offset])
    }
    /// Tries to pack bundle to the `buf`.
    ///
    /// Note that, if error will be returned the `buf` will be written up to 64 bytes of
    /// invalid data.
    ///
    /// # Errors
    ///
    /// `Error::UnexpectedEnd` will be returned if the `buf` does not have enough size.
    ///
    /// `Error::BadFormat` will be returned if the bundle size exceed 64 bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # use e2k_arch::raw::{Unpacked, Hs, Ss, Ct};
    /// let mut ct = Ct::default();
    /// ct.set_op(0x02);
    /// ct.set_preg(0x08);
    /// ct.set_ctpr(0x03);
    /// let mut ss = Ss::default();
    /// ss.set_ct(ct);
    /// ss.set_ipd(0x03);
    /// let mut bundle = Unpacked::default();
    /// bundle.hs.set_ss(true);
    /// bundle.ss = ss;
    /// let mut buf = [0u8; 64];
    /// let (packed, tail) = bundle.pack(&mut buf)?;
    /// let expected = [0x01, 0x10, 0x00, 0x00, 0x48, 0x0c, 0x00, 0xc0];
    /// assert_eq!(packed.as_slice(), &expected);
    /// # assert_eq!(tail, &[0u8; 56]);
    /// # Ok::<(), e2k_arch::raw::EncodeError>(())
    /// ```
    pub fn pack<'a>(&self, buf: &'a mut [u8]) -> Result<(&'a Packed, &'a mut [u8]), EncodeError> {
        let middle_offset = self.pack_head(buf)?;
        let tail_offset = self.pack_middle(middle_offset, buf)?;
        let len = self.pack_tail(tail_offset, buf)?;
        let mut hs = self.hs;
        hs.set_raw_offset(middle_offset as u8 / 4 - 1);
        hs.set_raw_len(len as u8 / 8 - 1);
        buf[0..4].copy_from_slice(&hs.0.to_le_bytes());
        let (head, tail) = buf.split_at_mut(len);
        let packed = unsafe { Packed::new_unchecked(head) };
        Ok((packed, tail))
    }
    /// Tries to pack `HS`, `SS`, `ALS0-5`, `CS0`, `ALES2/5` and `CS1` to the `buf`.
    fn pack_head(&self, buf: &mut [u8]) -> Result<usize, EncodeError> {
        let mut offset = 4;
        let mut write_u32 = |value: u32| {
            let end = offset + 4;
            if end <= buf.len() {
                let bytes = value.to_le_bytes();
                buf[offset..end].copy_from_slice(&bytes);
                offset = end;
                Ok(())
            } else {
                Err(EncodeError::UnexpectedBufferEnd)
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
        if self.ales[2] != Ales::NONE || self.ales[5] != Ales::NONE {
            let ales2 = if self.ales[2] != Ales::NONE {
                self.ales[2]
            } else {
                Ales::DEFAULT_25_EXT
            };
            let ales5 = if self.ales[5] != Ales::NONE {
                self.ales[5]
            } else {
                Ales::DEFAULT_25_EXT
            };
            if ales2.op() != 1 || ales5.op() != 1 {
                write_u32((ales2.0 as u32) << 16 | ales5.0 as u32)?;
            }
        }
        if self.hs.cs1() {
            write_u32(self.cs1.0)?;
        }
        Ok(offset)
    }
    /// Tries to pack `ALES0/1/3/4` and `AAS0-5` to the `buf`.
    fn pack_middle(&self, mut offset: usize, buf: &mut [u8]) -> Result<usize, EncodeError> {
        let mut write_u16 = |value: u16| {
            let start = offset ^ 2;
            let end = start + 2;
            if 64 < end {
                Err(EncodeError::InvalidFormat)
            } else if end <= buf.len() {
                buf[start..end].copy_from_slice(&value.to_le_bytes());
                offset += 2;
                Ok(())
            } else {
                Err(EncodeError::UnexpectedBufferEnd)
            }
        };
        for i in &[0, 1, 3, 4] {
            let ales = self.ales[*i].0;
            if ales != 0 {
                if self.hs.ales_mask() & 1 << *i != 0 {
                    write_u16(ales)?;
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
    /// Tries to pack `LTS0-3`, `PLS0-2` and `CDS0-2` to the `buf`.
    fn pack_tail(&self, mut offset: usize, buf: &mut [u8]) -> Result<usize, EncodeError> {
        let lts_count = self.lts_count();
        let count = lts_count + (self.hs.pls_len() + self.hs.cds_len()) as usize;
        if (offset + count * 4) % 8 != 0 {
            offset += 4;
        }
        if buf.len() < offset + count * 4 {
            return Err(EncodeError::InvalidFormat);
        }
        let mut write_u32 = |value: u32| {
            let end = offset + 4;
            buf[offset..end].copy_from_slice(&value.to_le_bytes());
            offset = end;
        };
        for lts in self.lts.iter().take(lts_count as usize).rev() {
            match lts {
                Some(lts) => write_u32(*lts),
                None => write_u32(0),
            }
        }
        for pls in self.pls.iter().take(self.hs.pls_len() as usize).rev() {
            write_u32(pls.0);
        }
        for cds in self.cds.iter().take(self.hs.cds_len() as usize).rev() {
            write_u32(cds.into_raw());
        }
        Ok(offset)
    }
    pub fn lts_count(&self) -> usize {
        self.lts
            .iter()
            .enumerate()
            .filter(|(_, lts)| lts.is_some())
            .last()
            .map_or(0, |(i, _)| i + 1)
    }
    pub fn push_rlp(&mut self, rlp: Rlp) {
        let count = self.rlp_iter().count();
        let cds_idx = count / 2;
        self.cds[cds_idx].rlp[count & 1] = rlp;
        self.hs.set_cds_len(cds_idx as u8 + 1);
    }
    pub fn rlp_iter<'a>(&'a self) -> impl Iterator<Item = &Rlp> + 'a {
        self.cds
            .iter()
            .take(self.hs.cds_len() as usize)
            .flat_map(|cds| cds.rlp.iter())
            .take_while(|rlp| rlp.is_some())
    }
    pub fn rlp_iter_mut(&mut self) -> impl Iterator<Item = &mut Rlp> {
        self.cds
            .iter_mut()
            .take(self.hs.cds_len() as usize)
            .flat_map(|cds| cds.rlp.iter_mut())
            .take_while(|rlp| rlp.is_some())
    }
    pub fn find_rlp(&self, channel: usize, pred: Pred) -> Option<&Rlp> {
        self.rlp_iter()
            .find(|rlp| !rlp.mrgc() && rlp.psrc() == pred.into() && rlp.cluster() == (channel >= 3))
    }
    pub fn find_rlp_mut(&mut self, channel: usize, pred: Pred) -> Option<&mut Rlp> {
        self.rlp_iter_mut()
            .find(|rlp| !rlp.mrgc() && rlp.psrc() == pred.into() && rlp.cluster() == (channel >= 3))
    }
    pub fn find_rpc(&self) -> Option<&Rlp> {
        self.rlp_iter().find(|rlp| rlp.is_rpc())
    }
}
