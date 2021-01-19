use crate::raw::{Packed, Unpacked};
use crate::{Aau, Alc, Cu, Plu};
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Failed to pre decode bundle")]
    PreDecode {
        #[from]
        source: crate::raw::PreDecodeError,
    },
    #[error("Failed to unpack bundle")]
    Unpack {
        #[from]
        source: crate::raw::DecodeError,
    },
    #[error("Failed to decode CU")]
    CuDecode {
        #[from]
        source: crate::cu::DecodeError,
    },
    #[error("Failed to decode ALC")]
    AlcDecode {
        #[from]
        source: crate::alc::DecodeError,
    },
    #[error("Failed to decode AAU")]
    AauDecode {
        #[from]
        source: crate::aau::DecodeError,
    },
    #[error("Failed to decode PLU")]
    PluDecode {
        #[from]
        source: crate::plu::DecodeError,
    },
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum EncodeError {
    #[error("Failed to encode CU")]
    CuEncode {
        #[from]
        source: crate::cu::EncodeError,
    },
    #[error("Failed to encode ALC")]
    AlcEncode {
        #[from]
        source: crate::alc::EncodeError,
    },
    #[error("Failed to encode AAU")]
    AauEncode {
        #[from]
        source: crate::aau::EncodeError,
    },
    #[error("Failed to encode PLU")]
    PluEncode {
        #[from]
        source: crate::plu::EncodeError,
    },
}

#[derive(Clone, Default, PartialEq)]
pub struct Bundle {
    pub cu: Cu,
    pub alc: Alc,
    pub aau: Aau,
    pub plu: Plu,
}

impl Bundle {
    pub fn from_bytes(version: u8, bytes: &[u8]) -> Result<(Self, &[u8]), DecodeError> {
        let (packed, tail) = Packed::from_bytes(bytes)?;
        let bundle = Bundle::from_packed(version, packed)?;
        Ok((bundle, tail))
    }
    pub fn from_packed(version: u8, packed: &Packed) -> Result<Self, DecodeError> {
        Self::from_unpacked(version, &Unpacked::unpack(packed)?)
    }
    pub fn from_unpacked(version: u8, bundle: &Unpacked) -> Result<Self, DecodeError> {
        let cu = Cu::unpack_from(bundle)?;
        let alc = Alc::unpack_from(version, bundle)?;
        let aau = Aau::unpack_from(bundle)?;
        let plu = Plu::from_slice(&bundle.pls[..bundle.hs.pls_len() as usize])?;
        Ok(Self { cu, alc, aau, plu })
    }
    pub fn into_raw(self, version: u8) -> Result<Unpacked, EncodeError> {
        let mut bundle = Unpacked::default();
        self.cu.pack_into(&mut bundle)?;
        self.alc.insert_into(version, &mut bundle)?;
        self.aau.pack_into(&mut bundle)?;
        if bundle.ss.0 != 0 {
            bundle.hs.set_ss(true);
        }
        // remove unneeded ALES2/5
        let e2 = bundle.ales[2].op();
        let e5 = bundle.ales[5].op();
        if (e2 == 0x00 || e2 == 0x01) && (e5 == 0x00 || e5 == 0x01) {
            bundle.ales[2].0 = 0;
            bundle.ales[5].0 = 0;
        }
        for (i, pls) in self.plu.into_raw()?.iter().enumerate() {
            if let Some(pls) = pls {
                bundle.hs.set_pls_len(i as u8 + 1);
                bundle.pls[i] = *pls;
            }
        }
        Ok(bundle)
    }
}

impl fmt::Display for Bundle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.cu.display_stubs(), f)?;
        fmt::Display::fmt(&self.alc, f)?;
        fmt::Display::fmt(&self.cu.display_controls(), f)?;
        fmt::Display::fmt(&self.aau, f)?;
        fmt::Display::fmt(&self.plu, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_bundle_build() {
        #[rustfmt::skip]
        let expected = &[
            0x12, 0x00, 0x40, 0x10,
            0x87, 0xd8, 0x36, 0x88,
            0xc0, 0x01, 0xe1, 0x0c,
            0xef, 0xbe, 0xad, 0xde,
        ];

        use crate::alc::instr::operand::{LitValue, Reg};
        use crate::alc::{instr, Channel};

        let mut bundle = Bundle::default();
        let instr = instr::fmul_adds(
            Reg::based_truncate(54),
            LitValue::l32_truncate(0, 0xdeadbeef),
            Reg::global_truncate(1),
            Reg::regular_truncate(7),
        );
        bundle.alc.channels[2] = Some(Channel::new(true, instr));

        let raw = bundle.into_raw(u8::MAX).unwrap();
        let mut buffer = [0u8; 64];
        let (packed, _) = raw.pack(&mut buffer).unwrap();
        assert_eq!(packed.as_slice(), expected);
    }

    macro_rules! test_dec_enc {
        ($(let $name:ident = $src:expr;)+) => {
            $(#[test]
            fn $name() {
                let src = $src;
                let (bundle, tail) = Bundle::from_bytes(u8::MAX, &src).unwrap();
                let raw = bundle.into_raw(u8::MAX).unwrap();
                let mut buffer = [0; 64];
                let (packed, _) = raw.pack(&mut buffer).unwrap();
                assert!(tail.is_empty());
                assert_eq!(src, packed.as_slice());
            })+
        };
    }

    test_dec_enc!(
        let hs_only = [
            // nop 3
            0x80, 0x01, 0x00, 0x00, // HS
            0x00, 0x00, 0x00, 0x00, // align
        ];
        let literal64 = [
            // ldd,2 %dr6, _f32s 0x20, %db[0]
            // movtd,3 _f64 0x6b5f0, %db[1]
            0x22, 0x00, 0x00, 0x30, // HS
            0x00, 0xda, 0x86, 0x67, // ALS2
            0x01, 0xdc, 0xc0, 0x61, // ALS3
            0x20, 0x00, 0x00, 0x00, // LTS2
            0x00, 0x00, 0x00, 0x00, // LTS1
            0xf0, 0xb5, 0x06, 0x00, // LTS0
        ];
        let lts_align = [
            // ct %ctpr1
            // ipd 3
            // adds,0 %r0, %r0, %r0 ? %pred0
            // adds,1 %r0, _f32s 0x11111111, %r0 ? %pred2
            // fadds,2 %r0, %r0, %r0 ? %pred1
            // adds,5 %r0, _f32s 0x55555555, %r0 ? %pred2
            // return %ctpr3
            // pass %pred0, @p0
            // pass %pred0, @p1
            // andp @p0, @p1, @p4
            // pass @p4, %pred0
            0x67, 0x50, 0x46, 0x9c, // HS
            0x20, 0x04, 0x00, 0xc0, // SS
            0x80, 0x80, 0x80, 0x10, // ALS0
            0x80, 0xd8, 0x80, 0x10, // ALS1
            0x80, 0x80, 0x80, 0x30, // ALS2
            0x80, 0xd9, 0x80, 0x10, // ALS5
            0x00, 0x00, 0x00, 0xf0, // CS0
            0xc0, 0x01, 0xc0, 0x02, // ALES2/5
            0x00, 0x00, 0x00, 0x00, // LTS2 (align)
            0x55, 0x55, 0x55, 0x55, // LTS1
            0x11, 0x11, 0x11, 0x11, // LTS0
            0x60, 0x00, 0x60, 0x60, // PLS0
            0x62, 0x50, 0x61, 0x10, // CDS1
            0x62, 0x08, 0x60, 0x04, // CDS0
        ];
        let ales25 = [
            // ct %ctpr1
            // adds,1 %r0, _f32s 0x11111111, %r0 ? %pred1
            // fadds,2 %r0, %r0, %r0 ? %pred2
            // adds,5 %r0, _f32s 0x55555555, %r0 ? %pred3
            // return %ctpr3
            // pass %pred0, @p0
            // pass %pred0, @p1
            // andp @p0, @p1, @p4
            // pass @p4, %pred0
            0x56, 0x50, 0x46, 0x98, // HS
            0x20, 0x04, 0x00, 0xc0, // SS
            0x80, 0xd8, 0x80, 0x10, // ALS0
            0x80, 0x80, 0x80, 0x30, // ALS2
            0x80, 0xd9, 0x80, 0x10, // ALS5
            0x00, 0x00, 0x00, 0xf0, // CS0
            0xc0, 0x01, 0xc0, 0x02, // ALES2/5
            0x55, 0x55, 0x55, 0x55, // LTS1
            0x11, 0x11, 0x11, 0x11, // LTS0
            0x60, 0x00, 0x60, 0x60, // PLS1
            0x00, 0x00, 0x63, 0x50, // PLS0
            0x62, 0x10, 0x61, 0x08, // CDS0
        ];
        let als25_ales5 = [
            // shrd,2 %dr0, %dr0, %dr0
            // fadds,5 %dr0, %dr0, %dr0, %dr0
            0x13, 0x00, 0x00, 0x92, //    HS
            0x80, 0x80, 0x80, 0x1b, //  ALS2
            0x80, 0x80, 0x80, 0x30, //  ALS5
            0xc0, 0x02, 0xc0, 0x01, // ALES2/5
        ];
        let als25_ales2 = [
            // fadds,2 %r0, %r0, %r0
            // ands,5 %r0, %r0, %r0
            0x13, 0x00, 0x40, 0x90, //    HS
            0x80, 0x80, 0x80, 0x30, //  ALS2
            0x80, 0x80, 0x80, 0x00, //  ALS5
            0xc0, 0x01, 0xc0, 0x02, // ALES2/5
        ];
        let lts_pls_cds = [
            // ct %ctpr1
            // adds,0 %r0, %r0, %r0 ? %pred0
            // fadds,2 %r0, %r0, %r0 ? %pred1
            // adds,5 %r0, _f32s 0xffffffff, %r0 ? %pred2
            // return %ctpr3
            // pass %pred0, @p2
            // andp @p4, @p2, @p5
            // pass @p5, %pred0
            // pass %pred0, @p0
            // pass %pred0, @p1
            // andp @p0, @p1, @p4
            0x56, 0x50, 0x4a, 0x94, // HS
            0x20, 0x04, 0x00, 0xc0, // SS
            0x80, 0x80, 0x80, 0x10, // ALS0
            0x80, 0x80, 0x80, 0x30, // ALS2
            0x80, 0xd8, 0x80, 0x10, // ALS5
            0x00, 0x00, 0x00, 0xf0, // CS0
            0xc0, 0x01, 0xc0, 0x02, // ALES2/5
            0xff, 0xff, 0xff, 0xff, // LTS0
            0xa0, 0x10, 0x00, 0x60, // PLS1
            0x40, 0x00, 0x60, 0x60, // PLS0
            0x00, 0x00, 0x62, 0x50, // CDS1
            0x61, 0x10, 0x60, 0x04, // CDS0
        ];
        let lts_staab = [
            // staab,2 %r0, %aad0 [ %aasti0 + _f32s,_lts0 0xeeee ]
            0x11, 0x00, 0x40, 0x10, // HS
            0x80, 0x01, 0x00, 0x1c, // ALS2
            0x00, 0x00, 0x00, 0x00, // LTS1 (align)
            0xee, 0xee, 0x00, 0x00, // LTS0
        ];
        let setwd_lts = [
            // adds,0 %r3, _f32s 0xffff, %r8
            // setwd wsz = 0x0, nfx = 0x0, dbl = 0x0
            0x22, 0x80, 0x00, 0x04, // HS
            0x88, 0xd9, 0x83, 0x10, // ALS0
            0x00, 0x00, 0x00, 0x00, // CS1
            0x00, 0x00, 0x00, 0x00, // LTS2 (align)
            0xff, 0xff, 0x00, 0x00, // LTS1
            0x00, 0x00, 0x00, 0x00, // LTS0
        ];
        let bundle_64_bytes = [
            // ipd 3
            // fmul_adds,0 %r0, _f32s 0xffffffff, %r1, %r0
            // fmul_adds,1 %r0, %r0, %r2, %r0
            // fmul_adds,2 %r0, %r0, %r3, %r0
            // fmul_adds,3 %r0, %r0, %r4, %r0
            // fmul_adds,4 %r0, %r0, %r5, %r0
            // fmul_adds,5 %r0, %r0, %r6, %r0
            // movab,0 area = 0, ind = 0, am = 0, be = 0, %r1
            // movah,1 area = 0, ind = 0, am = 0, be = 0, %r2
            // movaw,2 area = 0, ind = 0, am = 0, be = 0, %r3
            // movad,3 area = 0, ind = 0, am = 0, be = 0, %r4
            // pass %pred0, @p0
            // pass %pred0, @p1
            // andp @p0, @p1, @p4
            // pass @p4, %pred0
            0x78, 0x10, 0xf4, 0xff, // HS
            0x00, 0xf0, 0x00, 0xc0, // SS
            0x80, 0xd8, 0x80, 0x08, // ALS0
            0x80, 0x80, 0x80, 0x08, // ALS1
            0x80, 0x80, 0x80, 0x08, // ALS2
            0x80, 0x80, 0x80, 0x08, // ALS3
            0x80, 0x80, 0x80, 0x08, // ALS4
            0x80, 0x80, 0x80, 0x08, // ALS5
            0x86, 0x0c, 0x83, 0x0c, // ALES2/5
            0x82, 0x0c, 0x81, 0x0c, // ALES0/1
            0x85, 0x0c, 0x84, 0x0c, // ALES3/4
            0x84, 0x83, 0x82, 0x81, // AAS0/1
            0x00, 0x20, 0x00, 0x10, // AAS2/3
            0x00, 0x40, 0x00, 0x30, // AAS4/5
            0xff, 0xff, 0xff, 0xff, // LTS0
            0x60, 0x00, 0x60, 0x60, // PLS0
        ];
    );
}
