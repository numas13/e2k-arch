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
    use std::fs;

    #[test]
    fn bundle_pack_unpack() {
        let version = u8::MAX;
        for path in crate::TEST_BUNDLES_PATHS {
            let data = fs::read(path).unwrap();
            let (bundle, tail) = Bundle::from_bytes(version, data.as_slice()).unwrap();
            assert_eq!(tail, &[], "{}", path);
            let mut buffer = [0u8; 64];
            let (packed, _) = bundle.into_raw(version).unwrap().pack(&mut buffer).unwrap();
            assert_eq!(packed.as_slice(), data, "{}", path);
        }
    }

    #[test]
    fn simple_bundle_build() {
        let expected = &[
            0x12, 0x00, 0x40, 0x10, 0x87, 0xd8, 0x36, 0x08, 0x00, 0x00, 0xe1, 0x0c, 0xef, 0xbe,
            0xad, 0xde,
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
}
