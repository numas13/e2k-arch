use crate::alc::instr::operand::dst::{DecodeError as DstDecodeError, Dst};
use crate::raw::operand::Operand;
use crate::raw::types::{Area, Index};
use crate::raw::{self, Unpacked};
use crate::state::reg::Size;
use core::convert::TryFrom;
use core::fmt;
use num_enum::{TryFromPrimitive, TryFromPrimitiveError};
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Invalid AAS kind {:#x}", source.number)]
    InvalidKind {
        #[from]
        source: TryFromPrimitiveError<Op>,
    },
    #[error("Invalid AAS dst")]
    InvalidDst {
        #[from]
        source: DstDecodeError,
    },
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum EncodeError {}

#[derive(Copy, Clone, Debug, PartialEq, TryFromPrimitive)]
#[repr(u8)]
pub enum Op {
    Movab = raw::Aas::OP_MOVAB,
    Movah = raw::Aas::OP_MOVAH,
    Movaw = raw::Aas::OP_MOVAW,
    Movad = raw::Aas::OP_MOVAD,
    Movaq = raw::Aas::OP_MOVAQ,
    Movaqp = raw::Aas::OP_MOVAQP,
}

impl Op {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Movab => "movab",
            Self::Movah => "movah",
            Self::Movaw => "movaw",
            Self::Movad => "movad",
            Self::Movaq => "movaq",
            Self::Movaqp => "movaqp",
        }
    }
    pub const fn dst_size(&self) -> Size {
        match self {
            Self::Movab => Size::B,
            Self::Movah => Size::H,
            Self::Movaw => Size::W,
            Self::Movad => Size::D,
            Self::Movaq => Size::Q,
            Self::Movaqp => Size::Q,
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.as_str())
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Instr {
    pub op: Op,
    pub area: Area,
    pub index: Index,
    pub am: bool,
    pub be: bool,
    pub dst: Dst,
}

impl TryFrom<(raw::Aas, u8)> for Instr {
    type Error = DecodeError;
    fn try_from(aas: (raw::Aas, u8)) -> Result<Self, Self::Error> {
        let (aas, dst) = aas;
        let kind = Op::try_from(aas.op())?;
        let dst = Dst::try_from(Operand(dst))?;
        Ok(Self {
            op: kind,
            area: Area::new_truncate(aas.area()),
            index: Index::new_truncate(aas.index()),
            am: aas.am(),
            be: aas.be(),
            dst,
        })
    }
}

impl Into<(raw::Aas, u8)> for Instr {
    fn into(self) -> (raw::Aas, u8) {
        let mut aas = raw::Aas::default();
        aas.set_op(self.op as u8);
        aas.set_area(self.area.get());
        aas.set_index(self.index.get());
        aas.set_am(self.am);
        aas.set_be(self.be);
        let dst: Operand = self.dst.into();
        (aas, dst.0)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Aau {
    pub channels: [Option<Instr>; 4],
}

impl Aau {
    pub fn unpack_from(raw: &Unpacked) -> Result<Self, DecodeError> {
        let mut channels = [None; 4];
        for i in 0..4 {
            if raw.ss.is_set_aas(i as u8) {
                channels[i] = Some(Instr::try_from((raw.aas[i], raw.aas_dst[i]))?);
            }
        }
        Ok(Self { channels })
    }
    pub fn pack_into(&self, raw: &mut Unpacked) -> Result<(), EncodeError> {
        for i in 0..4 {
            if let Some((aas, dst)) = self.channels[i].map(Into::into) {
                raw.ss.set_aas(i as u8);
                raw.aas[i] = aas;
                raw.aas_dst[i] = dst;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Aau {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.channels.iter().enumerate() {
            if let Some(instr) = instr {
                write!(
                    f,
                    "{},{} {}, index = {}, am = {}, be = {}, {}",
                    instr.op,
                    i,
                    instr.area,
                    instr.index.get(),
                    instr.am as u8,
                    instr.be as u8,
                    instr.dst.display(instr.op.dst_size()),
                )?;
            }
        }
        Ok(())
    }
}
