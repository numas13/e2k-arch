use crate::alc::instr::Desc;
use core::fmt;

/// The error type for decoding/encoding bundles.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Error {
    NeedMoreBytes(usize),
    UnexpectedEnd,
    BadFormat,
    BadVersion,
    Src2LitError,
    Src3Error,
    Src4Error,
    DstError,
    MrgcNotFound,
    OpcodeNotFound,
    TstError,
    StateReg,
    Unknown,
    UnknownExt,
    Version(Desc),
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NeedMoreBytes(n) => write!(fmt, "need {} bytes", n),
            Self::UnexpectedEnd => fmt.write_str("unexpected bundle end"),
            Self::BadFormat => fmt.write_str("bad format"),
            Self::BadVersion => fmt.write_str("bad version"),
            Self::Src2LitError => fmt.write_str("bad src2 lit index"),
            Self::Src3Error => fmt.write_str("bad src3"),
            Self::Src4Error => fmt.write_str("bad src4"),
            Self::DstError => fmt.write_str("bad dst"),
            Self::MrgcNotFound => fmt.write_str("MRGC not found"),
            Self::OpcodeNotFound => fmt.write_str("opcode not found"),
            Self::StateReg => fmt.write_str("bad state register"),
            Self::TstError => fmt.write_str("bad tst register"),
            Self::Unknown => fmt.write_str("unknown instruction"),
            Self::UnknownExt => fmt.write_str("unknown instruction extension"),
            Self::Version(_desc) => fmt.write_str("unsupported instruction on target version"),
        }
    }
}
