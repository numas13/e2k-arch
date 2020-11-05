mod display;
pub mod operand;

use self::operand::*;
use crate::state::lit::LitValue;
use crate::state::reg::Reg;
use crate::state::reg::{Size, OP_SIZE_D, OP_SIZE_Q, OP_SIZE_U, OP_SIZE_X};
use crate::state::state_reg::StateReg;
use crate::{raw, InsertInto};
use core::convert::TryFrom;
use core::fmt;
use num_enum::{FromPrimitive, IntoPrimitive, TryFromPrimitive};
use thiserror::Error;
use Ext::*;
use Index::*;

#[derive(Debug, Error)]
#[error(
    "Instruction {name:?} is available in channel {channel} since version {expected} but the target cpu version is {version}"
)]
pub struct NotAvailableError {
    pub name: &'static str,
    pub channel: usize,
    pub version: u8,
    pub expected: u8,
}

#[derive(Debug, Error)]
#[error("Instruction {op:02x}:{ext:02x}:{src1:02x} was not found")]
pub struct NotFoundError {
    pub op: u8,
    pub ext: u8,
    pub src1: u8,
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Unknown instruction extension {0:#02x}")]
    UnknownExt(u8),
    #[error("Instruction not available")]
    NotAvailable {
        #[from]
        source: NotAvailableError,
    },
    #[error("Instruction not found")]
    NotFound {
        #[from]
        source: NotFoundError,
    },
    #[error("Failed to decode address")]
    InvalidAddr {
        #[from]
        source: self::operand::src2::DecodeError,
    },
    #[error("Failed to decode dst")]
    InvalidDst {
        #[from]
        source: self::operand::dst::DecodeError,
    },
    #[error("Failed to decode dst state register")]
    InvalidDstState,
    #[error("Merge condition not found")]
    InvalidMergeCond {
        #[from]
        source: self::operand::merge_cond::NotFoundError,
    },
    #[error("Failed to decode src2")]
    InvalidSrc2 {
        source: self::operand::src2::DecodeError,
    },
    #[error("Failed to decode src3")]
    InvalidSrc3 {
        source: crate::state::reg::DecodeError,
    },
    #[error("Failed to decode src4")]
    InvalidSrc4 {
        source: crate::state::reg::DecodeError,
    },
    #[error("Failed to decode state register")]
    InvalidSrcState,
    #[error("Failed to decode array address")]
    InvalidArrayAddr {
        #[from]
        source: self::operand::addr_array::DecodeError,
    },
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum EncodeError {
    #[error("Failed to encode unavailable instruction")]
    NotAvailable {
        #[from]
        source: NotAvailableError,
    },
    #[error("Failed to encode missing instruction")]
    NotFound {
        #[from]
        source: NotFoundError,
    },
}

#[derive(Copy, Clone)]
struct OperandSizes(pub u8);

impl OperandSizes {
    fn next(&mut self) -> Size {
        let size = Size::new_dxq(self.0);
        self.0 >>= 2;
        size
    }
}

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, FromPrimitive)]
#[repr(u8)]
pub enum Index {
    C0,
    C1,
    C2,
    C3,
    C4,
    #[num_enum(default)]
    C5,
}

#[derive(Copy, Clone, PartialEq)]
pub struct RawInstr {
    pub version: u8,
    pub channel: Index,
    pub als: raw::Als,
    pub ales: raw::Ales,
    pub mrgc: Option<MergeCond>,
    pub lit: Option<LitValue>,
}

impl RawInstr {
    pub fn new(
        version: u8,
        channel: usize,
        als: raw::Als,
        ales: raw::Ales,
        mc: Option<MergeCond>,
        lit: Option<LitValue>,
    ) -> Self {
        Self {
            version,
            channel: Index::from_primitive(channel as u8),
            als,
            ales,
            mrgc: mc,
            lit,
        }
    }
    fn src1(&self) -> u8 {
        self.als.raw_src1()
    }
    fn cmp_op(&self) -> u8 {
        self.als.cmp_op()
    }
}

impl Default for RawInstr {
    fn default() -> Self {
        let mut als = raw::Als::default();
        als.set_raw_src1(0xc0);
        let mut ales = raw::Ales::default();
        ales.set_raw_src3(0xc0);
        Self {
            channel: Index::C0,
            version: 1,
            als,
            ales,
            mrgc: None,
            lit: None,
        }
    }
}

impl core::fmt::Debug for RawInstr {
    fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
        fmt.debug_struct("RawInstr")
            .field("channel", &(self.channel as u8))
            .field("version", &self.version)
            .field("als", &self.als.0)
            .field("ales", &self.ales.0)
            .field("merge_condition", &self.mrgc)
            .finish()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Ext {
    Ex0,
    Ex1,
    Ex2,
    Ex4 = 0x04,
    Ex5,
    Ex6,
    Ex7,
    Ex8,
    Ex9,
    Exa,
    Exb,
    Exc,
    Exd,
    Exe,
    Exf,
}

macro_rules! pattern {
    ($opcode:literal, $ext:path) => {
        ($opcode, $ext)
    };
    ($opcode:literal) => {
        ($opcode, Ext::Ex0)
    };
}

macro_rules! ver {
    () => {
        0
    };
    ($version:literal) => {
        $version
    };
}

macro_rules! operand_sizes {
    ($s0:ident, $s1:ident, $s2:ident, $s3:ident) => (
          operand_sizes!(@s $s0)
        | operand_sizes!(@s $s1) << 2
        | operand_sizes!(@s $s2) << 4
        | operand_sizes!(@s $s3) << 6
    );
    ($s0:ident, $s1:ident, $s2:ident) => (
          operand_sizes!(@s $s0)
        | operand_sizes!(@s $s1) << 2
        | operand_sizes!(@s $s2) << 4
    );
    ($s0:ident, $s1:ident) => (
          operand_sizes!(@s $s0)
        | operand_sizes!(@s $s1) << 2
    );
    ($s0:ident) => (
          operand_sizes!(@s $s0)
    );
    () => (0);
    (@s q) => (OP_SIZE_Q);
    (@s x) => (OP_SIZE_X);
    (@s d) => (OP_SIZE_D);
    (@s $s:ident) => (OP_SIZE_U);
}

#[derive(Copy, Clone, Default, Debug)]
pub struct Opcode {
    pub version: u8,
    pub op: u8,
    pub ext: Option<Ext>,
    pub src1: Option<u8>,
    pub cmp_op: Option<u8>,
}

macro_rules! decl {
    (@func $kind:ident :: $instr:ident($($arg:ident: $ty:ty),+)) => {
        pub fn $instr($($arg: impl Into<$ty>),+) -> Instr {
            Instr::$kind($kind::$instr, $($arg.into()),+)
        }
    };
    (@opcode $version:ident, $opcode:literal, $($field:ident = $value:expr),* $(,)?) => {
        Opcode {
            version: $version,
            op: $opcode,
            $($field: Some($value),)*
            .. Opcode::default()
        }
    };
    ($( $kind:ident $args:tt {
        $( $instr:ident $( ( $($arg_size:ident),+ ) )? {
            $( $opcode:literal
                $( ($ext:path) )?
                $( [$($channel:pat),+] )?
                $( v:$version:literal )?
                $( if $( $field:ident == $value:expr ),+ )?
            ),+
        } )+
    } )+) => {
        $( #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, PartialEq)]
        pub enum $kind {
            $($instr),+
        }

        impl fmt::Debug for $kind {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                fmt.write_str(self.as_str())
            }
        }

        impl $kind {
            #[allow(unused_variables)]
            pub fn into_opcode(self, version: u8, channel: usize) -> Result<Option<Opcode>, EncodeError> {
                let chan = Index::from(channel as u8);
                let (opcode, name) = match self {
                    $( $( $kind::$instr if true $(&& matches!(chan, $($channel)|+))? => {
                        let version = ver!($($version)?);
                        (
                            decl!(@opcode
                                version,
                                $opcode,
                                $(ext = $ext,)?
                                $($($field = $value),+)?
                            ),
                            stringify!($instr),
                        )
                    }, )+ )+
                    _ => return Ok(None),
                };
                if opcode.version <= version {
                    Ok(Some(opcode))
                } else {
                    Err(EncodeError::NotAvailable {
                        source: NotAvailableError { version, channel, expected: opcode.version, name }
                    })
                }
            }
            pub fn as_str(&self) -> &'static str {
                match self {
                    $( Self::$instr => stringify!($instr), )+
                }
            }
            pub fn from_str(s: &str) -> Option<Self> {
                let ret = match s {
                    $( stringify!($instr) => Self::$instr, )+
                    _ => return None,
                };
                Some(ret)
            }
            fn operand_sizes(&self) -> OperandSizes {
                let value = match self {
                    $( Self::$instr => operand_sizes!( $( $($arg_size),+ )? ), )+
                };
                OperandSizes(value)
            }
        } )+

        $( $( decl!(@func $kind::$instr $args); )+ )+

        impl Desc {
            pub fn new(raw: &RawInstr) -> Result<Option<(u8, Desc)>, DecodeError> {
                let ext = Ext::try_from(raw.ales.op())
                    .map_err(|_| DecodeError::UnknownExt(raw.ales.op()))?;
                let desc = match (raw.als.op(), ext) {
                    $( $( $( pattern!($opcode $(,$ext)?) if true
                            $( && matches!(raw.channel, $($channel)|+) )?
                            $( $( && raw.$field() == $value)+ )?
                        => (ver!( $($version)? ), Desc::$kind($kind::$instr)),
                    )+ )+ )+
                    _ => return Ok(None),
                };
                Ok(Some(desc))
            }
        }
    };
}

#[derive(Copy, Clone, Debug)]
pub enum Desc {
    Op2(Op2),
    Op3(Op3),
    Op4(Op4),
    Op2cmp(Op2cmp),
    Op3cmp(Op3cmp),
    Op3mrgc(Op3mrgc),
    Op4mrgc(Op4mrgc),
    Op3imm8(Op3imm8),
    Op4imm8(Op4imm8),
    Op3load(Op3load),
    Op3store(Op3store),
    Op2rw(Op2rw),
    Op2rr(Op2rr),
    OpAload(OpAload),
    OpAstore(OpAstore),
}

impl Desc {
    pub fn as_str(&self) -> &'static str {
        match self {
            Desc::Op2(op) => op.as_str(),
            Desc::Op3(op) => op.as_str(),
            Desc::Op4(op) => op.as_str(),
            Desc::Op2cmp(op) => op.as_str(),
            Desc::Op3cmp(op) => op.as_str(),
            Desc::Op3mrgc(op) => op.as_str(),
            Desc::Op4mrgc(op) => op.as_str(),
            Desc::Op3imm8(op) => op.as_str(),
            Desc::Op4imm8(op) => op.as_str(),
            Desc::Op3load(op) => op.as_str(),
            Desc::Op3store(op) => op.as_str(),
            Desc::Op2rw(op) => op.as_str(),
            Desc::Op2rr(op) => op.as_str(),
            Desc::OpAload(op) => op.as_str(),
            Desc::OpAstore(op) => op.as_str(),
        }
    }
    pub fn from_str(s: &str) -> Option<Self> {
        macro_rules! find_op {
            ($($i:ident),+) => {
                $( if let Some(op) = $i::from_str(s) {
                    return Some(Self::$i(op));
                } )+
            };
        }
        find_op!(
            Op2, Op3, Op4, Op2cmp, Op3cmp, Op3mrgc, Op4mrgc, Op3imm8, Op4imm8, Op3load, Op3store,
            Op2rw, Op2rr, OpAload, OpAstore
        );
        None
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Instr {
    Op2(Op2, Src2, Dst),
    Op3(Op3, Src1, Src2, Dst),
    Op4(Op4, Src1, Src2, Src3, Dst),
    Op2cmp(Op2cmp, Src2, DstPreg),
    Op3cmp(Op3cmp, Src1, Src2, DstPreg),
    Op3mrgc(Op3mrgc, Src1, Src2, Dst, MergeCond),
    Op4mrgc(Op4mrgc, Src1, Src2, Src3, Dst, MergeCond),
    Op3imm8(Op3imm8, Src2, Imm8, Dst),
    Op4imm8(Op4imm8, Src1, Src2, Imm8, Dst),
    Op3load(Op3load, Addr, Dst),
    Op3store(Op3store, Src4, Addr),
    Op2rw(Op2rw, Src2, DstState),
    Op2rr(Op2rr, SrcState, Dst),
    OpAload(OpAload, AddrArray, Dst),
    OpAstore(OpAstore, Src4, AddrArray),
}

impl Instr {
    pub fn display<'a>(&'a self, channel: u8, sm: bool) -> impl fmt::Display + 'a {
        self::display::Display::new(self, channel, sm)
    }
    pub fn into_raw(self, version: u8, channel: usize) -> Result<RawInstr, EncodeError> {
        let mut raw = RawInstr::default();
        let opcode = match self {
            Self::Op2(op, src2, dst) => {
                dst.insert_into(&mut raw);
                src2.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op3(op, src1, src2, dst) => {
                dst.insert_into(&mut raw);
                src2.insert_into(&mut raw);
                src1.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op4(op, src1, src2, src3, dst) => {
                dst.insert_into(&mut raw);
                raw.ales.set_src3(src3.into());
                src2.insert_into(&mut raw);
                src1.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op2cmp(op, src2, dst) => {
                raw.als.set_cmp_dst(dst);
                src2.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op3cmp(op, src1, src2, dst) => {
                raw.als.set_cmp_dst(dst);
                src2.insert_into(&mut raw);
                src1.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op3mrgc(op, src1, src2, dst, mrgc) => {
                mrgc.insert_into(&mut raw);
                dst.insert_into(&mut raw);
                src2.insert_into(&mut raw);
                src1.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op4mrgc(op, src1, src2, src3, dst, mrgc) => {
                mrgc.insert_into(&mut raw);
                dst.insert_into(&mut raw);
                raw.ales.set_src3(src3.into());
                src2.insert_into(&mut raw);
                src1.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op3imm8(op, src2, imm8, dst) => {
                dst.insert_into(&mut raw);
                raw.ales.set_raw_src3(imm8);
                src2.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op4imm8(op, src1, src2, imm8, dst) => {
                dst.insert_into(&mut raw);
                raw.ales.set_raw_src3(imm8);
                src2.insert_into(&mut raw);
                src1.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op3load(op, addr, dst) => {
                dst.insert_into(&mut raw);
                addr.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op3store(op, src4, addr) => {
                addr.insert_into(&mut raw);
                raw.als.set_src4(src4.into());
                op.into_opcode(version, channel)
            }
            Self::Op2rw(op, src2, dst) => {
                raw.als.set_raw_dst(dst as u8);
                src2.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::Op2rr(op, src, dst) => {
                dst.insert_into(&mut raw);
                raw.als.set_raw_src1(src as u8);
                op.into_opcode(version, channel)
            }
            Self::OpAload(op, addr, dst) => {
                dst.insert_into(&mut raw);
                addr.insert_into(&mut raw);
                op.into_opcode(version, channel)
            }
            Self::OpAstore(op, src4, addr) => {
                addr.insert_into(&mut raw);
                raw.als.set_src4(src4.into());
                op.into_opcode(version, channel)
            }
        };
        let opcode = opcode?.ok_or_else(|| EncodeError::NotFound {
            source: NotFoundError {
                op: raw.als.op(),
                ext: raw.ales.op(),
                src1: raw.als.raw_src1(),
            },
        })?;
        raw.als.set_op(opcode.op);
        if let Some(ext) = opcode.ext {
            raw.ales.set_op(ext as u8);
        }
        if let Some(src1) = opcode.src1 {
            raw.als.set_raw_src1(src1);
        }
        if let Some(cmp_op) = opcode.cmp_op {
            raw.als.set_cmp_op(cmp_op);
        }
        Ok(raw)
    }
    pub fn new(raw: &RawInstr, lts: &[Option<u32>; 4]) -> Result<Self, DecodeError> {
        let (ver, desc) = Desc::new(raw)?.ok_or(DecodeError::NotFound {
            source: NotFoundError {
                op: raw.als.op(),
                ext: raw.ales.op(),
                src1: raw.als.raw_src1(),
            },
        })?;
        if raw.version < ver {
            return Err(DecodeError::NotAvailable {
                source: NotAvailableError {
                    version: raw.version,
                    channel: raw.channel as usize,
                    expected: ver,
                    name: desc.as_str(),
                },
            });
        }
        let src1 = || Src1::from(raw.als.src1());
        let src2 = || Src2::new(raw.als.src2(), lts);
        let src3 =
            || Reg::try_from(raw.ales.src3()).map_err(|e| DecodeError::InvalidSrc3 { source: e });
        let src4 =
            || Reg::try_from(raw.als.src4()).map_err(|e| DecodeError::InvalidSrc4 { source: e });
        let addr = || Addr::new(raw, lts).map_err(|e| DecodeError::InvalidAddr { source: e });
        let imm8 = || raw.ales.src3().0;
        let mrgc = || MergeCond::try_from(raw);
        let dst = || Dst::try_from(raw.als.dst());
        let dst_preg = || raw.als.cmp_dst();
        let addr_array = || AddrArray::new(raw, lts);
        let instr = match desc {
            Desc::Op2(op) => Self::Op2(op, src2()?, dst()?),
            Desc::Op3(op) => Self::Op3(op, src1(), src2()?, dst()?),
            Desc::Op4(op) => Self::Op4(op, src1(), src2()?, src3()?, dst()?),
            Desc::Op2cmp(op) => Self::Op2cmp(op, src2()?, dst_preg()),
            Desc::Op3cmp(op) => Self::Op3cmp(op, src1(), src2()?, dst_preg()),
            Desc::Op3mrgc(op) => Self::Op3mrgc(op, src1(), src2()?, dst()?, mrgc()?),
            Desc::Op4mrgc(op) => Self::Op4mrgc(op, src1(), src2()?, src3()?, dst()?, mrgc()?),
            Desc::Op3imm8(op) => Self::Op3imm8(op, src2()?, imm8(), dst()?),
            Desc::Op4imm8(op) => Self::Op4imm8(op, src1(), src2()?, imm8(), dst()?),
            Desc::Op3load(op) => Self::Op3load(op, addr()?, dst()?),
            Desc::Op3store(op) => Self::Op3store(op, src4()?, addr()?),
            Desc::Op2rw(op) => {
                let dst = StateReg::try_from(raw.als.raw_dst())
                    .map_err(|_| DecodeError::InvalidDstState)?;
                Self::Op2rw(op, src2()?, dst)
            }
            Desc::Op2rr(op) => {
                let src = StateReg::try_from(raw.als.raw_src1())
                    .map_err(|_| DecodeError::InvalidSrcState)?;
                Self::Op2rr(op, src, dst()?)
            }
            Desc::OpAload(op) => Self::OpAload(op, addr_array()?, dst()?),
            Desc::OpAstore(op) => Self::OpAstore(op, src4()?, addr_array()?),
        };
        Ok(instr)
    }
}

decl! {
    Op2(src2: Src2, dst: Dst) {
        bitrevs(w, w) { 0x26[C0,C1,C3,C4] v:2 }
        bitrevd(d, d) { 0x27[C0,C1,C3,C4] v:2 }
        fstois(w, w) { 0x3c[C0,C1,C3,C4] }
        fstoistr(w, w) { 0x3c[C0,C1,C3,C4] if src1 == 0xc2 }
        fdtoid(d, d) { 0x3d[C0,C1,C3,C4] }
        fdtoidtr(d, d) { 0x3d[C0,C1,C3,C4] if src1 == 0xc2 }
        fstoid(w, d) { 0x3e[C0,C1,C3,C4] }
        fstoidtr(w, d) { 0x3e[C0,C1,C3,C4] if src1 == 0xc2 }
        fdtois(d, w) { 0x3f[C0,C1,C3,C4] }
        fdtoistr(d, w) { 0x3f[C0,C1,C3,C4] if src1 == 0xc2 }
        movfi(d, w) { 0x5c[C1,C4] }
        movx(d, d) { 0x5f[C0,C1,C3,C4] v:2 }
        movts(w, w) { 0x60[C0,C1,C3,C4] }
        movtd(d, d) { 0x61[C0,C1,C3,C4] } // ctpr
        getpl(w, d) { 0x63[C0] if src1 == 0xf0 } // ctpr
        fxsqrtisx(w, x) { 0x52[C5] }
        fxsqrtidx(d, x) { 0x53[C5] }
        fxsqrtixx(x, x) { 0x57[C5] }
        lzcnts(w, w) { 0x64[C1,C4] v:2 }
        lzcntd(d, d) { 0x65[C1,C4] v:2 }
        popcnts(w, w) { 0x66[C1,C4] v:2 }
        popcntd(d, d) { 0x67[C1,C4] v:2 }
        gettags(w, w) { 0x08(Ex1)[C2] }
        gettagd(d, d) { 0x09(Ex1)[C2] }
        gettc(w, d) { 0x24(Ex1)[C0] } // tc
        gettst(w, w) { 0x24(Ex1)[C3] }
        puttst(d, w) { 0x25(Ex1)[C3] }
        invtc(w, d) { 0x26(Ex1)[C0] } // tc
        fsqrts(w, w) { 0x4c(Ex1)[C5] }
        fsqrtid(d, d) { 0x4d(Ex1)[C5] }
        frcps(w, w) { 0x50(Ex1)[C5] }
        frsqrts(w, w) { 0x54(Ex1)[C5] }
        movtq(q, q) { 0x57(Ex1)[C0,C3] } // pair
        getsp(w, d) { 0x58(Ex1)[C0,C3] if src1 == 0xec }
    }
    Op3(src1: Src1, src2: Src2, dst: Dst) {
        ands(w, w, w) { 0x00[C0,C1,C2,C3,C4,C5] }
        andd(d, d, d) { 0x01[C0,C1,C2,C3,C4,C5] }
        andns(w, w, w) { 0x02[C0,C1,C2,C3,C4,C5] }
        andnd(d, d, d) { 0x03[C0,C1,C2,C3,C4,C5] }
        ors(w, w, w) { 0x04[C0,C1,C2,C3,C4,C5] }
        ord(d, d, d) { 0x05[C0,C1,C2,C3,C4,C5] }
        orns(w, w, w) { 0x06[C0,C1,C2,C3,C4,C5] }
        ornd(d, d, d) { 0x07[C0,C1,C2,C3,C4,C5] }
        xors(w, w, w) { 0x08[C0,C1,C2,C3,C4,C5] }
        xord(d, d, d) { 0x09[C0,C1,C2,C3,C4,C5] }
        xorns(w, w, w) { 0x0a[C0,C1,C2,C3,C4,C5] }
        xornd(d, d, d) { 0x0b[C0,C1,C2,C3,C4,C5] }
        sxt(w, w, d) { 0x0c[C0,C1,C2,C3,C4,C5] }
        adds(w, w, w) { 0x10[C0,C1,C2,C3,C4,C5] }
        addd(d, d, d) { 0x11[C0,C1,C2,C3,C4,C5] }
        subs(w, w, w) { 0x12[C0,C1,C2,C3,C4,C5] }
        subd(d, d, d) { 0x13[C0,C1,C2,C3,C4,C5] }
        scls(w, w, w) { 0x14[C0,C1,C2,C3,C4,C5] }
        scld(d, d, d) { 0x15[C0,C1,C2,C3,C4,C5] }
        scrs(w, w, w) { 0x16[C0,C1,C2,C3,C4,C5] }
        scrd(d, d, d) { 0x17[C0,C1,C2,C3,C4,C5] }
        shls(w, w, w) { 0x18[C0,C1,C2,C3,C4,C5] }
        shld(d, d, d) { 0x19[C0,C1,C2,C3,C4,C5] }
        shrs(w, w, w) { 0x1a[C0,C1,C2,C3,C4,C5] }
        shrd(d, d, d) { 0x1b[C0,C1,C2,C3,C4,C5] }
        sars(w, w, w) { 0x1c[C0,C1,C2,C3,C4,C5] }
        sard(d, d, d) { 0x1d[C0,C1,C2,C3,C4,C5] }
        getfs(w, w, w) { 0x1e[C0,C1,C2,C3,C4,C5] }
        getfd(d, d, d) { 0x1f[C0,C1,C2,C3,C4,C5] }
        fadds(w, w, w) { 0x30[C0,C1,C3,C4], 0x30(Ex2)[C2,C5] v:4 }
        faddd(d, d, d) { 0x31[C0,C1,C3,C4], 0x31(Ex2)[C2,C5] v:4 }
        fsubs(w, w, w) { 0x32[C0,C1,C3,C4], 0x32(Ex2)[C2,C5] v:4 }
        fsubd(d, d, d) { 0x33[C0,C1,C3,C4], 0x33(Ex2)[C2,C5] v:4 }
        fmins(w, w, w) { 0x34[C0,C1,C3,C4] }
        fmind(d, d, d) { 0x35[C0,C1,C3,C4] }
        fmaxs(w, w, w) { 0x36[C0,C1,C3,C4] }
        fmaxd(d, d, d) { 0x37[C0,C1,C3,C4] }
        fmuls(w, w, w) { 0x38[C0,C1,C3,C4], 0x38(Ex2)[C2,C5] v:4 }
        fmuld(d, d, d) { 0x39[C0,C1,C3,C4], 0x39(Ex2)[C2,C5] v:4 }
        fxaddss(x, w, w) { 0x40[C0,C1,C3,C4] }
        fxadddd(x, d, d) { 0x41[C0,C1,C3,C4] }
        fxaddsx(x, w, x) { 0x42[C0,C1,C3,C4] }
        fxadddx(x, d, x) { 0x43[C0,C1,C3,C4] }
        fxaddxs(x, x, w) { 0x44[C0,C1,C3,C4] }
        fxaddxd(x, x, d) { 0x45[C0,C1,C3,C4] }
        fxaddxx(x, x, x) { 0x47[C0,C1,C3,C4] }
        fxsubss(x, w, w) { 0x48[C0,C1,C3,C4] }
        fxsubdd(x, d, d) { 0x49[C0,C1,C3,C4] }
        fxsubsx(x, w, x) { 0x4a[C0,C1,C3,C4] }
        fxsubdx(x, d, x) { 0x4b[C0,C1,C3,C4] }
        fxsubxs(x, x, w) { 0x4c[C0,C1,C3,C4] }
        fxsubxd(x, x, d) { 0x4d[C0,C1,C3,C4] }
        fxsubxx(x, x, x) { 0x4f[C0,C1,C3,C4] }
        fxrsubss(x, w, w) { 0x58[C0,C1,C3,C4] }
        fxrsubdd(x, d, d) { 0x59[C0,C1,C3,C4] }
        fxrsubsx(x, w, x) { 0x5a[C0,C1,C3,C4] }
        fxrsubdx(x, d, x) { 0x5b[C0,C1,C3,C4] }
        fxmulss(x, w, w) { 0x50[C0,C1,C3,C4] }
        fxmuldd(x, d, d) { 0x51[C0,C1,C3,C4] }
        fxmulsx(x, w, x) { 0x52[C0,C1,C3,C4] }
        fxmuldx(x, d, x) { 0x53[C0,C1,C3,C4] }
        fxmulxs(x, x, w) { 0x54[C0,C1,C3,C4] }
        fxmulxd(x, x, d) { 0x55[C0,C1,C3,C4] }
        fxmulxx(x, x, x) { 0x57[C0,C1,C3,C4] }
        movif(d, w, d) { 0x5e[C1,C4] }
        vfsi(w, d, d) { 0x63[C1,C4] }
        udivs(w, w, w) { 0x40[C5] }
        udivd(d, d, d) { 0x41[C5] }
        sdivs(w, w, w) { 0x42[C5] }
        sdivd(d, d, d) { 0x43[C5] }
        udivx(d, w, w) { 0x44[C5] }
        umodx(d, w, w) { 0x45[C5] }
        sdivx(d, w, w) { 0x46[C5] }
        smodx(d, w, w) { 0x47[C5] }
        fxdivss(x, w, w) { 0x48[C5] }
        fxdivdd(x, d, d) { 0x49[C5] }
        fxdivsx(x, w, x) { 0x4a[C5] }
        fxdivdx(x, d, x) { 0x4b[C5] }
        fxdivxs(x, x, w) { 0x4c[C5] }
        fxdivxd(x, x, d) { 0x4d[C5] }
        fxdivxx(x, x, x) { 0x4f[C5] }
        fxsqrtuxx(x, x, x) { 0x59[C5] }
        fxsqrtusx(x, w, x) { 0x5a[C5] }
        fxsqrtudx(x, d, x) { 0x5b[C5] }
        fxsqrttxx(x, x, x) { 0x5d[C5] }
        fxsqrttsx(x, w, x) { 0x5e[C5] }
        fxsqrttdx(x, d, x) { 0x5f[C5] }
        fxdivtss(x, w, w) { 0x60[C5] }
        fxdivtdd(x, d, d) { 0x61[C5] }
        fxdivtsx(x, w, x) { 0x62[C5] }
        fxdivtdx(x, d, x) { 0x63[C5] }
        puttags(w, w, w) { 0x0a(Ex1)[C2] }
        puttagd(d, d, d) { 0x0b(Ex1)[C2] }
        pminub(d, d, d) { 0x00(Ex1)[C0,C3] }
        pminsh(d, d, d) { 0x01(Ex1)[C0,C3] }
        pmaxub(d, d, d) { 0x02(Ex1)[C0,C3] }
        pmaxsh(d, d, d) { 0x03(Ex1)[C0,C3] }
        pminsb(d, d, d) { 0x04(Ex1)[C0,C3] v:3 }
        pminuh(d, d, d) { 0x05(Ex1)[C0,C3] v:3 }
        pmaxsb(d, d, d) { 0x06(Ex1)[C0,C3] v:3 }
        pmaxuh(d, d, d) { 0x07(Ex1)[C0,C3] v:3 }
        paddb(d, d, d) { 0x08(Ex1)[C0,C3] }
        paddh(d, d, d) { 0x09(Ex1)[C0,C3] }
        paddsb(d, d, d) { 0x0a(Ex1)[C0,C3] }
        paddsh(d, d, d) { 0x0b(Ex1)[C0,C3] }
        paddusb(d, d, d) { 0x0c(Ex1)[C0,C3] }
        paddush(d, d, d) { 0x0d(Ex1)[C0,C3] }
        paddw(d, d, d) { 0x0e(Ex1)[C0,C3] }
        paddd(d, d, d) { 0x0f(Ex1)[C0,C3] }
        psubb(d, d, d) { 0x10(Ex1)[C0,C3] }
        psubh(d, d, d) { 0x11(Ex1)[C0,C3] }
        psubsb(d, d, d) { 0x12(Ex1)[C0,C3] }
        psubsh(d, d, d) { 0x13(Ex1)[C0,C3] }
        psubusb(d, d, d) { 0x14(Ex1)[C0,C3] }
        psubush(d, d, d) { 0x15(Ex1)[C0,C3] }
        psubw(d, d, d) { 0x16(Ex1)[C0,C3] }
        psubd(d, d, d) { 0x17(Ex1)[C0,C3] }
        pcmpeqb(d, d, d) { 0x18(Ex1)[C0,C3] }
        pcmpeqh(d, d, d) { 0x19(Ex1)[C0,C3] }
        pcmpeqw(d, d, d) { 0x1a(Ex1)[C0,C3] }
        pcmpgtb(d, d, d) { 0x1b(Ex1)[C0,C3] }
        pcmpgth(d, d, d) { 0x1c(Ex1)[C0,C3] }
        pcmpgtw(d, d, d) { 0x1d(Ex1)[C0,C3] }
        pavgusb(d, d, d) { 0x1e(Ex1)[C0,C3] }
        pavgush(d, d, d) { 0x1f(Ex1)[C0,C3] }
        punpckhbh(d, d, d) { 0x00(Ex1)[C1,C4] }
        punpcklbh(d, d, d) { 0x01(Ex1)[C1,C4] }
        punpckhhw(d, d, d) { 0x02(Ex1)[C1,C4] }
        punpcklhw(d, d, d) { 0x03(Ex1)[C1,C4] }
        punpckhwd(d, d, d) { 0x04(Ex1)[C1,C4] }
        punpcklwd(d, d, d) { 0x05(Ex1)[C1,C4] }
        pmovmskps(d, d, d) { 0x06(Ex1)[C1,C4] }
        pmovmskpd(d, d, d) { 0x07(Ex1)[C1,C4] }
        pmovmskb(d, d, d) { 0x0b(Ex1)[C1,C4] }
        packsshb(d, d, d) { 0x08(Ex1)[C1,C4] }
        packushb(d, d, d) { 0x09(Ex1)[C1,C4] }
        packsswh(d, d, d) { 0x0a(Ex1)[C1,C4] }
        psrlw(d, d, d) { 0x10(Ex1)[C1,C4], 0x10(Ex2)[C0,C3] v:3 }
        psrlh(d, d, d) { 0x11(Ex1)[C1,C4], 0x11(Ex2)[C0,C3] v:3 }
        psraw(d, d, d) { 0x12(Ex1)[C1,C4], 0x12(Ex2)[C0,C3] v:3 }
        psrah(d, d, d) { 0x13(Ex1)[C1,C4], 0x13(Ex2)[C0,C3] v:3 }
        psllw(d, d, d) { 0x14(Ex1)[C1,C4], 0x14(Ex2)[C0,C3] v:3 }
        psllh(d, d, d) { 0x15(Ex1)[C1,C4], 0x15(Ex2)[C0,C3] v:3 }
        pmulhh(d, d, d) { 0x18(Ex1)[C1,C4] }
        pmullh(d, d, d) { 0x19(Ex1)[C1,C4] }
        pmaddh(d, d, d) { 0x1a(Ex1)[C1,C4] }
        pmulhuh(d, d, d) { 0x1b(Ex1)[C1,C4] }
        strd(d, d, d) { 0x1b(Ex1)[C2,C5] }
        psadbw(d, d, d) { 0x1c(Ex1)[C1,C4] }
        pmulubhh(d, d, d) { 0x1d(Ex1)[C1,C4] v:2 }
        muls(w, w, w) { 0x20(Ex1)[C0,C1,C3,C4] }
        muld(d, d, d) { 0x21(Ex1)[C0,C1,C3,C4] }
        umulx(w, w, d) { 0x22(Ex1)[C0,C1,C3,C4] }
        smulx(w, w, d) { 0x23(Ex1)[C0,C1,C3,C4] }
        puttc(d, d, d) { 0x25(Ex1)[C0] }
        fscales(w, w, w) { 0x24(Ex1)[C1,C4] v:4 }
        fscaled(d, w, d) { 0x25(Ex1)[C1,C4] v:4 }
        pfadds(d, d, d) { 0x30(Ex1)[C0,C1,C3,C4], 0x30(Ex1)[C2,C5] v:4 }
        pfaddd(d, d, d) { 0x31(Ex1)[C0,C1,C3,C4], 0x31(Ex1)[C2,C5] v:4 }
        pfsubs(d, d, d) { 0x32(Ex1)[C0,C1,C3,C4], 0x32(Ex1)[C2,C5] v:4 }
        pfsubd(d, d, d) { 0x33(Ex1)[C0,C1,C3,C4], 0x33(Ex1)[C2,C5] v:4 }
        pfmins(d, d, d) { 0x34(Ex1)[C0,C1,C3,C4] }
        pfmind(d, d, d) { 0x35(Ex1)[C0,C1,C3,C4] }
        pfmaxs(d, d, d) { 0x36(Ex1)[C0,C1,C3,C4] }
        pfmaxd(d, d, d) { 0x37(Ex1)[C0,C1,C3,C4] }
        pfmuls(d, d, d) { 0x38(Ex1)[C0,C1,C3,C4], 0x38(Ex1)[C2,C5] v:4 }
        pfmuld(d, d, d) { 0x39(Ex1)[C0,C1,C3,C4] }
        fdivs(w, w, w) { 0x48(Ex1)[C5] }
        fdivd(d, d, d) { 0x49(Ex1)[C5] }
        pfdivs(d, d, d) { 0x4a(Ex1)[C5] }
        pfdivd(d, d, d) { 0x4b(Ex1)[C5] }
        pcmpeqd(d, d, d) { 0x4d(Ex1)[C0,C3] v:3 }
        pcmpgtd(d, d, d) { 0x4f(Ex1)[C0,C3] v:3 }
        pfsqrts(d, d, d) { 0x4e(Ex1)[C5] }
        fsqrttd(d, d, d) { 0x51(Ex1)[C5] }
        pfsqrttd(d, d, d) { 0x53(Ex1)[C5] }
        pandd(d, d, d) { 0x48(Ex1)[C0,C1,C3,C4] }
        pandnd(d, d, d) { 0x49(Ex1)[C0,C1,C3,C4] }
        pord(d, d, d) { 0x4a(Ex1)[C0,C1,C3,C4] }
        pxord(d, d, d) { 0x4b(Ex1)[C0,C1,C3,C4] }
        psrld(d, d, d) { 0x4c(Ex1)[C1,C4] }
        pslld(d, d, d) { 0x4e(Ex1)[C1,C4] }
        aptoap(q, w, q) { 0x50(Ex1)[C0,C3] } // pair
        aptoapb(q, w, q) { 0x51(Ex1)[C0,C3] } // pair
        getva(q, w, d) { 0x52(Ex1)[C0,C3] }
        mpsadbh(d, d, d) { 0x58(Ex1)[C1,C4] v:3 }
        ldrd(d, d, d) { 0x5b(Ex1)[C2,C5] }
        pmaddubsh(d, d, d) { 0x68(Ex1)[C1,C4] v:3 }
        pmulhrsh(d, d, d) { 0x69(Ex1)[C1,C4] v:3 }
        phminposuh(d, d, d) { 0x6a(Ex1)[C1,C4] v:3 }
        packuswh(d, d, d) { 0x6b(Ex1)[C1,C4] v:3 }
        fstoifs(w, w, w) { 0x6c(Ex1)[C0,C1,C3,C4] v:3 }
        fdtoifd(d, d, d) { 0x6d(Ex1)[C0,C1,C3,C4] v:3 }
        pfstoifs(d, d, d) { 0x6e(Ex1)[C0,C1,C3,C4] v:3 }
        pfdtoifd(d, d, d) { 0x6f(Ex1)[C0,C1,C3,C4] v:3 }
        umulhd(w, w, d) { 0x70(Ex1)[C0,C1,C3,C4] v:3 }
        smulhd(w, w, d) { 0x71(Ex1)[C0,C1,C3,C4] v:3 }
        pfhadds(d, d, d) { 0x72(Ex1)[C0,C1,C2,C3,C4,C5] v:3 }
        pfhsubs(d, d, d) { 0x73(Ex1)[C0,C1,C2,C3,C4,C5] v:3 }
        pfaddsubs(d, d, d) { 0x77(Ex1)[C0,C1,C2,C3,C4,C5] v:3 }
        pminuw(d, d, d) { 0x20(Ex2)[C0,C3] v:3 }
        pminsw(d, d, d) { 0x21(Ex2)[C0,C3] v:3 }
        pmaxuw(d, d, d) { 0x22(Ex2)[C0,C3] v:3 }
        pmaxsw(d, d, d) { 0x23(Ex2)[C0,C3] v:3 }
        phaddh(d, d, d) { 0x29(Ex2)[C0,C3] v:3 }
        phaddw(d, d, d) { 0x2a(Ex2)[C0,C3] v:3 }
        phaddsh(d, d, d) { 0x2b(Ex2)[C0,C3] v:3 }
        phsubh(d, d, d) { 0x2d(Ex2)[C0,C3] v:3 }
        phsubw(d, d, d) { 0x2e(Ex2)[C0,C3] v:3 }
        phsubsh(d, d, d) { 0x2f(Ex2)[C0,C3] v:3 }
        psignb(d, d, d) { 0x30(Ex2)[C0,C3] v:3 }
        psignh(d, d, d) { 0x31(Ex2)[C0,C3] v:3 }
        psignw(d, d, d) { 0x32(Ex2)[C0,C3] v:3 }
        ands_fb(w, w, w) { 0x00(Ex4)[C0,C3] }
        andns_fb(w, w, w) { 0x02(Ex4)[C0,C3] }
        ors_fb(w, w, w) { 0x04(Ex4)[C0,C3] }
        orns_fb(w, w, w) { 0x06(Ex4)[C0,C3] }
        xors_fb(w, w, w) { 0x08(Ex4)[C0,C3] }
        xorns_fb(w, w, w) { 0x0a(Ex4)[C0,C3] }
        adds_fb(w, w, w) { 0x10(Ex4)[C0,C3] }
        subs_fb(w, w, w) { 0x12(Ex4)[C0,C3] }
        umulx_fb(w, w, w) { 0x22(Ex4)[C0,C3] }
        smulx_fb(w, w, w) { 0x23(Ex4)[C0,C3] }
        ands_fh(w, w, w) { 0x00(Ex5)[C0,C3] }
        andns_fh(w, w, w) { 0x02(Ex5)[C0,C3] }
        ors_fh(w, w, w) { 0x04(Ex5)[C0,C3] }
        orns_fh(w, w, w) { 0x06(Ex5)[C0,C3] }
        xors_fh(w, w, w) { 0x09(Ex5)[C0,C3] }
        xorns_fh(w, w, w) { 0x0a(Ex5)[C0,C3] }
        adds_fh(w, w, w) { 0x10(Ex5)[C0,C3] }
        subs_fh(w, w, w) { 0x12(Ex5)[C0,C3] }
        umulx_fh(w, w, w) { 0x22(Ex5)[C0,C3] }
        smulx_fh(w, w, w) { 0x23(Ex5)[C0,C3] }
        ands_fw(w, w, w) { 0x00(Ex6)[C0,C3] }
        andns_fw(w, w, w) { 0x02(Ex6)[C0,C3] }
        ors_fw(w, w, w) { 0x04(Ex6)[C0,C3] }
        orns_fw(w, w, w) { 0x06(Ex6)[C0,C3] }
        xors_fw(w, w, w) { 0x06(Ex6)[C0,C3] }
        xorns_fw(w, w, w) { 0x0a(Ex6)[C0,C3] }
        adds_fw(w, w, w) { 0x10(Ex6)[C0,C3] }
        subs_fw(w, w, w) { 0x12(Ex6)[C0,C3] }
        umulx_fw(w, w, w) { 0x22(Ex6)[C0,C3] }
        smulx_fw(w, w, w) { 0x23(Ex6)[C0,C3] }
        andd_fd(d, d, w) { 0x01(Ex7)[C0,C3] }
        andnd_fd(d, d, w) { 0x03(Ex7)[C0,C3] }
        ord_fd(d, d, w) { 0x05(Ex7)[C0,C3] }
        ornd_fd(d, d, w) { 0x07(Ex7)[C0,C3] }
        xord_fd(d, d, w) { 0x09(Ex7)[C0,C3] }
        xornd_fd(d, d, w) { 0x0b(Ex7)[C0,C3] }
        addd_fd(d, d, w) { 0x11(Ex7)[C0,C3] }
        subd_fd(d, d, w) { 0x13(Ex7)[C0,C3] }
    }
    Op4(src1: Src1, src2: Src2, src3: Src3, dst: Dst) {
        scls_fb(w, w, w, w) { 0x14(Ex4)[C0,C3] }
        scrs_fb(w, w, w, w) { 0x16(Ex4)[C0,C3] }
        shls_fb(w, w, w, w) { 0x18(Ex4)[C0,C3] }
        shrs_fb(w, w, w, w) { 0x1a(Ex4)[C0,C3] }
        sars_fb(w, w, w, w) { 0x1c(Ex4)[C0,C3] }
        incs_fb(w, w, w, w) { 0x30(Ex4)[C0,C3] }
        decs_fb(w, w, w, w) { 0x32(Ex4)[C0,C3] }
        scls_fh(w, w, w, w) { 0x14(Ex5)[C0,C3] }
        scrs_fh(w, w, w, w) { 0x16(Ex5)[C0,C3] }
        shls_fh(w, w, w, w) { 0x18(Ex5)[C0,C3] }
        shrs_fh(w, w, w, w) { 0x1a(Ex5)[C0,C3] }
        sars_fh(w, w, w, w) { 0x1c(Ex5)[C0,C3] }
        incs_fh(w, w, w, w) { 0x30(Ex5)[C0,C3] }
        decs_fh(w, w, w, w) { 0x32(Ex5)[C0,C3] }
        scls_fw(w, w, w, w) { 0x14(Ex6)[C0,C3] }
        scrs_fw(w, w, w, w) { 0x16(Ex6)[C0,C3] }
        shls_fw(w, w, w, w) { 0x18(Ex6)[C0,C3] }
        shrs_fw(w, w, w, w) { 0x1a(Ex6)[C0,C3] }
        sars_fw(w, w, w, w) { 0x1c(Ex6)[C0,C3] }
        incs_fw(w, w, w, w) { 0x30(Ex6)[C0,C3] }
        decs_fw(w, w, w, w) { 0x32(Ex6)[C0,C3] }
        scld_fd(d, d, d, w) { 0x15(Ex7)[C0,C3] }
        scrd_fd(d, d, d, w) { 0x17(Ex7)[C0,C3] }
        shld_fd(d, d, d, w) { 0x19(Ex7)[C0,C3] }
        shrd_fd(d, d, d, w) { 0x1b(Ex7)[C0,C3] }
        sard_fd(d, d, d, w) { 0x1d(Ex7)[C0,C3] }
        incd_fd(d, d, d, w) { 0x31(Ex7)[C0,C3] }
        decd_fd(d, d, d, w) { 0x33(Ex7)[C0,C3] }
        and_ands(w, w, w, w) { 0x00(Ex8)[C1,C4] }
        and_andd(d, d, d, d) { 0x01(Ex8)[C1,C4] }
        andn_ands(w, w, w, w) { 0x02(Ex8)[C1,C4] }
        andn_andd(d, d, d, d) { 0x03(Ex8)[C1,C4] }
        or_ands(w, w, w, w) { 0x04(Ex8)[C1,C4] }
        or_andd(d, d, d, d) { 0x05(Ex8)[C1,C4] }
        orn_ands(w, w, w, w) { 0x06(Ex8)[C1,C4] }
        orn_andd(d, d, d, d) { 0x07(Ex8)[C1,C4] }
        xor_ands(w, w, w, w) { 0x08(Ex8)[C1,C4] }
        xor_andd(d, d, d, d) { 0x09(Ex8)[C1,C4] }
        xorn_ands(w, w, w, w) { 0x0a(Ex8)[C1,C4] }
        xorn_andd(d, d, d, d) { 0x0b(Ex8)[C1,C4] }
        add_ands(w, w, w, w) { 0x10(Ex8)[C1,C4] }
        add_andd(d, d, d, d) { 0x11(Ex8)[C1,C4] }
        sub_ands(w, w, w, w) { 0x12(Ex8)[C1,C4] }
        sub_andd(d, d, d, d) { 0x13(Ex8)[C1,C4] }
        scl_ands(w, w, w, w) { 0x14(Ex8)[C1,C4] }
        scl_andd(d, d, d, d) { 0x15(Ex8)[C1,C4] }
        scr_ands(w, w, w, w) { 0x16(Ex8)[C1,C4] }
        scr_andd(d, d, d, d) { 0x17(Ex8)[C1,C4] }
        shl_ands(w, w, w, w) { 0x18(Ex8)[C1,C4] }
        shl_andd(d, d, d, d) { 0x19(Ex8)[C1,C4] }
        shr_ands(w, w, w, w) { 0x1a(Ex8)[C1,C4] }
        shr_andd(d, d, d, d) { 0x1b(Ex8)[C1,C4] }
        sar_ands(w, w, w, w) { 0x1c(Ex8)[C1,C4] }
        sar_andd(d, d, d, d) { 0x1d(Ex8)[C1,C4] }
        getf_ands(w, w, w, w) { 0x1e(Ex8)[C1,C4] }
        getf_andd(d, d, d, d) { 0x1f(Ex8)[C1,C4] }
        and_andns(w, w, w, w) { 0x20(Ex8)[C1,C4] }
        and_andnd(d, d, d, d) { 0x21(Ex8)[C1,C4] }
        andn_andns(w, w, w, w) { 0x22(Ex8)[C1,C4] }
        andn_andnd(d, d, d, d) { 0x23(Ex8)[C1,C4] }
        or_andns(w, w, w, w) { 0x24(Ex8)[C1,C4] }
        or_andnd(d, d, d, d) { 0x25(Ex8)[C1,C4] }
        orn_andns(w, w, w, w) { 0x26(Ex8)[C1,C4] }
        orn_andnd(d, d, d, d) { 0x27(Ex8)[C1,C4] }
        xor_andns(w, w, w, w) { 0x28(Ex8)[C1,C4] }
        xor_andnd(d, d, d, d) { 0x29(Ex8)[C1,C4] }
        xorn_andns(w, w, w, w) { 0x2a(Ex8)[C1,C4] }
        xorn_andnd(d, d, d, d) { 0x2b(Ex8)[C1,C4] }
        add_andns(w, w, w, w) { 0x30(Ex8)[C1,C4] }
        add_andnd(d, d, d, d) { 0x31(Ex8)[C1,C4] }
        sub_andns(w, w, w, w) { 0x32(Ex8)[C1,C4] }
        sub_andnd(d, d, d, d) { 0x33(Ex8)[C1,C4] }
        scl_andns(w, w, w, w) { 0x34(Ex8)[C1,C4] }
        scl_andnd(d, d, d, d) { 0x35(Ex8)[C1,C4] }
        scr_andns(w, w, w, w) { 0x36(Ex8)[C1,C4] }
        scr_andnd(d, d, d, d) { 0x37(Ex8)[C1,C4] }
        shl_andns(w, w, w, w) { 0x38(Ex8)[C1,C4] }
        shl_andnd(d, d, d, d) { 0x39(Ex8)[C1,C4] }
        shr_andns(w, w, w, w) { 0x3a(Ex8)[C1,C4] }
        shr_andnd(d, d, d, d) { 0x3b(Ex8)[C1,C4] }
        sar_andns(w, w, w, w) { 0x3c(Ex8)[C1,C4] }
        sar_andnd(d, d, d, d) { 0x3d(Ex8)[C1,C4] }
        getf_andns(w, w, w, w) { 0x3e(Ex8)[C1,C4] }
        getf_andnd(d, d, d, d) { 0x3f(Ex8)[C1,C4] }
        and_ors(w, w, w, w) { 0x40(Ex8)[C1,C4] }
        and_ord(d, d, d, d) { 0x41(Ex8)[C1,C4] }
        andn_ors(w, w, w, w) { 0x42(Ex8)[C1,C4] }
        andn_ord(d, d, d, d) { 0x43(Ex8)[C1,C4] }
        or_ors(w, w, w, w) { 0x44(Ex8)[C1,C4] }
        or_ord(d, d, d, d) { 0x45(Ex8)[C1,C4] }
        orn_ors(w, w, w, w) { 0x46(Ex8)[C1,C4] }
        orn_ord(d, d, d, d) { 0x47(Ex8)[C1,C4] }
        xor_ors(w, w, w, w) { 0x48(Ex8)[C1,C4] }
        xor_ord(d, d, d, d) { 0x49(Ex8)[C1,C4] }
        xorn_ors(w, w, w, w) { 0x4a(Ex8)[C1,C4] }
        xorn_ord(d, d, d, d) { 0x4b(Ex8)[C1,C4] }
        add_ors(w, w, w, w) { 0x50(Ex8)[C1,C4] }
        add_ord(d, d, d, d) { 0x51(Ex8)[C1,C4] }
        sub_ors(w, w, w, w) { 0x52(Ex8)[C1,C4] }
        sub_ord(d, d, d, d) { 0x53(Ex8)[C1,C4] }
        scl_ors(w, w, w, w) { 0x54(Ex8)[C1,C4] }
        scl_ord(d, d, d, d) { 0x55(Ex8)[C1,C4] }
        scr_ors(w, w, w, w) { 0x56(Ex8)[C1,C4] }
        scr_ord(d, d, d, d) { 0x57(Ex8)[C1,C4] }
        shl_ors(w, w, w, w) { 0x58(Ex8)[C1,C4] }
        shl_ord(d, d, d, d) { 0x59(Ex8)[C1,C4] }
        shr_ors(w, w, w, w) { 0x5a(Ex8)[C1,C4] }
        shr_ord(d, d, d, d) { 0x5b(Ex8)[C1,C4] }
        sar_ors(w, w, w, w) { 0x5c(Ex8)[C1,C4] }
        sar_ord(d, d, d, d) { 0x5d(Ex8)[C1,C4] }
        getf_ors(w, w, w, w) { 0x5e(Ex8)[C1,C4] }
        getf_ord(d, d, d, d) { 0x5f(Ex8)[C1,C4] }
        and_orns(w, w, w, w) { 0x60(Ex8)[C1,C4] }
        and_ornd(d, d, d, d) { 0x61(Ex8)[C1,C4] }
        andn_orns(w, w, w, w) { 0x62(Ex8)[C1,C4] }
        andn_ornd(d, d, d, d) { 0x63(Ex8)[C1,C4] }
        or_orns(w, w, w, w) { 0x64(Ex8)[C1,C4] }
        or_ornd(d, d, d, d) { 0x65(Ex8)[C1,C4] }
        orn_orns(w, w, w, w) { 0x66(Ex8)[C1,C4] }
        orn_ornd(d, d, d, d) { 0x67(Ex8)[C1,C4] }
        xor_orns(w, w, w, w) { 0x68(Ex8)[C1,C4] }
        xor_ornd(d, d, d, d) { 0x69(Ex8)[C1,C4] }
        xorn_orns(w, w, w, w) { 0x6a(Ex8)[C1,C4] }
        xorn_ornd(d, d, d, d) { 0x6b(Ex8)[C1,C4] }
        add_orns(w, w, w, w) { 0x70(Ex8)[C1,C4] }
        add_ornd(d, d, d, d) { 0x71(Ex8)[C1,C4] }
        sub_orns(w, w, w, w) { 0x72(Ex8)[C1,C4] }
        sub_ornd(d, d, d, d) { 0x73(Ex8)[C1,C4] }
        scl_orns(w, w, w, w) { 0x74(Ex8)[C1,C4] }
        scl_ornd(d, d, d, d) { 0x75(Ex8)[C1,C4] }
        scr_orns(w, w, w, w) { 0x76(Ex8)[C1,C4] }
        scr_ornd(d, d, d, d) { 0x77(Ex8)[C1,C4] }
        shl_orns(w, w, w, w) { 0x78(Ex8)[C1,C4] }
        shl_ornd(d, d, d, d) { 0x79(Ex8)[C1,C4] }
        shr_orns(w, w, w, w) { 0x7a(Ex8)[C1,C4] }
        shr_ornd(d, d, d, d) { 0x7b(Ex8)[C1,C4] }
        sar_orns(w, w, w, w) { 0x7c(Ex8)[C1,C4] }
        sar_ornd(d, d, d, d) { 0x7d(Ex8)[C1,C4] }
        getf_orns(w, w, w, w) { 0x7e(Ex8)[C1,C4] }
        getf_ornd(d, d, d, d) { 0x7f(Ex8)[C1,C4] }
        and_xors(w, w, w, w) { 0x00(Ex9)[C1,C4] }
        and_xord(d, d, d, d) { 0x01(Ex9)[C1,C4] }
        andn_xors(w, w, w, w) { 0x02(Ex9)[C1,C4] }
        andn_xord(d, d, d, d) { 0x03(Ex9)[C1,C4] }
        or_xors(w, w, w, w) { 0x04(Ex9)[C1,C4] }
        or_xord(d, d, d, d) { 0x05(Ex9)[C1,C4] }
        orn_xors(w, w, w, w) { 0x06(Ex9)[C1,C4] }
        orn_xord(d, d, d, d) { 0x07(Ex9)[C1,C4] }
        xor_xors(w, w, w, w) { 0x08(Ex9)[C1,C4] }
        xor_xord(d, d, d, d) { 0x09(Ex9)[C1,C4] }
        xorn_xors(w, w, w, w) { 0x0a(Ex9)[C1,C4] }
        xorn_xord(d, d, d, d) { 0x0b(Ex9)[C1,C4] }
        add_xors(w, w, w, w) { 0x10(Ex9)[C1,C4] }
        add_xord(d, d, d, d) { 0x11(Ex9)[C1,C4] }
        sub_xors(w, w, w, w) { 0x12(Ex9)[C1,C4] }
        sub_xord(d, d, d, d) { 0x13(Ex9)[C1,C4] }
        scl_xors(w, w, w, w) { 0x14(Ex9)[C1,C4] }
        scl_xord(d, d, d, d) { 0x15(Ex9)[C1,C4] }
        scr_xors(w, w, w, w) { 0x16(Ex9)[C1,C4] }
        scr_xord(d, d, d, d) { 0x17(Ex9)[C1,C4] }
        shl_xors(w, w, w, w) { 0x18(Ex9)[C1,C4] }
        shl_xord(d, d, d, d) { 0x19(Ex9)[C1,C4] }
        shr_xors(w, w, w, w) { 0x1a(Ex9)[C1,C4] }
        shr_xord(d, d, d, d) { 0x1b(Ex9)[C1,C4] }
        sar_xors(w, w, w, w) { 0x1c(Ex9)[C1,C4] }
        sar_xord(d, d, d, d) { 0x1d(Ex9)[C1,C4] }
        getf_xors(w, w, w, w) { 0x1e(Ex9)[C1,C4] }
        getf_xord(d, d, d, d) { 0x1f(Ex9)[C1,C4] }
        and_xorns(w, w, w, w) { 0x20(Ex9)[C1,C4] }
        and_xornd(d, d, d, d) { 0x21(Ex9)[C1,C4] }
        andn_xorns(w, w, w, w) { 0x22(Ex9)[C1,C4] }
        andn_xornd(d, d, d, d) { 0x23(Ex9)[C1,C4] }
        or_xorns(w, w, w, w) { 0x24(Ex9)[C1,C4] }
        or_xornd(d, d, d, d) { 0x25(Ex9)[C1,C4] }
        orn_xorns(w, w, w, w) { 0x26(Ex9)[C1,C4] }
        orn_xornd(d, d, d, d) { 0x27(Ex9)[C1,C4] }
        xor_xorns(w, w, w, w) { 0x28(Ex9)[C1,C4] }
        xor_xornd(d, d, d, d) { 0x29(Ex9)[C1,C4] }
        xorn_xorns(w, w, w, w) { 0x2a(Ex9)[C1,C4] }
        xorn_xornd(d, d, d, d) { 0x2b(Ex9)[C1,C4] }
        add_xorns(w, w, w, w) { 0x30(Ex9)[C1,C4] }
        add_xornd(d, d, d, d) { 0x31(Ex9)[C1,C4] }
        sub_xorns(w, w, w, w) { 0x32(Ex9)[C1,C4] }
        sub_xornd(d, d, d, d) { 0x33(Ex9)[C1,C4] }
        scl_xorns(w, w, w, w) { 0x34(Ex9)[C1,C4] }
        scl_xornd(d, d, d, d) { 0x35(Ex9)[C1,C4] }
        scr_xorns(w, w, w, w) { 0x36(Ex9)[C1,C4] }
        scr_xornd(d, d, d, d) { 0x37(Ex9)[C1,C4] }
        shl_xorns(w, w, w, w) { 0x38(Ex9)[C1,C4] }
        shl_xornd(d, d, d, d) { 0x39(Ex9)[C1,C4] }
        shr_xorns(w, w, w, w) { 0x3a(Ex9)[C1,C4] }
        shr_xornd(d, d, d, d) { 0x3b(Ex9)[C1,C4] }
        sar_xorns(w, w, w, w) { 0x3c(Ex9)[C1,C4] }
        sar_xornd(d, d, d, d) { 0x3d(Ex9)[C1,C4] }
        getf_xorns(w, w, w, w) { 0x3e(Ex9)[C1,C4] }
        getf_xornd(d, d, d, d) { 0x3f(Ex9)[C1,C4] }
        and_rsubs(w, w, w, w) { 0x40(Ex9)[C1,C4] }
        and_rsubd(d, d, d, d) { 0x41(Ex9)[C1,C4] }
        andn_rsubs(w, w, w, w) { 0x42(Ex9)[C1,C4] }
        andn_rsubd(d, d, d, d) { 0x43(Ex9)[C1,C4] }
        or_rsubs(w, w, w, w) { 0x44(Ex9)[C1,C4] }
        or_rsubd(d, d, d, d) { 0x45(Ex9)[C1,C4] }
        orn_rsubs(w, w, w, w) { 0x46(Ex9)[C1,C4] }
        orn_rsubd(d, d, d, d) { 0x47(Ex9)[C1,C4] }
        xor_rsubs(w, w, w, w) { 0x48(Ex9)[C1,C4] }
        xor_rsubd(d, d, d, d) { 0x49(Ex9)[C1,C4] }
        xorn_rsubs(w, w, w, w) { 0x4a(Ex9)[C1,C4] }
        xorn_rsubd(d, d, d, d) { 0x4b(Ex9)[C1,C4] }
        add_rsubs(w, w, w, w) { 0x50(Ex9)[C1,C4] }
        add_rsubd(d, d, d, d) { 0x51(Ex9)[C1,C4] }
        sub_rsubs(w, w, w, w) { 0x52(Ex9)[C1,C4] }
        sub_rsubd(d, d, d, d) { 0x53(Ex9)[C1,C4] }
        scl_rsubs(w, w, w, w) { 0x54(Ex9)[C1,C4] }
        scl_rsubd(d, d, d, d) { 0x55(Ex9)[C1,C4] }
        scr_rsubs(w, w, w, w) { 0x56(Ex9)[C1,C4] }
        scr_rsubd(d, d, d, d) { 0x57(Ex9)[C1,C4] }
        shl_rsubs(w, w, w, w) { 0x58(Ex9)[C1,C4] }
        shl_rsubd(d, d, d, d) { 0x59(Ex9)[C1,C4] }
        shr_rsubs(w, w, w, w) { 0x5a(Ex9)[C1,C4] }
        shr_rsubd(d, d, d, d) { 0x5b(Ex9)[C1,C4] }
        sar_rsubs(w, w, w, w) { 0x5c(Ex9)[C1,C4] }
        sar_rsubd(d, d, d, d) { 0x5d(Ex9)[C1,C4] }
        getf_rsubs(w, w, w, w) { 0x5e(Ex9)[C1,C4] }
        getf_rsubd(d, d, d, d) { 0x5f(Ex9)[C1,C4] }
        and_adds(w, w, w, w) { 0x00(Exa)[C1,C4] }
        and_addd(d, d, d, d) { 0x01(Exa)[C1,C4] }
        andn_adds(w, w, w, w) { 0x02(Exa)[C1,C4] }
        andn_addd(d, d, d, d) { 0x03(Exa)[C1,C4] }
        or_adds(w, w, w, w) { 0x04(Exa)[C1,C4] }
        or_addd(d, d, d, d) { 0x05(Exa)[C1,C4] }
        orn_adds(w, w, w, w) { 0x06(Exa)[C1,C4] }
        orn_addd(d, d, d, d) { 0x07(Exa)[C1,C4] }
        xor_adds(w, w, w, w) { 0x08(Exa)[C1,C4] }
        xor_addd(d, d, d, d) { 0x09(Exa)[C1,C4] }
        xorn_adds(w, w, w, w) { 0x0a(Exa)[C1,C4] }
        xorn_addd(d, d, d, d) { 0x0b(Exa)[C1,C4] }
        add_adds(w, w, w, w) { 0x10(Exa)[C1,C4] }
        add_addd(d, d, d, d) { 0x11(Exa)[C1,C4] }
        sub_adds(w, w, w, w) { 0x12(Exa)[C1,C4] }
        sub_addd(d, d, d, d) { 0x13(Exa)[C1,C4] }
        scl_adds(w, w, w, w) { 0x14(Exa)[C1,C4] }
        scl_addd(d, d, d, d) { 0x15(Exa)[C1,C4] }
        scr_adds(w, w, w, w) { 0x16(Exa)[C1,C4] }
        scr_addd(d, d, d, d) { 0x17(Exa)[C1,C4] }
        shl_adds(w, w, w, w) { 0x18(Exa)[C1,C4] }
        shl_addd(d, d, d, d) { 0x19(Exa)[C1,C4] }
        shr_adds(w, w, w, w) { 0x1a(Exa)[C1,C4] }
        shr_addd(d, d, d, d) { 0x1b(Exa)[C1,C4] }
        sar_adds(w, w, w, w) { 0x1c(Exa)[C1,C4] }
        sar_addd(d, d, d, d) { 0x1d(Exa)[C1,C4] }
        getf_adds(w, w, w, w) { 0x1e(Exa)[C1,C4] }
        getf_addd(d, d, d, d) { 0x1f(Exa)[C1,C4] }
        and_subs(w, w, w, w) { 0x20(Exa)[C1,C4] }
        and_subd(d, d, d, d) { 0x21(Exa)[C1,C4] }
        andn_subs(w, w, w, w) { 0x22(Exa)[C1,C4] }
        andn_subd(d, d, d, d) { 0x23(Exa)[C1,C4] }
        or_subs(w, w, w, w) { 0x24(Exa)[C1,C4] }
        or_subd(d, d, d, d) { 0x25(Exa)[C1,C4] }
        orn_subs(w, w, w, w) { 0x26(Exa)[C1,C4] }
        orn_subd(d, d, d, d) { 0x27(Exa)[C1,C4] }
        xor_subs(w, w, w, w) { 0x28(Exa)[C1,C4] }
        xor_subd(d, d, d, d) { 0x29(Exa)[C1,C4] }
        xorn_subs(w, w, w, w) { 0x2a(Exa)[C1,C4] }
        xorn_subd(d, d, d, d) { 0x2b(Exa)[C1,C4] }
        add_subs(w, w, w, w) { 0x30(Exa)[C1,C4] }
        add_subd(d, d, d, d) { 0x31(Exa)[C1,C4] }
        sub_subs(w, w, w, w) { 0x32(Exa)[C1,C4] }
        sub_subd(d, d, d, d) { 0x33(Exa)[C1,C4] }
        scl_subs(w, w, w, w) { 0x34(Exa)[C1,C4] }
        scl_subd(d, d, d, d) { 0x35(Exa)[C1,C4] }
        scr_subs(w, w, w, w) { 0x36(Exa)[C1,C4] }
        scr_subd(d, d, d, d) { 0x37(Exa)[C1,C4] }
        shl_subs(w, w, w, w) { 0x38(Exa)[C1,C4] }
        shl_subd(d, d, d, d) { 0x39(Exa)[C1,C4] }
        shr_subs(w, w, w, w) { 0x3a(Exa)[C1,C4] }
        shr_subd(d, d, d, d) { 0x3b(Exa)[C1,C4] }
        sar_subs(w, w, w, w) { 0x3c(Exa)[C1,C4] }
        sar_subd(d, d, d, d) { 0x3d(Exa)[C1,C4] }
        getf_subs(w, w, w, w) { 0x3e(Exa)[C1,C4] }
        getf_subd(d, d, d, d) { 0x3f(Exa)[C1,C4] }
        insfs(w, w, w, w) { 0x6c(Exb)[C0,C1,C3,C4] v:4 }
        insfd(d, d, d, d) { 0x6d(Exb)[C0,C1,C3,C4] }
        fadd_adds(w, w, w, w) { 0x00(Exc)[C0,C1,C3,C4] v:2, 0x00(Exc)[C2,C5] v:4 }
        fadd_addd(d, d, d, d) { 0x01(Exc)[C0,C1,C3,C4] v:2, 0x01(Exc)[C2,C5] v:4 }
        fsub_adds(w, w, w, w) { 0x02(Exc)[C0,C1,C3,C4] v:2, 0x02(Exc)[C2,C5] v:4 }
        fsub_addd(d, d, d, d) { 0x03(Exc)[C0,C1,C3,C4] v:2, 0x03(Exc)[C2,C5] v:4 }
        fmul_adds(w, w, w, w) { 0x08(Exc)[C0,C1,C3,C4], 0x08(Exc)[C2,C5] v:4 }
        fmul_addd(d, d, d, d) { 0x09(Exc)[C0,C1,C3,C4], 0x09(Exc)[C2,C5] v:4 }
        fadd_subs(w, w, w, w) { 0x20(Exc)[C0,C1,C3,C4] v:2, 0x20(Exc)[C2,C5] v:4 }
        fadd_subd(d, d, d, d) { 0x21(Exc)[C0,C1,C3,C4] v:2, 0x21(Exc)[C2,C5] v:4 }
        fsub_subs(w, w, w, w) { 0x22(Exc)[C0,C1,C3,C4] v:2, 0x22(Exc)[C2,C5] v:4 }
        fsub_subd(d, d, d, d) { 0x23(Exc)[C0,C1,C3,C4] v:2, 0x23(Exc)[C2,C5] v:4 }
        fmul_subs(w, w, w, w) { 0x28(Exc)[C0,C1,C3,C4], 0x28(Exc)[C2,C5] v:4 }
        fmul_subd(d, d, d, d) { 0x29(Exc)[C0,C1,C3,C4], 0x29(Exc)[C2,C5] v:4 }
        fadd_rsubs(w, w, w, w) { 0x20(Exd)[C0,C1,C3,C4] v:2, 0x20(Exd)[C2,C5] v:4 }
        fadd_rsubd(d, d, d, d) { 0x21(Exd)[C0,C1,C3,C4] v:2, 0x21(Exd)[C2,C5] v:4 }
        fsub_rsubs(w, w, w, w) { 0x22(Exd)[C0,C1,C3,C4] v:2, 0x22(Exd)[C2,C5] v:4 }
        fsub_rsubd(d, d, d, d) { 0x23(Exd)[C0,C1,C3,C4] v:2, 0x23(Exd)[C2,C5] v:4 }
        fmul_rsubs(w, w, w, w) { 0x28(Exd)[C0,C1,C3,C4], 0x28(Exd)[C2,C5] v:4 }
        fmul_rsubd(d, d, d, d) { 0x29(Exd)[C0,C1,C3,C4], 0x29(Exd)[C2,C5] v:4 }
        pfadd_adds(d, d, d, d) { 0x00(Exe)[C0,C1,C3,C4] v:2, 0x00(Exe)[C2,C5] v:4 }
        pfadd_addd(d, d, d, d) { 0x01(Exe)[C0,C1,C3,C4] v:2, 0x01(Exe)[C2,C5] v:4 }
        pfsub_adds(d, d, d, d) { 0x02(Exe)[C0,C1,C3,C4] v:2, 0x02(Exe)[C2,C5] v:4 }
        pfsub_addd(d, d, d, d) { 0x03(Exe)[C0,C1,C3,C4] v:2, 0x03(Exe)[C2,C5] v:4 }
        pfhadd_adds(d, d, d, d) { 0x04(Exe)[C0,C1,C3,C4] v:3, 0x04(Exe)[C2,C5] v:4 }
        pfhsub_adds(d, d, d, d) { 0x06(Exe)[C0,C1,C3,C4] v:3, 0x06(Exe)[C2,C5] v:4 }
        pfmul_adds(d, d, d, d) { 0x08(Exe)[C0,C1,C3,C4], 0x08(Exe)[C2,C5] v:4 }
        pfmul_addd(d, d, d, d) { 0x09(Exe)[C0,C1,C3,C4], 0x09(Exe)[C2,C5] v:4 }
        pfaddsub_adds(d, d, d, d) { 0x0e(Exe)[C0,C1,C3,C4] v:3, 0x0e(Exe)[C2,C5] v:4 }
        pfadd_subs(d, d, d, d) { 0x20(Exe)[C0,C1,C3,C4] v:2, 0x20(Exe)[C2,C5] v:4 }
        pfadd_subd(d, d, d, d) { 0x21(Exe)[C0,C1,C3,C4] v:2, 0x21(Exe)[C2,C5] v:4 }
        pfsub_subs(d, d, d, d) { 0x22(Exe)[C0,C1,C3,C4] v:2, 0x22(Exe)[C2,C5] v:4 }
        pfsub_subd(d, d, d, d) { 0x23(Exe)[C0,C1,C3,C4] v:2, 0x23(Exe)[C2,C5] v:4 }
        pfhadd_subs(d, d, d, d) { 0x24(Exe)[C0,C1,C3,C4] v:3, 0x24(Exe)[C2,C5] v:4 }
        pfhsub_subs(d, d, d, d) { 0x26(Exe)[C0,C1,C3,C4] v:3, 0x26(Exe)[C2,C5] v:4 }
        pfmul_subs(d, d, d, d) { 0x28(Exe)[C0,C1,C3,C4], 0x28(Exe)[C2,C5] v:4 }
        pfmul_subd(d, d, d, d) { 0x29(Exe)[C0,C1,C3,C4], 0x29(Exe)[C2,C5] v:4 }
        pfaddsub_subs(d, d, d, d) { 0x2e(Exe)[C0,C1,C3,C4] v:3, 0x2e(Exe)[C2,C5] v:4 }
        pfadd_hadds(d, d, d, d) { 0x40(Exe)[C0,C1,C3,C4] v:3, 0x40(Exe)[C2,C5] v:4 }
        pfsub_hadds(d, d, d, d) { 0x42(Exe)[C0,C1,C3,C4] v:3, 0x42(Exe)[C2,C5] v:4 }
        pfhadd_hadds(d, d, d, d) { 0x44(Exe)[C0,C1,C3,C4] v:3, 0x44(Exe)[C2,C5] v:4 }
        pfhsub_hadds(d, d, d, d) { 0x46(Exe)[C0,C1,C3,C4] v:3, 0x46(Exe)[C2,C5] v:4 }
        pfmul_hadds(d, d, d, d) { 0x48(Exe)[C0,C1,C3,C4] v:3, 0x48(Exe)[C2,C5] v:4 }
        pfaddsub_hadds(d, d, d, d) { 0x4e(Exe)[C0,C1,C3,C4] v:3, 0x4e(Exe)[C2,C5] v:4 }
        pfadd_hsubs(d, d, d, d) { 0x60(Exe)[C0,C1,C3,C4] v:3, 0x60(Exe)[C2,C5] v:4 }
        pfsub_hsubs(d, d, d, d) { 0x62(Exe)[C0,C1,C3,C4] v:3, 0x62(Exe)[C2,C5] v:4 }
        pfhadd_hsubs(d, d, d, d) { 0x64(Exe)[C0,C1,C3,C4] v:3, 0x64(Exe)[C2,C5] v:4 }
        pfhsub_hsubs(d, d, d, d) { 0x66(Exe)[C0,C1,C3,C4] v:3, 0x66(Exe)[C2,C5] v:4 }
        pfmul_hsubs(d, d, d, d) { 0x68(Exe)[C0,C1,C3,C4] v:3, 0x68(Exe)[C2,C5] v:4 }
        pfaddsub_hsubs(d, d, d, d) { 0x6e(Exe)[C0,C1,C3,C4] v:3, 0x6e(Exe)[C2,C5] v:4 }
        pfadd_rsubs(d, d, d, d) { 0x20(Exf)[C0,C1,C3,C4] v:2, 0x20(Exf)[C2,C5] v:4 }
        pfadd_rsubd(d, d, d, d) { 0x21(Exf)[C0,C1,C3,C4] v:2, 0x21(Exf)[C2,C5] v:4 }
        pfsub_rsubs(d, d, d, d) { 0x22(Exf)[C0,C1,C3,C4] v:2, 0x22(Exf)[C2,C5] v:4 }
        pfsub_rsubd(d, d, d, d) { 0x23(Exf)[C0,C1,C3,C4] v:2, 0x23(Exf)[C2,C5] v:4 }
        pfhadd_rsubs(d, d, d, d) { 0x24(Exf)[C0,C1,C3,C4] v:3, 0x24(Exf)[C2,C5] v:4 }
        pfhsub_rsubs(d, d, d, d) { 0x26(Exf)[C0,C1,C3,C4] v:3, 0x26(Exf)[C2,C5] v:4 }
        pfmul_rsubs(d, d, d, d) { 0x28(Exf)[C0,C1,C3,C4], 0x28(Exf)[C2,C5] v:4 }
        pfmul_rsubd(d, d, d, d) { 0x29(Exf)[C0,C1,C3,C4], 0x29(Exf)[C2,C5] v:4 }
        pfaddsub_rsubs(d, d, d, d) { 0x2e(Exf)[C0,C1,C3,C4] v:3, 0x2e(Exf)[C2,C5] v:4 }
        pshufb(d, d, d, d) { 0x4d(Exf)[C0,C1,C3,C4] v:2 }
        pfadd_addsubs(d, d, d, d) { 0x60(Exf)[C0,C1,C3,C4] v:3, 0x60(Exf)[C2,C5] v:4 }
        pfsub_addsubs(d, d, d, d) { 0x62(Exf)[C0,C1,C3,C4] v:3, 0x62(Exf)[C2,C5] v:4 }
        pfhadd_addsubs(d, d, d, d) { 0x64(Exf)[C0,C1,C3,C4] v:3, 0x64(Exf)[C2,C5] v:4 }
        pfhsub_addsubs(d, d, d, d) { 0x66(Exf)[C0,C1,C3,C4] v:3, 0x66(Exf)[C2,C5] v:4 }
        pfmul_addsubs(d, d, d, d) { 0x68(Exf)[C0,C1,C3,C4] v:3, 0x68(Exf)[C2,C5] v:4 }
        pmerge(d, d, d, d) { 0x6d(Exf)[C0,C1,C3,C4] v:2 }
        pfaddsub_addsubs(d, d, d, d) { 0x6e(Exf)[C0,C1,C3,C4] v:3, 0x6e(Exf)[C2,C5] v:4 }
    }
    Op2cmp(src2: Src2, dst: DstPreg) {
        cctopo(w) { 0x24[C0,C1,C3,C4] if cmp_op == 0 }
        cctopb(w) { 0x24[C0,C1,C3,C4] if cmp_op == 1 }
        cctope(w) { 0x24[C0,C1,C3,C4] if cmp_op == 2 }
        cctopbe(w) { 0x24[C0,C1,C3,C4] if cmp_op == 3 }
        cctops(w) { 0x24[C0,C1,C3,C4] if cmp_op == 4 }
        cctopp(w) { 0x24[C0,C1,C3,C4] if cmp_op == 5 }
        cctopl(w) { 0x24[C0,C1,C3,C4] if cmp_op == 6 }
        cctople(w) { 0x24[C0,C1,C3,C4] if cmp_op == 7 }
    }
    Op3cmp(src1: Src1, src2: Src2, dst: DstPreg) {
        cmposb(w, w) { 0x20[C0,C1,C3,C4] if cmp_op == 0 }
        cmpbsb(w, w) { 0x20[C0,C1,C3,C4] if cmp_op == 1 }
        cmpesb(w, w) { 0x20[C0,C1,C3,C4] if cmp_op == 2 }
        cmpbesb(w, w) { 0x20[C0,C1,C3,C4] if cmp_op == 3 }
        cmpssb(w, w) { 0x20[C0,C1,C3,C4] if cmp_op == 4 }
        cmppsb(w, w) { 0x20[C0,C1,C3,C4] if cmp_op == 5 }
        cmplsb(w, w) { 0x20[C0,C1,C3,C4] if cmp_op == 6 }
        cmplesb(w, w) { 0x20[C0,C1,C3,C4] if cmp_op == 7 }
        cmpodb(d, d) { 0x21[C0,C1,C3,C4] if cmp_op == 0 }
        cmpbdb(d, d) { 0x21[C0,C1,C3,C4] if cmp_op == 1 }
        cmpedb(d, d) { 0x21[C0,C1,C3,C4] if cmp_op == 2 }
        cmpbedb(d, d) { 0x21[C0,C1,C3,C4] if cmp_op == 3 }
        cmpsdb(d, d) { 0x21[C0,C1,C3,C4] if cmp_op == 4 }
        cmppdb(d, d) { 0x21[C0,C1,C3,C4] if cmp_op == 5 }
        cmpldb(d, d) { 0x21[C0,C1,C3,C4] if cmp_op == 6 }
        cmpledb(d, d) { 0x21[C0,C1,C3,C4] if cmp_op == 7 }
        cmpandesb(w, w) { 0x22[C0,C1,C3,C4] if cmp_op == 2 }
        cmpandssb(w, w) { 0x22[C0,C1,C3,C4] if cmp_op == 4 }
        cmpandpsb(w, w) { 0x22[C0,C1,C3,C4] if cmp_op == 5 }
        cmpandlesb(w, w) { 0x22[C0,C1,C3,C4] if cmp_op == 7 }
        cmpandedb(d, d) { 0x23[C0,C1,C3,C4] if cmp_op == 2 }
        cmpandsdb(d, d) { 0x23[C0,C1,C3,C4] if cmp_op == 4 }
        cmpandpdb(d, d) { 0x23[C0,C1,C3,C4] if cmp_op == 5 }
        cmpandledb(d, d) { 0x23[C0,C1,C3,C4] if cmp_op == 7 }
        fxcmpeqsb(x, w) { 0x28[C0,C1,C3,C4] if cmp_op == 0 }
        fxcmpltsb(x, w) { 0x28[C0,C1,C3,C4] if cmp_op == 1 }
        fxcmplesb(x, w) { 0x28[C0,C1,C3,C4] if cmp_op == 2 }
        fxcmpuodsb(x, w) { 0x28[C0,C1,C3,C4] if cmp_op == 3 }
        fxcmpneqsb(x, w) { 0x28[C0,C1,C3,C4] if cmp_op == 4 }
        fxcmpnltsb(x, w) { 0x28[C0,C1,C3,C4] if cmp_op == 5 }
        fxcmpnlesb(x, w) { 0x28[C0,C1,C3,C4] if cmp_op == 6 }
        fxcmpodsb(x, w) { 0x28[C0,C1,C3,C4] if cmp_op == 7 }
        fxcmpeqdb(x, d) { 0x29[C0,C1,C3,C4] if cmp_op == 0 }
        fxcmpltdb(x, d) { 0x29[C0,C1,C3,C4] if cmp_op == 1 }
        fxcmpledb(x, d) { 0x29[C0,C1,C3,C4] if cmp_op == 2 }
        fxcmpuoddb(x, d) { 0x29[C0,C1,C3,C4] if cmp_op == 3 }
        fxcmpneqdb(x, d) { 0x29[C0,C1,C3,C4] if cmp_op == 4 }
        fxcmpnltdb(x, d) { 0x29[C0,C1,C3,C4] if cmp_op == 5 }
        fxcmpnledb(x, d) { 0x29[C0,C1,C3,C4] if cmp_op == 6 }
        fxcmpoddb(x, d) { 0x29[C0,C1,C3,C4] if cmp_op == 7 }
        fxcmpeqxb(x, x) { 0x2b[C0,C1,C3,C4] if cmp_op == 0 }
        fxcmpltxb(x, x) { 0x2b[C0,C1,C3,C4] if cmp_op == 1 }
        fxcmplexb(x, x) { 0x2b[C0,C1,C3,C4] if cmp_op == 2 }
        fxcmpuodxb(x, x) { 0x2b[C0,C1,C3,C4] if cmp_op == 3 }
        fxcmpneqxb(x, x) { 0x2b[C0,C1,C3,C4] if cmp_op == 4 }
        fxcmpnltxb(x, x) { 0x2b[C0,C1,C3,C4] if cmp_op == 5 }
        fxcmpnlexb(x, x) { 0x2b[C0,C1,C3,C4] if cmp_op == 6 }
        fxcmpodxb(x, x) { 0x2b[C0,C1,C3,C4] if cmp_op == 7 }
        fcmpeqsb(w, w) { 0x2e[C0,C1,C3,C4] if cmp_op == 0 }
        fcmpltsb(w, w) { 0x2e[C0,C1,C3,C4] if cmp_op == 1 }
        fcmplesb(w, w) { 0x2e[C0,C1,C3,C4] if cmp_op == 2 }
        fcmpuodsb(w, w) { 0x2e[C0,C1,C3,C4] if cmp_op == 3 }
        fcmpneqsb(w, w) { 0x2e[C0,C1,C3,C4] if cmp_op == 4 }
        fcmpnltsb(w, w) { 0x2e[C0,C1,C3,C4] if cmp_op == 5 }
        fcmpnlesb(w, w) { 0x2e[C0,C1,C3,C4] if cmp_op == 6 }
        fcmpodsb(w, w) { 0x2e[C0,C1,C3,C4] if cmp_op == 7 }
        fcmpeqdb(d, d) { 0x2f[C0,C1,C3,C4] if cmp_op == 0 }
        fcmpltdb(d, d) { 0x2f[C0,C1,C3,C4] if cmp_op == 1 }
        fcmpledb(d, d) { 0x2f[C0,C1,C3,C4] if cmp_op == 2 }
        fcmpuoddb(d, d) { 0x2f[C0,C1,C3,C4] if cmp_op == 3 }
        fcmpneqdb(d, d) { 0x2f[C0,C1,C3,C4] if cmp_op == 4 }
        fcmpnltdb(d, d) { 0x2f[C0,C1,C3,C4] if cmp_op == 5 }
        fcmpnledb(d, d) { 0x2f[C0,C1,C3,C4] if cmp_op == 6 }
        fcmpoddb(d, d) { 0x2f[C0,C1,C3,C4] if cmp_op == 7 }
    }
    Op3mrgc(src1: Src1, src2: Src2, dst: Dst, cond: MergeCond) {
        merges(w, w, w) { 0x0e[C0,C1,C2,C3,C4,C5] } // mrgc
        merged(d, d, d) { 0x0f[C0,C1,C2,C3,C4,C5] } // mrgc
    }
    Op4mrgc(src1: Src1, src2: Src2, src3: Src3, dst: Dst, cond: MergeCond) {
        merge_ands(w, w, w, w) { 0x0e(Ex8)[C1,C4] } // mrgc
        merge_andd(d, d, d, d) { 0x0f(Ex8)[C1,C4] } // mrgc
        merge_andns(w, w, w, w) { 0x2e(Ex8)[C1,C4] } // mrgc
        merge_andnd(d, d, d, d) { 0x2f(Ex8)[C1,C4] } // mrgc
        merge_ors(w, w, w, w) { 0x4e(Ex8)[C1,C4] } // mrgc
        merge_ord(d, d, d, d) { 0x4f(Ex8)[C1,C4] } // mrgc
        merge_orns(w, w, w, w) { 0x6e(Ex8)[C1,C4] } // mrgc
        merge_ornd(d, d, d, d) { 0x6f(Ex8)[C1,C4] } // mrgc
        merge_xors(w, w, w, w) { 0x0e(Ex9)[C1,C4] } // mrgc
        merge_xord(d, d, d, d) { 0x0f(Ex9)[C1,C4] } // mrgc
        merge_xorns(w, w, w, w) { 0x2e(Ex9)[C1,C4] } // mrgc
        merge_xornd(d, d, d, d) { 0x2f(Ex9)[C1,C4] } // mrgc
        merge_rsubs(w, w, w, w) { 0x4e(Ex9)[C1,C4] } // mrgc
        merge_rsubd(d, d, d, d) { 0x4f(Ex9)[C1,C4] } // mrgc
        merge_adds(w, w, w, w) { 0x0e(Exa)[C1,C4] } // mrgc
        merge_addd(d, d, d, d) { 0x0f(Exa)[C1,C4] } // mrgc
        merge_subs(w, w, w, w) { 0x2e(Exa)[C1,C4] } // mrgc
        merge_subd(d, d, d, d) { 0x2f(Exa)[C1,C4] } // mrgc
    }
    Op3imm8(src2: Src2, src3: Imm8, dst: Dst) {
        pshufh(d, d) { 0x17(Ex1)[C1,C4] }
    }
    Op4imm8(src1: Src1, src2: Src2, src3: Imm8, dst: Dst) {
        psrlqh(d, d, d) { 0x0c(Ex1)[C1,C4] }
        psrlql(d, d, d) { 0x0d(Ex1)[C1,C4] }
        psllqh(d, d, d) { 0x0e(Ex1)[C1,C4] }
        psllql(d, d, d) { 0x0f(Ex1)[C1,C4] }
        pshufw(d, d, d) { 0x16(Ex1)[C1,C4] }
        pextrh(d, d, d) { 0x1e(Ex1)[C1,C3] }
        pinsh(d, d, d) { 0x1f(Ex1)[C1,C3] }
    }
    Op3load(addr: Addr, dst: Dst) {
        ldb(d, d, b) { 0x64[C0,C2,C3,C5] }
        ldh(d, d, h) { 0x65[C0,C2,C3,C5] }
        ldw(d, d, w) { 0x66[C0,C2,C3,C5] }
        ldd(d, d, d) { 0x67[C0,C2,C3,C5] }
        ldcsb(w, w, b) { 0x68[C0,C2,C3,C5] }
        ldcsh(w, w, h) { 0x69[C0,C2,C3,C5] }
        ldcsw(w, w, w) { 0x6a[C0,C2,C3,C5] }
        ldcsd(w, w, d) { 0x6b[C0,C2,C3,C5] }
        lddsb(w, w, b) { 0x6c[C0,C2,C3,C5] }
        lddsh(w, w, h) { 0x6d[C0,C2,C3,C5] }
        lddsw(w, w, w) { 0x6e[C0,C2,C3,C5] }
        lddsd(w, w, d) { 0x6f[C0,C2,C3,C5] }
        ldesb(w, w, b) { 0x70[C0,C2,C3,C5] }
        ldesh(w, w, h) { 0x71[C0,C2,C3,C5] }
        ldesw(w, w, w) { 0x72[C0,C2,C3,C5] }
        ldesd(w, w, d) { 0x73[C0,C2,C3,C5] }
        ldfsb(w, w, b) { 0x74[C0,C2,C3,C5] }
        ldfsh(w, w, h) { 0x75[C0,C2,C3,C5] }
        ldfsw(w, w, w) { 0x76[C0,C2,C3,C5] }
        ldfsd(w, w, d) { 0x77[C0,C2,C3,C5] }
        ldgsb(w, w, b) { 0x78[C0,C2,C3,C5] }
        ldgsh(w, w, h) { 0x79[C0,C2,C3,C5] }
        ldgsw(w, w, w) { 0x7a[C0,C2,C3,C5] }
        ldgsd(w, w, d) { 0x7b[C0,C2,C3,C5] }
        ldssb(w, w, b) { 0x7c[C0,C2,C3,C5] }
        ldssh(w, w, h) { 0x7d[C0,C2,C3,C5] }
        ldssw(w, w, w) { 0x7e[C0,C2,C3,C5] }
        ldssd(w, w, d) { 0x7f[C0,C2,C3,C5] }
        stcsq(w, w, q) { 0x02[C5], 0x02(Ex1)[C2] } // pair
        stdsq(w, w, q) { 0x03[C5], 0x03(Ex1)[C2] } // pair
        stesq(w, w, q) { 0x04[C5], 0x04(Ex1)[C2] } // pair
        stfsq(w, w, q) { 0x05[C5], 0x05(Ex1)[C2] } // pair
        stgsq(w, w, q) { 0x06[C5], 0x06(Ex1)[C2] } // pair
        stssq(w, w, q) { 0x07[C5], 0x07(Ex1)[C2] } // pair
        ldcsq(w, w, q) { 0x42(Ex1)[C0,C2,C3,C5] } // pair
        lddsq(w, w, q) { 0x43(Ex1)[C0,C2,C3,C5] } // pair
        ldesq(w, w, q) { 0x44(Ex1)[C0,C2,C3,C5] } // pair
        ldfsq(w, w, q) { 0x45(Ex1)[C0,C2,C3,C5] } // pair
        ldgsq(w, w, q) { 0x46(Ex1)[C0,C2,C3,C5] } // pair
        ldssq(w, w, q) { 0x47(Ex1)[C0,C2,C3,C5] } // pair
        ldcudb(w, w, b) { 0x60(Ex1)[C0,C2,C3,C5] }
        ldcudh(w, w, h) { 0x61(Ex1)[C0,C2,C3,C5] }
        ldcudw(w, w, w) { 0x62(Ex1)[C0,C2,C3,C5] }
        ldcudd(w, w, d) { 0x63(Ex1)[C0,C2,C3,C5] }
        ldgdb(w, w, b) { 0x64(Ex1)[C0,C2,C3,C5] }
        ldgdh(w, w, h) { 0x65(Ex1)[C0,C2,C3,C5] }
        ldgdw(w, w, w) { 0x66(Ex1)[C0,C2,C3,C5] }
        ldgdd(w, w, d) { 0x67(Ex1)[C0,C2,C3,C5] }
        ldapb(q, w, b) { 0x68(Ex1)[C0,C2,C3,C5] }
        ldaph(q, w, h) { 0x69(Ex1)[C0,C2,C3,C5] }
        ldapw(q, w, w) { 0x6a(Ex1)[C0,C2,C3,C5] }
        ldapd(q, w, d) { 0x6b(Ex1)[C0,C2,C3,C5] }
    }
    Op3store(src4: Src4, addr: Addr) {
        stb(b, d, d) { 0x24[C2,C5] }
        sth(h, d, d) { 0x25[C2,C5] }
        stw(w, d, d) { 0x26[C2,C5] }
        std(d, d, d) { 0x27[C2,C5] }
        stcsb(b, w, w) { 0x28[C2,C5] }
        stcsh(h, w, w) { 0x29[C2,C5] }
        stcsw(w, w, w) { 0x2a[C2,C5] }
        stcsd(d, w, w) { 0x2b[C2,C5] }
        stdsb(b, w, w) { 0x2c[C2,C5] }
        stdsh(h, w, w) { 0x2d[C2,C5] }
        stdsw(w, w, w) { 0x2e[C2,C5] }
        stdsd(d, w, w) { 0x2f[C2,C5] }
        stesb(b, w, w) { 0x30[C2,C5] }
        stesh(h, w, w) { 0x31[C2,C5] }
        stesw(w, w, w) { 0x32[C2,C5] }
        stesd(d, w, w) { 0x33[C2,C5] }
        stfsb(b, w, w) { 0x34[C2,C5] }
        stfsh(h, w, w) { 0x35[C2,C5] }
        stfsw(w, w, w) { 0x36[C2,C5] }
        stfsd(d, w, w) { 0x37[C2,C5] }
        stgsb(b, w, w) { 0x38[C2,C5] }
        stgsh(h, w, w) { 0x39[C2,C5] }
        stgsw(w, w, w) { 0x3a[C2,C5] }
        stgsd(d, w, w) { 0x3b[C2,C5] }
        stssb(b, w, w) { 0x3c[C2,C5] }
        stssh(h, w, w) { 0x3d[C2,C5] }
        stssw(w, w, w) { 0x3e[C2,C5] }
        stssd(d, w, w) { 0x3f[C2,C5] }
        stgdb(b, w, w) { 0x24(Ex1)[C2,C5] }
        stgdh(h, w, w) { 0x25(Ex1)[C2,C5] }
        stgdw(w, w, w) { 0x26(Ex1)[C2,C5] }
        stgdd(d, w, w) { 0x27(Ex1)[C2,C5] }
        stapb(b, q, w) { 0x28(Ex1)[C2,C5] }
        staph(h, q, w) { 0x29(Ex1)[C2,C5] }
        stapw(w, q, w) { 0x2a(Ex1)[C2,C5] }
        stapd(d, q, w) { 0x2b(Ex1)[C2,C5] }
        stgdq(q, w, w) { 0x39(Ex1)[C2,C5] } // pair
        stapq(q, q, w) { 0x3a(Ex1)[C2,C5] } // pair
    }
    Op2rw(src2: Src2, dst: StateReg) {
        rws { 0x3c(Ex1)[C0] } // state
        rwd { 0x3d(Ex1)[C0] } // state
    }
    Op2rr(src1: StateReg, dst: Dst) {
        rrs { 0x3e(Ex1)[C0] } // state
        rrd { 0x3f(Ex1)[C0] } // state
    }
    OpAload(addr: AddrArray, dst: Dst) {
        ldaab(b) { 0x5c(Ex1)[C2,C5] }
        ldaah(h) { 0x5d(Ex1)[C2,C5] }
        ldaaw(w) { 0x5e(Ex1)[C2,C5] } // aaurrs, mas
        ldaad(d) { 0x5f(Ex1)[C2,C5] } // aaurrd, mas
        ldaaq(q) { 0x7f(Ex1)[C2,C5] } // apurrq, pair, mas
    }
    OpAstore(src4: Src4, addr: AddrArray) {
        staab(b) { 0x1c(Ex1)[C2,C5] }
        staah(h) { 0x1d(Ex1)[C2,C5] }
        staaw(w) { 0x1e(Ex1)[C2,C5] } // apurw, mas
        staad(d) { 0x1f(Ex1)[C2,C5] } // apurwd, mas
        staaq(q) { 0x3f(Ex1)[C2,C5] } // apurwq, pair, mas
    }
}
