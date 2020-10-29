pub mod operands;

use self::operands::*;
use super::Index;
use super::Raw;
use crate::{Error, InsertInto};
use core::convert::TryFrom;
use core::fmt;
use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Copy, Clone, Debug, PartialEq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum Ext {
    None,
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
    ($o:literal, $e:path) => {
        ($o, $e)
    };
    ($o:literal) => {
        ($o, Ext::None)
    };
}

macro_rules! ver {
    () => {
        0
    };
    ($v:literal) => {
        $v
    };
}

macro_rules! print_args {
    ($f:ident, $a:ident) => { write!($f, "{}", $a)? };
    ($f:ident, $a:ident, $($tail:ident),+) => {
        write!($f, "{}, ", $a)?;
        print_args!($f, $($tail),+)
    };
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
    (@opcode $v:ident, $o:literal, $($n:ident = $e:expr),* $(,)?) => {
        Opcode {
            version: $v,
            op: $o,
            $($n: Some($e),)*
            .. Opcode::default()
        }
    };
    (
    $(
        $k:ident($($a:ident: $t:ty),+) {
            $(
                $i:ident {
                    $( $o:literal $(($e:path))? $([$($c:pat),+])? $(v:$v:literal)? $(if $($n:ident == $x:expr),+)? ),+
                }
            )+
        }
    )+
    ) => {
        $( // types
            #[allow(non_camel_case_types)]
            #[derive(Copy, Clone, Debug, PartialEq)]
            pub enum $k {
                $($i),+
            }

            impl $k {
                #[allow(unused_variables)]
                pub fn into_opcode(self, version: u8, chan: Index) -> Option<Opcode> {
                    match self {
                        $( // instructions
                            $( // opcodes
                                $k::$i if true $(&& matches!(chan, $($c)|+))? => {
                                    let v = ver!($($v)?);
                                    Some(decl!(@opcode v, $o, $(ext = $e,)? $($($n = $x),+)?))
                                },
                            )+
                        )+
                        _ => Option::None,
                    }
                }
            }
        )+

        #[derive(Copy, Clone, Debug, PartialEq)]
        pub enum Desc {
            $($k(u8, $k),)+
        }

        #[derive(Copy, Clone, Debug, PartialEq)]
        pub enum Instr {
            $($k($k, $($t),+),)+
        }

        impl TryFrom<&'_ Raw> for Instr {
            type Error = Error;
            fn try_from(raw: &Raw) -> Result<Self, Error> {
                let ext = Ext::try_from(raw.ales.op()).map_err(|_| Error::UnknownExt)?;
                let desc = match (raw.als.op(), ext) {
                    $( // types
                        $( // instructions
                            $( // opcodes
                                pattern!($o $(,$e)?)
                                if true
                                    $(&& matches!(raw.channel, $($c)|+))? // match channels
                                    $($(&& raw.$n() == $x)+)? // match fields
                                => Desc::$k(ver!($($v)?), $k::$i),
                            )+
                        )+
                    )+
                    _ => return Err(Error::Unknown),
                };
                let instr = match desc {
                    $(
                        Desc::$k(v, o) if v <= raw.version => {
                            Instr::$k(o, $(<$t>::try_from(raw)?),+)
                        },
                    )+
                    _ => return Err(Error::Version(desc)),
                };
                Ok(instr)
            }
        }

        impl Instr {
            pub fn into_raw(self, version: u8, channel: Index) -> Result<Raw, Error> {
                let mut raw = Raw::default();
                let opcode = match self {
                    $( // types
                        Self::$k(o, $($a),+) => {
                            let opcode = o.into_opcode(version, channel).ok_or(Error::OpcodeNotFound)?;
                            $($a.insert_into(&mut raw);)+
                            opcode
                        },
                    )+
                };
                raw.als.set_op(opcode.op);
                if let Some(ext) = opcode.ext {
                    raw.ales.set_op(ext as u8);
                }
                if let Some(src1) = opcode.src1 {
                    raw.als.set_src1(src1);
                }
                if let Some(cmp_op) = opcode.cmp_op {
                    raw.als.set_cmp_op(cmp_op);
                }
                Ok(raw)
            }
        }

        impl Instr {
            pub fn print(&self, fmt: &mut fmt::Formatter, i: usize, sm: bool) -> fmt::Result {
                match self {
                    $(
                        Self::$k(o, $($a),+) => {
                            write!(fmt, "{:?},{}", o, i)?;
                            if sm {
                                fmt.write_str(",sm")?;
                            }
                            fmt.write_str(" ")?;
                            print_args!(fmt, $($a),+);
                            Ok(())
                        }
                    )+
                }
            }
        }
    };
}

use Ext::*;
use Index::*;

decl! {
    Op1(dst: Tst) {
        gettst { 0x24(Ex1)[C3] }
    }
    Op2(src2: Src2, dst: Dst) {
        bitrevs { 0x26[C0,C1,C3,C4] v:2 }
        bitrevd { 0x27[C0,C1,C3,C4] v:2 }
        fstois { 0x3c[C0,C1,C3,C4] }
        fstoistr { 0x3c[C0,C1,C3,C4] if src1 == 0xc2 }
        fdtoid { 0x3d[C0,C1,C3,C4] }
        fdtoidtr { 0x3d[C0,C1,C3,C4] if src1 == 0xc2 }
        fstoid { 0x3e[C0,C1,C3,C4] }
        fstoidtr { 0x3e[C0,C1,C3,C4] if src1 == 0xc2 }
        fdtois { 0x3f[C0,C1,C3,C4] }
        fdtoistr { 0x3f[C0,C1,C3,C4] if src1 == 0xc2 }
        movfi { 0x5c[C1,C4] }
        movx { 0x5f[C0,C1,C3,C4] v:2 }
        movts { 0x60[C0,C1,C3,C4] }
        movtd { 0x61[C0,C1,C3,C4] }
        getpl { 0x63[C0] if src1 == 0xf0 }
        fxsqrtisx { 0x52[C5] }
        fxsqrtidx { 0x53[C5] }
        fxsqrtixx { 0x57[C5] }
        lzcnts { 0x64[C1,C4] v:2 }
        lzcntd { 0x65[C1,C4] v:2 }
        popcnts { 0x66[C1,C4] v:2 }
        popcntd { 0x67[C1,C4] v:2 }
        gettags { 0x08(Ex1)[C2] }
        gettagd { 0x09(Ex1)[C2] }
        gettc { 0x24(Ex1)[C0] }
        puttst { 0x25(Ex1)[C3] }
        invtc { 0x26(Ex1)[C0] }
        fsqrts { 0x4c(Ex1)[C5] }
        fsqrtid { 0x4d(Ex1)[C5] }
        frcps { 0x50(Ex1)[C5] }
        frsqrts { 0x54(Ex1)[C5] }
        movtq { 0x57(Ex1)[C0,C3] } // pair
        getsp { 0x58(Ex1)[C0,C3] if src1 == 0xec }
    }
    Op3(src1: Src1, src2: Src2, dst: Dst) {
        ands { 0x00[C0,C1,C2,C3,C4,C5] }
        andd { 0x01[C0,C1,C2,C3,C4,C5] }
        andns { 0x02[C0,C1,C2,C3,C4,C5] }
        andnd { 0x03[C0,C1,C2,C3,C4,C5] }
        ors { 0x04[C0,C1,C2,C3,C4,C5] }
        ord { 0x05[C0,C1,C2,C3,C4,C5] }
        orns { 0x06[C0,C1,C2,C3,C4,C5] }
        ornd { 0x07[C0,C1,C2,C3,C4,C5] }
        xors { 0x08[C0,C1,C2,C3,C4,C5] }
        xord { 0x09[C0,C1,C2,C3,C4,C5] }
        xorns { 0x0a[C0,C1,C2,C3,C4,C5] }
        xornd { 0x0b[C0,C1,C2,C3,C4,C5] }
        sxt { 0x0c[C0,C1,C2,C3,C4,C5] }
        adds { 0x10[C0,C1,C2,C3,C4,C5] }
        addd { 0x11[C0,C1,C2,C3,C4,C5] }
        subs { 0x12[C0,C1,C2,C3,C4,C5] }
        subd { 0x13[C0,C1,C2,C3,C4,C5] }
        scls { 0x14[C0,C1,C2,C3,C4,C5] }
        scld { 0x15[C0,C1,C2,C3,C4,C5] }
        scrs { 0x16[C0,C1,C2,C3,C4,C5] }
        scrd { 0x17[C0,C1,C2,C3,C4,C5] }
        shls { 0x18[C0,C1,C2,C3,C4,C5] }
        shld { 0x19[C0,C1,C2,C3,C4,C5] }
        shrs { 0x1a[C0,C1,C2,C3,C4,C5] }
        shrd { 0x1b[C0,C1,C2,C3,C4,C5] }
        sars { 0x1c[C0,C1,C2,C3,C4,C5] }
        sard { 0x1d[C0,C1,C2,C3,C4,C5] }
        getfs { 0x1e[C0,C1,C2,C3,C4,C5] }
        getfd { 0x1f[C0,C1,C2,C3,C4,C5] }
        fadds { 0x30[C0,C1,C3,C4], 0x30(Ex2)[C2,C5] v:4 }
        faddd { 0x31[C0,C1,C3,C4], 0x31(Ex2)[C2,C5] v:4 }
        fsubs { 0x32[C0,C1,C3,C4], 0x32(Ex2)[C2,C5] v:4 }
        fsubd { 0x33(Ex2)[C2,C5] v:4, 0x33[C0,C1,C3,C4] }
        fmins { 0x34[C0,C1,C3,C4] }
        fmind { 0x35[C0,C1,C3,C4] }
        fmaxs { 0x36[C0,C1,C3,C4] }
        fmaxd { 0x37[C0,C1,C3,C4] }
        fmuls { 0x38(Ex2)[C2,C5] v:4, 0x38[C0,C1,C3,C4] }
        fmuld { 0x39[C0,C1,C3,C4], 0x39(Ex2)[C2,C5] v:4 }
        fxaddss { 0x40[C0,C1,C3,C4] }
        fxadddd { 0x41[C0,C1,C3,C4] }
        fxaddsx { 0x42[C0,C1,C3,C4] }
        fxadddx { 0x43[C0,C1,C3,C4] }
        fxaddxs { 0x44[C0,C1,C3,C4] }
        fxaddxd { 0x45[C0,C1,C3,C4] }
        fxaddxx { 0x47[C0,C1,C3,C4] }
        fxsubss { 0x48[C0,C1,C3,C4] }
        fxsubdd { 0x49[C0,C1,C3,C4] }
        fxsubsx { 0x4a[C0,C1,C3,C4] }
        fxsubdx { 0x4b[C0,C1,C3,C4] }
        fxsubxs { 0x4c[C0,C1,C3,C4] }
        fxsubxd { 0x4d[C0,C1,C3,C4] }
        fxsubxx { 0x4f[C0,C1,C3,C4] }
        fxrsubss { 0x58[C0,C1,C3,C4] }
        fxrsubdd { 0x59[C0,C1,C3,C4] }
        fxrsubsx { 0x5a[C0,C1,C3,C4] }
        fxrsubdx { 0x5b[C0,C1,C3,C4] }
        fxmulss { 0x50[C0,C1,C3,C4] }
        fxmuldd { 0x51[C0,C1,C3,C4] }
        fxmulsx { 0x52[C0,C1,C3,C4] }
        fxmuldx { 0x53[C0,C1,C3,C4] }
        fxmulxs { 0x54[C0,C1,C3,C4] }
        fxmulxd { 0x55[C0,C1,C3,C4] }
        fxmulxx { 0x57[C0,C1,C3,C4] }
        movif { 0x5e[C1,C4] }
        vfsi { 0x63[C1,C4] }
        udivs { 0x40[C5] }
        udivd { 0x41[C5] }
        sdivs { 0x42[C5] }
        sdivd { 0x43[C5] }
        udivx { 0x44[C5] }
        umodx { 0x45[C5] }
        sdivx { 0x46[C5] }
        smodx { 0x47[C5] }
        fxdivss { 0x48[C5] }
        fxdivdd { 0x49[C5] }
        fxdivsx { 0x4a[C5] }
        fxdivdx { 0x4b[C5] }
        fxdivxs { 0x4c[C5] }
        fxdivxd { 0x4d[C5] }
        fxdivxx { 0x4f[C5] }
        fxsqrtuxx { 0x59[C5] }
        fxsqrtusx { 0x5a[C5] }
        fxsqrtudx { 0x5b[C5] }
        fxsqrttxx { 0x5d[C5] }
        fxsqrttsx { 0x5e[C5] }
        fxsqrttdx { 0x5f[C5] }
        fxdivtss { 0x60[C5] }
        fxdivtdd { 0x61[C5] }
        fxdivtsx { 0x62[C5] }
        fxdivtdx { 0x63[C5] }
        puttags { 0x0a(Ex1)[C2] }
        puttagd { 0x0b(Ex1)[C2] }
        pminub { 0x00(Ex1)[C0,C3] }
        pminsh { 0x01(Ex1)[C0,C3] }
        pmaxub { 0x02(Ex1)[C0,C3] }
        pmaxsh { 0x03(Ex1)[C0,C3] }
        pminsb { 0x04(Ex1)[C0,C3] v:3 }
        pminuh { 0x05(Ex1)[C0,C3] v:3 }
        pmaxsb { 0x06(Ex1)[C0,C3] v:3 }
        pmaxuh { 0x07(Ex1)[C0,C3] v:3 }
        paddb { 0x08(Ex1)[C0,C3] }
        paddh { 0x09(Ex1)[C0,C3] }
        paddsb { 0x0a(Ex1)[C0,C3] }
        paddsh { 0x0b(Ex1)[C0,C3] }
        paddusb { 0x0c(Ex1)[C0,C3] }
        paddush { 0x0d(Ex1)[C0,C3] }
        paddw { 0x0e(Ex1)[C0,C3] }
        paddd { 0x0f(Ex1)[C0,C3] }
        psubb { 0x10(Ex1)[C0,C3] }
        psubh { 0x11(Ex1)[C0,C3] }
        psubsb { 0x12(Ex1)[C0,C3] }
        psubsh { 0x13(Ex1)[C0,C3] }
        psubusb { 0x14(Ex1)[C0,C3] }
        psubush { 0x15(Ex1)[C0,C3] }
        psubw { 0x16(Ex1)[C0,C3] }
        psubd { 0x17(Ex1)[C0,C3] }
        pcmpeqb { 0x18(Ex1)[C0,C3] }
        pcmpeqh { 0x19(Ex1)[C0,C3] }
        pcmpeqw { 0x1a(Ex1)[C0,C3] }
        pcmpgtb { 0x1b(Ex1)[C0,C3] }
        pcmpgth { 0x1c(Ex1)[C0,C3] }
        pcmpgtw { 0x1d(Ex1)[C0,C3] }
        pavgusb { 0x1e(Ex1)[C0,C3] }
        pavgush { 0x1f(Ex1)[C0,C3] }
        punpckhbh { 0x00(Ex1)[C1,C4] }
        punpcklbh { 0x01(Ex1)[C1,C4] }
        punpckhhw { 0x02(Ex1)[C1,C4] }
        punpcklhw { 0x03(Ex1)[C1,C4] }
        punpckhwd { 0x04(Ex1)[C1,C4] }
        punpcklwd { 0x05(Ex1)[C1,C4] }
        pmovmskps { 0x06(Ex1)[C1,C4] }
        pmovmskpd { 0x07(Ex1)[C1,C4] }
        pmovmskb { 0x0b(Ex1)[C1,C4] }
        packsshb { 0x08(Ex1)[C1,C4] }
        packushb { 0x09(Ex1)[C1,C4] }
        packsswh { 0x0a(Ex1)[C1,C4] }
        psrlw { 0x10(Ex2)[C0,C3] v:3, 0x10(Ex1)[C1,C4] }
        psrlh { 0x11(Ex2)[C0,C3] v:3, 0x11(Ex1)[C1,C4] }
        psraw { 0x12(Ex2)[C0,C3] v:3, 0x12(Ex1)[C1,C4] }
        psrah { 0x13(Ex2)[C0,C3] v:3, 0x13(Ex1)[C1,C4] }
        psllw { 0x14(Ex2)[C0,C3] v:3, 0x14(Ex1)[C1,C4] }
        psllh { 0x15(Ex2)[C0,C3] v:3, 0x15(Ex1)[C1,C4] }
        pmulhh { 0x18(Ex1)[C1,C4] }
        pmullh { 0x19(Ex1)[C1,C4] }
        pmaddh { 0x1a(Ex1)[C1,C4] }
        pmulhuh { 0x1b(Ex1)[C1,C4] }
        strd { 0x1b(Ex1)[C2,C5] }
        psadbw { 0x1c(Ex1)[C1,C4] }
        pmulubhh { 0x1d(Ex1)[C1,C4] v:2 }
        muls { 0x20(Ex1)[C0,C1,C3,C4] }
        muld { 0x21(Ex1)[C0,C1,C3,C4] }
        umulx { 0x22(Ex1)[C0,C1,C3,C4] }
        smulx { 0x23(Ex1)[C0,C1,C3,C4] }
        puttc { 0x25(Ex1)[C0] }
        fscales { 0x24(Ex1)[C1,C4] v:4 }
        fscaled { 0x25(Ex1)[C1,C4] v:4 }
        pfadds { 0x30(Ex1)[C0,C1,C3,C4], 0x30(Ex1)[C2,C5] v:4 }
        pfaddd { 0x31(Ex1)[C0,C1,C3,C4], 0x31(Ex1)[C2,C5] v:4 }
        pfsubs { 0x32(Ex1)[C0,C1,C3,C4], 0x32(Ex1)[C2,C5] v:4 }
        pfsubd { 0x33(Ex1)[C0,C1,C3,C4], 0x33(Ex1)[C2,C5] v:4 }
        pfmins { 0x34(Ex1)[C0,C1,C3,C4] }
        pfmind { 0x35(Ex1)[C0,C1,C3,C4] }
        pfmaxs { 0x36(Ex1)[C0,C1,C3,C4] }
        pfmaxd { 0x37(Ex1)[C0,C1,C3,C4] }
        pfmuls { 0x38(Ex1)[C0,C1,C3,C4], 0x38(Ex1)[C2,C5] v:4 }
        pfmuld { 0x39(Ex1)[C0,C1,C3,C4] }
        fdivs { 0x48(Ex1)[C5] }
        fdivd { 0x49(Ex1)[C5] }
        pfdivs { 0x4a(Ex1)[C5] }
        pfdivd { 0x4b(Ex1)[C5] }
        pcmpeqd { 0x4d(Ex1)[C0,C3] v:3 }
        pcmpgtd { 0x4f(Ex1)[C0,C3] v:3 }
        pfsqrts { 0x4e(Ex1)[C5] }
        fsqrttd { 0x51(Ex1)[C5] }
        pfsqrttd { 0x53(Ex1)[C5] }
        pandd { 0x48(Ex1)[C0,C1,C3,C4] }
        pandnd { 0x49(Ex1)[C0,C1,C3,C4] }
        pord { 0x4a(Ex1)[C0,C1,C3,C4] }
        pxord { 0x4b(Ex1)[C0,C1,C3,C4] }
        psrld { 0x4c(Ex1)[C1,C4] }
        pslld { 0x4e(Ex1)[C1,C4] }
        aptoap { 0x50(Ex1)[C0,C3] } // pair
        aptoapb { 0x51(Ex1)[C0,C3] } // pair
        getva { 0x52(Ex1)[C0,C3] }
        mpsadbh { 0x58(Ex1)[C1,C4] v:3 }
        ldrd { 0x5b(Ex1)[C2,C5] }
        pmaddubsh { 0x68(Ex1)[C1,C4] v:3 }
        pmulhrsh { 0x69(Ex1)[C1,C4] v:3 }
        phminposuh { 0x6a(Ex1)[C1,C4] v:3 }
        packuswh { 0x6b(Ex1)[C1,C4] v:3 }
        fstoifs { 0x6c(Ex1)[C0,C1,C3,C4] v:3 }
        fdtoifd { 0x6d(Ex1)[C0,C1,C3,C4] v:3 }
        pfstoifs { 0x6e(Ex1)[C0,C1,C3,C4] v:3 }
        pfdtoifd { 0x6f(Ex1)[C0,C1,C3,C4] v:3 }
        umulhd { 0x70(Ex1)[C0,C1,C3,C4] v:3 }
        smulhd { 0x71(Ex1)[C0,C1,C3,C4] v:3 }
        pfhadds { 0x72(Ex1)[C0,C1,C2,C3,C4,C5] v:3 }
        pfhsubs { 0x73(Ex1)[C0,C1,C2,C3,C4,C5] v:3 }
        pfaddsubs { 0x77(Ex1)[C0,C1,C2,C3,C4,C5] v:3 }
        pminuw { 0x20(Ex2)[C0,C3] v:3 }
        pminsw { 0x21(Ex2)[C0,C3] v:3 }
        pmaxuw { 0x22(Ex2)[C0,C3] v:3 }
        pmaxsw { 0x23(Ex2)[C0,C3] v:3 }
        phaddh { 0x29(Ex2)[C0,C3] v:3 }
        phaddw { 0x2a(Ex2)[C0,C3] v:3 }
        phaddsh { 0x2b(Ex2)[C0,C3] v:3 }
        phsubh { 0x2d(Ex2)[C0,C3] v:3 }
        phsubw { 0x2e(Ex2)[C0,C3] v:3 }
        phsubsh { 0x2f(Ex2)[C0,C3] v:3 }
        psignb { 0x30(Ex2)[C0,C3] v:3 }
        psignh { 0x31(Ex2)[C0,C3] v:3 }
        psignw { 0x32(Ex2)[C0,C3] v:3 }
        ands_fb { 0x00(Ex4)[C0,C3] }
        andns_fb { 0x02(Ex4)[C0,C3] }
        ors_fb { 0x04(Ex4)[C0,C3] }
        orns_fb { 0x06(Ex4)[C0,C3] }
        xors_fb { 0x08(Ex4)[C0,C3] }
        xorns_fb { 0x0a(Ex4)[C0,C3] }
        adds_fb { 0x10(Ex4)[C0,C3] }
        subs_fb { 0x12(Ex4)[C0,C3] }
        umulx_fb { 0x22(Ex4)[C0,C3] }
        smulx_fb { 0x23(Ex4)[C0,C3] }
        ands_fh { 0x00(Ex5)[C0,C3] }
        andns_fh { 0x02(Ex5)[C0,C3] }
        ors_fh { 0x04(Ex5)[C0,C3] }
        orns_fh { 0x06(Ex5)[C0,C3] }
        xors_fh { 0x09(Ex5)[C0,C3] }
        xorns_fh { 0x0a(Ex5)[C0,C3] }
        adds_fh { 0x10(Ex5)[C0,C3] }
        subs_fh { 0x12(Ex5)[C0,C3] }
        umulx_fh { 0x22(Ex5)[C0,C3] }
        smulx_fh { 0x23(Ex5)[C0,C3] }
        ands_fw { 0x00(Ex6)[C0,C3] }
        andns_fw { 0x02(Ex6)[C0,C3] }
        ors_fw { 0x04(Ex6)[C0,C3] }
        orns_fw { 0x06(Ex6)[C0,C3] }
        xors_fw { 0x06(Ex6)[C0,C3] }
        xorns_fw { 0x0a(Ex6)[C0,C3] }
        adds_fw { 0x10(Ex6)[C0,C3] }
        subs_fw { 0x12(Ex6)[C0,C3] }
        umulx_fw { 0x22(Ex6)[C0,C3] }
        smulx_fw { 0x23(Ex6)[C0,C3] }
        andd_fd { 0x01(Ex7)[C0,C3] }
        andnd_fd { 0x03(Ex7)[C0,C3] }
        ord_fd { 0x05(Ex7)[C0,C3] }
        ornd_fd { 0x07(Ex7)[C0,C3] }
        xord_fd { 0x09(Ex7)[C0,C3] }
        xornd_fd { 0x0b(Ex7)[C0,C3] }
        addd_fd { 0x11(Ex7)[C0,C3] }
        subd_fd { 0x13(Ex7)[C0,C3] }
        pfadd_adds { 0x00(Exe)[C0,C1,C3,C4] v:2, 0x00(Exe)[C2,C5] v:4 }
        pfadd_addd { 0x01(Exe)[C2,C5] v:4, 0x01(Exe)[C0,C1,C3,C4] v:2 }
        pfsub_adds { 0x02(Exe)[C0,C1,C3,C4] v:2, 0x02(Exe)[C2,C5] v:4 }
        pfsub_addd { 0x03(Exe)[C2,C5] v:4, 0x03(Exe)[C0,C1,C3,C4] v:2 }
        pfhadd_adds { 0x04(Exe)[C2,C5] v:4, 0x04(Exe)[C0,C1,C3,C4] v:3 }
        pfhsub_adds { 0x06(Exe)[C2,C5] v:4, 0x06(Exe)[C0,C1,C3,C4] v:3 }
        pfmul_adds { 0x08(Exe)[C2,C5] v:4, 0x08(Exe)[C0,C1,C3,C4] }
        pfmul_addd { 0x09(Exe)[C0,C1,C3,C4], 0x09(Exe)[C2,C5] v:4 }
        pfaddsub_adds { 0x0e(Exe)[C0,C1,C3,C4] v:3, 0x0e(Exe)[C2,C5] v:4 }
        pfadd_subs { 0x20(Exe)[C2,C5] v:4, 0x20(Exe)[C0,C1,C3,C4] v:2 }
        pfadd_subd { 0x21(Exe)[C0,C1,C3,C4] v:2, 0x21(Exe)[C2,C5] v:4 }
        pfsub_subs { 0x22(Exe)[C2,C5] v:4, 0x22(Exe)[C0,C1,C3,C4] v:2 }
        pfsub_subd { 0x23(Exe)[C0,C1,C3,C4] v:2, 0x23(Exe)[C2,C5] v:4 }
        pfhadd_subs { 0x24(Exe)[C2,C5] v:4, 0x24(Exe)[C0,C1,C3,C4] v:3 }
        pfhsub_subs { 0x26(Exe)[C0,C1,C3,C4] v:3, 0x26(Exe)[C2,C5] v:4 }
        pfmul_subs { 0x28(Exe)[C0,C1,C3,C4], 0x28(Exe)[C2,C5] v:4 }
        pfmul_subd { 0x29(Exe)[C2,C5] v:4, 0x29(Exe)[C0,C1,C3,C4] }
        pfaddsub_subs { 0x2e(Exe)[C2,C5] v:4, 0x2e(Exe)[C0,C1,C3,C4] v:3 }
        pfadd_hadds { 0x40(Exe)[C0,C1,C3,C4] v:3, 0x40(Exe)[C2,C5] v:4 }
        pfsub_hadds { 0x42(Exe)[C0,C1,C3,C4] v:3, 0x42(Exe)[C2,C5] v:4 }
        pfhadd_hadds { 0x44(Exe)[C0,C1,C3,C4] v:3, 0x44(Exe)[C2,C5] v:4 }
        pfhsub_hadds { 0x46(Exe)[C0,C1,C3,C4] v:3, 0x46(Exe)[C2,C5] v:4 }
        pfmul_hadds { 0x48(Exe)[C2,C5] v:4, 0x48(Exe)[C0,C1,C3,C4] v:3 }
        pfaddsub_hadds { 0x4e(Exe)[C0,C1,C3,C4] v:3, 0x4e(Exe)[C2,C5] v:4 }
        pfadd_hsubs { 0x60(Exe)[C0,C1,C3,C4] v:3, 0x60(Exe)[C2,C5] v:4 }
        pfsub_hsubs { 0x62(Exe)[C2,C5] v:4, 0x62(Exe)[C0,C1,C3,C4] v:3 }
        pfhadd_hsubs { 0x64(Exe)[C0,C1,C3,C4] v:3, 0x64(Exe)[C2,C5] v:4 }
        pfhsub_hsubs { 0x66(Exe)[C2,C5] v:4, 0x66(Exe)[C0,C1,C3,C4] v:3 }
        pfmul_hsubs { 0x68(Exe)[C2,C5] v:4, 0x68(Exe)[C0,C1,C3,C4] v:3 }
        pfaddsub_hsubs { 0x6e(Exe)[C2,C5] v:4, 0x6e(Exe)[C0,C1,C3,C4] v:3 }
        pfadd_rsubs { 0x20(Exf)[C2,C5] v:4, 0x20(Exf)[C0,C1,C3,C4] v:2 }
        pfadd_rsubd { 0x21(Exf)[C0,C1,C3,C4] v:2, 0x21(Exf)[C2,C5] v:4 }
        pfsub_rsubs { 0x22(Exf)[C2,C5] v:4, 0x22(Exf)[C0,C1,C3,C4] v:2 }
        pfsub_rsubd { 0x23(Exf)[C0,C1,C3,C4] v:2, 0x23(Exf)[C2,C5] v:4 }
        pfhadd_rsubs { 0x24(Exf)[C0,C1,C3,C4] v:3, 0x24(Exf)[C2,C5] v:4 }
        pfhsub_rsubs { 0x26(Exf)[C2,C5] v:4, 0x26(Exf)[C0,C1,C3,C4] v:3 }
        pfmul_rsubs { 0x28(Exf)[C2,C5] v:4, 0x28(Exf)[C0,C1,C3,C4] }
        pfmul_rsubd { 0x29(Exf)[C2,C5] v:4, 0x29(Exf)[C0,C1,C3,C4] }
        pfaddsub_rsubs { 0x2e(Exf)[C2,C5] v:4, 0x2e(Exf)[C0,C1,C3,C4] v:3 }
        pshufb { 0x4d(Exf)[C0,C1,C3,C4] v:2 }
        pfadd_addsubs { 0x60(Exf)[C0,C1,C3,C4] v:3, 0x60(Exf)[C2,C5] v:4 }
        pfsub_addsubs { 0x62(Exf)[C0,C1,C3,C4] v:3, 0x62(Exf)[C2,C5] v:4 }
        pfhadd_addsubs { 0x64(Exf)[C0,C1,C3,C4] v:3, 0x64(Exf)[C2,C5] v:4 }
        pfhsub_addsubs { 0x66(Exf)[C2,C5] v:4, 0x66(Exf)[C0,C1,C3,C4] v:3 }
        pfmul_addsubs { 0x68(Exf)[C2,C5] v:4, 0x68(Exf)[C0,C1,C3,C4] v:3 }
        pmerge { 0x6d(Exf)[C0,C1,C3,C4] v:2 }
        pfaddsub_addsubs { 0x6e(Exf)[C0,C1,C3,C4] v:3, 0x6e(Exf)[C2,C5] v:4 }
    }
    Op4(src1: Src1, src2: Src2, src3: Src3, dst: Dst) {
        scls_fb { 0x14(Ex4)[C0,C3] }
        scrs_fb { 0x16(Ex4)[C0,C3] }
        shls_fb { 0x18(Ex4)[C0,C3] }
        shrs_fb { 0x1a(Ex4)[C0,C3] }
        sars_fb { 0x1c(Ex4)[C0,C3] }
        incs_fb { 0x30(Ex4)[C0,C3] }
        decs_fb { 0x32(Ex4)[C0,C3] }
        scls_fh { 0x14(Ex5)[C0,C3] }
        scrs_fh { 0x16(Ex5)[C0,C3] }
        shls_fh { 0x18(Ex5)[C0,C3] }
        shrs_fh { 0x1a(Ex5)[C0,C3] }
        sars_fh { 0x1c(Ex5)[C0,C3] }
        incs_fh { 0x30(Ex5)[C0,C3] }
        decs_fh { 0x32(Ex5)[C0,C3] }
        scls_fw { 0x14(Ex6)[C0,C3] }
        scrs_fw { 0x16(Ex6)[C0,C3] }
        shls_fw { 0x18(Ex6)[C0,C3] }
        shrs_fw { 0x1a(Ex6)[C0,C3] }
        sars_fw { 0x1c(Ex6)[C0,C3] }
        incs_fw { 0x30(Ex6)[C0,C3] }
        decs_fw { 0x32(Ex6)[C0,C3] }
        scld_fd { 0x15(Ex7)[C0,C3] }
        scrd_fd { 0x17(Ex7)[C0,C3] }
        shld_fd { 0x19(Ex7)[C0,C3] }
        shrd_fd { 0x1b(Ex7)[C0,C3] }
        sard_fd { 0x1d(Ex7)[C0,C3] }
        incd_fd { 0x31(Ex7)[C0,C3] }
        decd_fd { 0x33(Ex7)[C0,C3] }
        and_ands { 0x00(Ex8)[C1,C4] }
        and_andd { 0x01(Ex8)[C1,C4] }
        andn_ands { 0x02(Ex8)[C1,C4] }
        andn_andd { 0x03(Ex8)[C1,C4] }
        or_ands { 0x04(Ex8)[C1,C4] }
        or_andd { 0x05(Ex8)[C1,C4] }
        orn_ands { 0x06(Ex8)[C1,C4] }
        orn_andd { 0x07(Ex8)[C1,C4] }
        xor_ands { 0x08(Ex8)[C1,C4] }
        xor_andd { 0x09(Ex8)[C1,C4] }
        xorn_ands { 0x0a(Ex8)[C1,C4] }
        xorn_andd { 0x0b(Ex8)[C1,C4] }
        add_ands { 0x10(Ex8)[C1,C4] }
        add_andd { 0x11(Ex8)[C1,C4] }
        sub_ands { 0x12(Ex8)[C1,C4] }
        sub_andd { 0x13(Ex8)[C1,C4] }
        scl_ands { 0x14(Ex8)[C1,C4] }
        scl_andd { 0x15(Ex8)[C1,C4] }
        scr_ands { 0x16(Ex8)[C1,C4] }
        scr_andd { 0x17(Ex8)[C1,C4] }
        shl_ands { 0x18(Ex8)[C1,C4] }
        shl_andd { 0x19(Ex8)[C1,C4] }
        shr_ands { 0x1a(Ex8)[C1,C4] }
        shr_andd { 0x1b(Ex8)[C1,C4] }
        sar_ands { 0x1c(Ex8)[C1,C4] }
        sar_andd { 0x1d(Ex8)[C1,C4] }
        getf_ands { 0x1e(Ex8)[C1,C4] }
        getf_andd { 0x1f(Ex8)[C1,C4] }
        and_andns { 0x20(Ex8)[C1,C4] }
        and_andnd { 0x21(Ex8)[C1,C4] }
        andn_andns { 0x22(Ex8)[C1,C4] }
        andn_andnd { 0x23(Ex8)[C1,C4] }
        or_andns { 0x24(Ex8)[C1,C4] }
        or_andnd { 0x25(Ex8)[C1,C4] }
        orn_andns { 0x26(Ex8)[C1,C4] }
        orn_andnd { 0x27(Ex8)[C1,C4] }
        xor_andns { 0x28(Ex8)[C1,C4] }
        xor_andnd { 0x29(Ex8)[C1,C4] }
        xorn_andns { 0x2a(Ex8)[C1,C4] }
        xorn_andnd { 0x2b(Ex8)[C1,C4] }
        add_andns { 0x30(Ex8)[C1,C4] }
        add_andnd { 0x31(Ex8)[C1,C4] }
        sub_andns { 0x32(Ex8)[C1,C4] }
        sub_andnd { 0x33(Ex8)[C1,C4] }
        scl_andns { 0x34(Ex8)[C1,C4] }
        scl_andnd { 0x35(Ex8)[C1,C4] }
        scr_andns { 0x36(Ex8)[C1,C4] }
        scr_andnd { 0x37(Ex8)[C1,C4] }
        shl_andns { 0x38(Ex8)[C1,C4] }
        shl_andnd { 0x39(Ex8)[C1,C4] }
        shr_andns { 0x3a(Ex8)[C1,C4] }
        shr_andnd { 0x3b(Ex8)[C1,C4] }
        sar_andns { 0x3c(Ex8)[C1,C4] }
        sar_andnd { 0x3d(Ex8)[C1,C4] }
        getf_andns { 0x3e(Ex8)[C1,C4] }
        getf_andnd { 0x3f(Ex8)[C1,C4] }
        and_ors { 0x40(Ex8)[C1,C4] }
        and_ord { 0x41(Ex8)[C1,C4] }
        andn_ors { 0x42(Ex8)[C1,C4] }
        andn_ord { 0x43(Ex8)[C1,C4] }
        or_ors { 0x44(Ex8)[C1,C4] }
        or_ord { 0x45(Ex8)[C1,C4] }
        orn_ors { 0x46(Ex8)[C1,C4] }
        orn_ord { 0x47(Ex8)[C1,C4] }
        xor_ors { 0x48(Ex8)[C1,C4] }
        xor_ord { 0x49(Ex8)[C1,C4] }
        xorn_ors { 0x4a(Ex8)[C1,C4] }
        xorn_ord { 0x4b(Ex8)[C1,C4] }
        add_ors { 0x50(Ex8)[C1,C4] }
        add_ord { 0x51(Ex8)[C1,C4] }
        sub_ors { 0x52(Ex8)[C1,C4] }
        sub_ord { 0x53(Ex8)[C1,C4] }
        scl_ors { 0x54(Ex8)[C1,C4] }
        scl_ord { 0x55(Ex8)[C1,C4] }
        scr_ors { 0x56(Ex8)[C1,C4] }
        scr_ord { 0x57(Ex8)[C1,C4] }
        shl_ors { 0x58(Ex8)[C1,C4] }
        shl_ord { 0x59(Ex8)[C1,C4] }
        shr_ors { 0x5a(Ex8)[C1,C4] }
        shr_ord { 0x5b(Ex8)[C1,C4] }
        sar_ors { 0x5c(Ex8)[C1,C4] }
        sar_ord { 0x5d(Ex8)[C1,C4] }
        getf_ors { 0x5e(Ex8)[C1,C4] }
        getf_ord { 0x5f(Ex8)[C1,C4] }
        and_orns { 0x60(Ex8)[C1,C4] }
        and_ornd { 0x61(Ex8)[C1,C4] }
        andn_orns { 0x62(Ex8)[C1,C4] }
        andn_ornd { 0x63(Ex8)[C1,C4] }
        or_orns { 0x64(Ex8)[C1,C4] }
        or_ornd { 0x65(Ex8)[C1,C4] }
        orn_orns { 0x66(Ex8)[C1,C4] }
        orn_ornd { 0x67(Ex8)[C1,C4] }
        xor_orns { 0x68(Ex8)[C1,C4] }
        xor_ornd { 0x69(Ex8)[C1,C4] }
        xorn_orns { 0x6a(Ex8)[C1,C4] }
        xorn_ornd { 0x6b(Ex8)[C1,C4] }
        add_orns { 0x70(Ex8)[C1,C4] }
        add_ornd { 0x71(Ex8)[C1,C4] }
        sub_orns { 0x72(Ex8)[C1,C4] }
        sub_ornd { 0x73(Ex8)[C1,C4] }
        scl_orns { 0x74(Ex8)[C1,C4] }
        scl_ornd { 0x75(Ex8)[C1,C4] }
        scr_orns { 0x76(Ex8)[C1,C4] }
        scr_ornd { 0x77(Ex8)[C1,C4] }
        shl_orns { 0x78(Ex8)[C1,C4] }
        shl_ornd { 0x79(Ex8)[C1,C4] }
        shr_orns { 0x7a(Ex8)[C1,C4] }
        shr_ornd { 0x7b(Ex8)[C1,C4] }
        sar_orns { 0x7c(Ex8)[C1,C4] }
        sar_ornd { 0x7d(Ex8)[C1,C4] }
        getf_orns { 0x7e(Ex8)[C1,C4] }
        getf_ornd { 0x7f(Ex8)[C1,C4] }
        and_xors { 0x00(Ex9)[C1,C4] }
        and_xord { 0x01(Ex9)[C1,C4] }
        andn_xors { 0x02(Ex9)[C1,C4] }
        andn_xord { 0x03(Ex9)[C1,C4] }
        or_xors { 0x04(Ex9)[C1,C4] }
        or_xord { 0x05(Ex9)[C1,C4] }
        orn_xors { 0x06(Ex9)[C1,C4] }
        orn_xord { 0x07(Ex9)[C1,C4] }
        xor_xors { 0x08(Ex9)[C1,C4] }
        xor_xord { 0x09(Ex9)[C1,C4] }
        xorn_xors { 0x0a(Ex9)[C1,C4] }
        xorn_xord { 0x0b(Ex9)[C1,C4] }
        add_xors { 0x10(Ex9)[C1,C4] }
        add_xord { 0x11(Ex9)[C1,C4] }
        sub_xors { 0x12(Ex9)[C1,C4] }
        sub_xord { 0x13(Ex9)[C1,C4] }
        scl_xors { 0x14(Ex9)[C1,C4] }
        scl_xord { 0x15(Ex9)[C1,C4] }
        scr_xors { 0x16(Ex9)[C1,C4] }
        scr_xord { 0x17(Ex9)[C1,C4] }
        shl_xors { 0x18(Ex9)[C1,C4] }
        shl_xord { 0x19(Ex9)[C1,C4] }
        shr_xors { 0x1a(Ex9)[C1,C4] }
        shr_xord { 0x1b(Ex9)[C1,C4] }
        sar_xors { 0x1c(Ex9)[C1,C4] }
        sar_xord { 0x1d(Ex9)[C1,C4] }
        getf_xors { 0x1e(Ex9)[C1,C4] }
        getf_xord { 0x1f(Ex9)[C1,C4] }
        and_xorns { 0x20(Ex9)[C1,C4] }
        and_xornd { 0x21(Ex9)[C1,C4] }
        andn_xorns { 0x22(Ex9)[C1,C4] }
        andn_xornd { 0x23(Ex9)[C1,C4] }
        or_xorns { 0x24(Ex9)[C1,C4] }
        or_xornd { 0x25(Ex9)[C1,C4] }
        orn_xorns { 0x26(Ex9)[C1,C4] }
        orn_xornd { 0x27(Ex9)[C1,C4] }
        xor_xorns { 0x28(Ex9)[C1,C4] }
        xor_xornd { 0x29(Ex9)[C1,C4] }
        xorn_xorns { 0x2a(Ex9)[C1,C4] }
        xorn_xornd { 0x2b(Ex9)[C1,C4] }
        add_xorns { 0x30(Ex9)[C1,C4] }
        add_xornd { 0x31(Ex9)[C1,C4] }
        sub_xorns { 0x32(Ex9)[C1,C4] }
        sub_xornd { 0x33(Ex9)[C1,C4] }
        scl_xorns { 0x34(Ex9)[C1,C4] }
        scl_xornd { 0x35(Ex9)[C1,C4] }
        scr_xorns { 0x36(Ex9)[C1,C4] }
        scr_xornd { 0x37(Ex9)[C1,C4] }
        shl_xorns { 0x38(Ex9)[C1,C4] }
        shl_xornd { 0x39(Ex9)[C1,C4] }
        shr_xorns { 0x3a(Ex9)[C1,C4] }
        shr_xornd { 0x3b(Ex9)[C1,C4] }
        sar_xorns { 0x3c(Ex9)[C1,C4] }
        sar_xornd { 0x3d(Ex9)[C1,C4] }
        getf_xorns { 0x3e(Ex9)[C1,C4] }
        getf_xornd { 0x3f(Ex9)[C1,C4] }
        and_rsubs { 0x40(Ex9)[C1,C4] }
        and_rsubd { 0x41(Ex9)[C1,C4] }
        andn_rsubs { 0x42(Ex9)[C1,C4] }
        andn_rsubd { 0x43(Ex9)[C1,C4] }
        or_rsubs { 0x44(Ex9)[C1,C4] }
        or_rsubd { 0x45(Ex9)[C1,C4] }
        orn_rsubs { 0x46(Ex9)[C1,C4] }
        orn_rsubd { 0x47(Ex9)[C1,C4] }
        xor_rsubs { 0x48(Ex9)[C1,C4] }
        xor_rsubd { 0x49(Ex9)[C1,C4] }
        xorn_rsubs { 0x4a(Ex9)[C1,C4] }
        xorn_rsubd { 0x4b(Ex9)[C1,C4] }
        add_rsubs { 0x50(Ex9)[C1,C4] }
        add_rsubd { 0x51(Ex9)[C1,C4] }
        sub_rsubs { 0x52(Ex9)[C1,C4] }
        sub_rsubd { 0x53(Ex9)[C1,C4] }
        scl_rsubs { 0x54(Ex9)[C1,C4] }
        scl_rsubd { 0x55(Ex9)[C1,C4] }
        scr_rsubs { 0x56(Ex9)[C1,C4] }
        scr_rsubd { 0x57(Ex9)[C1,C4] }
        shl_rsubs { 0x58(Ex9)[C1,C4] }
        shl_rsubd { 0x59(Ex9)[C1,C4] }
        shr_rsubs { 0x5a(Ex9)[C1,C4] }
        shr_rsubd { 0x5b(Ex9)[C1,C4] }
        sar_rsubs { 0x5c(Ex9)[C1,C4] }
        sar_rsubd { 0x5d(Ex9)[C1,C4] }
        getf_rsubs { 0x5e(Ex9)[C1,C4] }
        getf_rsubd { 0x5f(Ex9)[C1,C4] }
        and_adds { 0x00(Exa)[C1,C4] }
        and_addd { 0x01(Exa)[C1,C4] }
        andn_adds { 0x02(Exa)[C1,C4] }
        andn_addd { 0x03(Exa)[C1,C4] }
        or_adds { 0x04(Exa)[C1,C4] }
        or_addd { 0x05(Exa)[C1,C4] }
        orn_adds { 0x06(Exa)[C1,C4] }
        orn_addd { 0x07(Exa)[C1,C4] }
        xor_adds { 0x08(Exa)[C1,C4] }
        xor_addd { 0x09(Exa)[C1,C4] }
        xorn_adds { 0x0a(Exa)[C1,C4] }
        xorn_addd { 0x0b(Exa)[C1,C4] }
        add_adds { 0x10(Exa)[C1,C4] }
        add_addd { 0x11(Exa)[C1,C4] }
        sub_adds { 0x12(Exa)[C1,C4] }
        sub_addd { 0x13(Exa)[C1,C4] }
        scl_adds { 0x14(Exa)[C1,C4] }
        scl_addd { 0x15(Exa)[C1,C4] }
        scr_adds { 0x16(Exa)[C1,C4] }
        scr_addd { 0x17(Exa)[C1,C4] }
        shl_adds { 0x18(Exa)[C1,C4] }
        shl_addd { 0x19(Exa)[C1,C4] }
        shr_adds { 0x1a(Exa)[C1,C4] }
        shr_addd { 0x1b(Exa)[C1,C4] }
        sar_adds { 0x1c(Exa)[C1,C4] }
        sar_addd { 0x1d(Exa)[C1,C4] }
        getf_adds { 0x1e(Exa)[C1,C4] }
        getf_addd { 0x1f(Exa)[C1,C4] }
        and_subs { 0x20(Exa)[C1,C4] }
        and_subd { 0x21(Exa)[C1,C4] }
        andn_subs { 0x22(Exa)[C1,C4] }
        andn_subd { 0x23(Exa)[C1,C4] }
        or_subs { 0x24(Exa)[C1,C4] }
        or_subd { 0x25(Exa)[C1,C4] }
        orn_subs { 0x26(Exa)[C1,C4] }
        orn_subd { 0x27(Exa)[C1,C4] }
        xor_subs { 0x28(Exa)[C1,C4] }
        xor_subd { 0x29(Exa)[C1,C4] }
        xorn_subs { 0x2a(Exa)[C1,C4] }
        xorn_subd { 0x2b(Exa)[C1,C4] }
        add_subs { 0x30(Exa)[C1,C4] }
        add_subd { 0x31(Exa)[C1,C4] }
        sub_subs { 0x32(Exa)[C1,C4] }
        sub_subd { 0x33(Exa)[C1,C4] }
        scl_subs { 0x34(Exa)[C1,C4] }
        scl_subd { 0x35(Exa)[C1,C4] }
        scr_subs { 0x36(Exa)[C1,C4] }
        scr_subd { 0x37(Exa)[C1,C4] }
        shl_subs { 0x38(Exa)[C1,C4] }
        shl_subd { 0x39(Exa)[C1,C4] }
        shr_subs { 0x3a(Exa)[C1,C4] }
        shr_subd { 0x3b(Exa)[C1,C4] }
        sar_subs { 0x3c(Exa)[C1,C4] }
        sar_subd { 0x3d(Exa)[C1,C4] }
        getf_subs { 0x3e(Exa)[C1,C4] }
        getf_subd { 0x3f(Exa)[C1,C4] }
        insfs { 0x6c(Exb)[C0,C1,C3,C4] v:4 }
        insfd { 0x6d(Exb)[C0,C1,C3,C4] }
        fadd_adds { 0x00(Exc)[C2,C5] v:4, 0x00(Exc)[C0,C1,C3,C4] v:2 }
        fadd_addd { 0x01(Exc)[C0,C1,C3,C4] v:2, 0x01(Exc)[C2,C5] v:4 }
        fsub_adds { 0x02(Exc)[C2,C5] v:4, 0x02(Exc)[C0,C1,C3,C4] v:2 }
        fsub_addd { 0x03(Exc)[C0,C1,C3,C4] v:2, 0x03(Exc)[C2,C5] v:4 }
        fmul_adds { 0x08(Exc)[C0,C1,C3,C4], 0x08(Exc)[C2,C5] v:4 }
        fmul_addd { 0x09(Exc)[C0,C1,C3,C4], 0x09(Exc)[C2,C5] v:4 }
        fadd_subs { 0x20(Exc)[C0,C1,C3,C4] v:2, 0x20(Exc)[C2,C5] v:4 }
        fadd_subd { 0x21(Exc)[C0,C1,C3,C4] v:2, 0x21(Exc)[C2,C5] v:4 }
        fsub_subs { 0x22(Exc)[C0,C1,C3,C4] v:2, 0x22(Exc)[C2,C5] v:4 }
        fsub_subd { 0x23(Exc)[C0,C1,C3,C4] v:2, 0x23(Exc)[C2,C5] v:4 }
        fmul_subs { 0x28(Exc)[C0,C1,C3,C4], 0x28(Exc)[C2,C5] v:4 }
        fmul_subd { 0x29(Exc)[C2,C5] v:4, 0x29(Exc)[C0,C1,C3,C4] }
        fadd_rsubs { 0x20(Exd)[C2,C5] v:4, 0x20(Exd)[C0,C1,C3,C4] v:2 }
        fadd_rsubd { 0x21(Exd)[C0,C1,C3,C4] v:2, 0x21(Exd)[C2,C5] v:4 }
        fsub_rsubs { 0x22(Exd)[C2,C5] v:4, 0x22(Exd)[C0,C1,C3,C4] v:2 }
        fsub_rsubd { 0x23(Exd)[C2,C5] v:4, 0x23(Exd)[C0,C1,C3,C4] v:2 }
        fmul_rsubs { 0x28(Exd)[C0,C1,C3,C4], 0x28(Exd)[C2,C5] v:4 }
        fmul_rsubd { 0x29(Exd)[C0,C1,C3,C4], 0x29(Exd)[C2,C5] v:4 }
    }
    Op2cmp(src2: Src2, dst: DstPreg) {
        cctopo { 0x24[C0,C1,C3,C4] if cmp_op == 0 }
        cctopb { 0x24[C0,C1,C3,C4] if cmp_op == 1 }
        cctope { 0x24[C0,C1,C3,C4] if cmp_op == 2 }
        cctopbe { 0x24[C0,C1,C3,C4] if cmp_op == 3 }
        cctops { 0x24[C0,C1,C3,C4] if cmp_op == 4 }
        cctopp { 0x24[C0,C1,C3,C4] if cmp_op == 5 }
        cctopl { 0x24[C0,C1,C3,C4] if cmp_op == 6 }
        cctople { 0x24[C0,C1,C3,C4] if cmp_op == 7 }
    }
    Op3cmp(src1: Src1, src2: Src2, dst: DstPreg) {
        cmposb { 0x20[C0,C1,C3,C4] if cmp_op == 0 }
        cmpbsb { 0x20[C0,C1,C3,C4] if cmp_op == 1 }
        cmpesb { 0x20[C0,C1,C3,C4] if cmp_op == 2 }
        cmpbesb { 0x20[C0,C1,C3,C4] if cmp_op == 3 }
        cmpssb { 0x20[C0,C1,C3,C4] if cmp_op == 4 }
        cmppsb { 0x20[C0,C1,C3,C4] if cmp_op == 5 }
        cmplsb { 0x20[C0,C1,C3,C4] if cmp_op == 6 }
        cmplesb { 0x20[C0,C1,C3,C4] if cmp_op == 7 }
        cmpodb { 0x21[C0,C1,C3,C4] if cmp_op == 0 }
        cmpbdb { 0x21[C0,C1,C3,C4] if cmp_op == 1 }
        cmpedb { 0x21[C0,C1,C3,C4] if cmp_op == 2 }
        cmpbedb { 0x21[C0,C1,C3,C4] if cmp_op == 3 }
        cmpsdb { 0x21[C0,C1,C3,C4] if cmp_op == 4 }
        cmppdb { 0x21[C0,C1,C3,C4] if cmp_op == 5 }
        cmpldb { 0x21[C0,C1,C3,C4] if cmp_op == 6 }
        cmpledb { 0x21[C0,C1,C3,C4] if cmp_op == 7 }
        cmpandesb { 0x22[C0,C1,C3,C4] if cmp_op == 2 }
        cmpandssb { 0x22[C0,C1,C3,C4] if cmp_op == 4 }
        cmpandpsb { 0x22[C0,C1,C3,C4] if cmp_op == 5 }
        cmpandlesb { 0x22[C0,C1,C3,C4] if cmp_op == 7 }
        cmpandedb { 0x23[C0,C1,C3,C4] if cmp_op == 2 }
        cmpandsdb { 0x23[C0,C1,C3,C4] if cmp_op == 4 }
        cmpandpdb { 0x23[C0,C1,C3,C4] if cmp_op == 5 }
        cmpandledb { 0x23[C0,C1,C3,C4] if cmp_op == 7 }
        fxcmpeqsb { 0x28[C0,C1,C3,C4] if cmp_op == 0 }
        fxcmpltsb { 0x28[C0,C1,C3,C4] if cmp_op == 1 }
        fxcmplesb { 0x28[C0,C1,C3,C4] if cmp_op == 2 }
        fxcmpuodsb { 0x28[C0,C1,C3,C4] if cmp_op == 3 }
        fxcmpneqsb { 0x28[C0,C1,C3,C4] if cmp_op == 4 }
        fxcmpnltsb { 0x28[C0,C1,C3,C4] if cmp_op == 5 }
        fxcmpnlesb { 0x28[C0,C1,C3,C4] if cmp_op == 6 }
        fxcmpodsb { 0x28[C0,C1,C3,C4] if cmp_op == 7 }
        fxcmpeqdb { 0x29[C0,C1,C3,C4] if cmp_op == 0 }
        fxcmpltdb { 0x29[C0,C1,C3,C4] if cmp_op == 1 }
        fxcmpledb { 0x29[C0,C1,C3,C4] if cmp_op == 2 }
        fxcmpuoddb { 0x29[C0,C1,C3,C4] if cmp_op == 3 }
        fxcmpneqdb { 0x29[C0,C1,C3,C4] if cmp_op == 4 }
        fxcmpnltdb { 0x29[C0,C1,C3,C4] if cmp_op == 5 }
        fxcmpnledb { 0x29[C0,C1,C3,C4] if cmp_op == 6 }
        fxcmpoddb { 0x29[C0,C1,C3,C4] if cmp_op == 7 }
        fxcmpeqxb { 0x2b[C0,C1,C3,C4] if cmp_op == 0 }
        fxcmpltxb { 0x2b[C0,C1,C3,C4] if cmp_op == 1 }
        fxcmplexb { 0x2b[C0,C1,C3,C4] if cmp_op == 2 }
        fxcmpuodxb { 0x2b[C0,C1,C3,C4] if cmp_op == 3 }
        fxcmpneqxb { 0x2b[C0,C1,C3,C4] if cmp_op == 4 }
        fxcmpnltxb { 0x2b[C0,C1,C3,C4] if cmp_op == 5 }
        fxcmpnlexb { 0x2b[C0,C1,C3,C4] if cmp_op == 6 }
        fxcmpodxb { 0x2b[C0,C1,C3,C4] if cmp_op == 7 }
        fcmpeqsb { 0x2e[C0,C1,C3,C4] if cmp_op == 0 }
        fcmpltsb { 0x2e[C0,C1,C3,C4] if cmp_op == 1 }
        fcmplesb { 0x2e[C0,C1,C3,C4] if cmp_op == 2 }
        fcmpuodsb { 0x2e[C0,C1,C3,C4] if cmp_op == 3 }
        fcmpneqsb { 0x2e[C0,C1,C3,C4] if cmp_op == 4 }
        fcmpnltsb { 0x2e[C0,C1,C3,C4] if cmp_op == 5 }
        fcmpnlesb { 0x2e[C0,C1,C3,C4] if cmp_op == 6 }
        fcmpodsb { 0x2e[C0,C1,C3,C4] if cmp_op == 7 }
        fcmpeqdb { 0x2f[C0,C1,C3,C4] if cmp_op == 0 }
        fcmpltdb { 0x2f[C0,C1,C3,C4] if cmp_op == 1 }
        fcmpledb { 0x2f[C0,C1,C3,C4] if cmp_op == 2 }
        fcmpuoddb { 0x2f[C0,C1,C3,C4] if cmp_op == 3 }
        fcmpneqdb { 0x2f[C0,C1,C3,C4] if cmp_op == 4 }
        fcmpnltdb { 0x2f[C0,C1,C3,C4] if cmp_op == 5 }
        fcmpnledb { 0x2f[C0,C1,C3,C4] if cmp_op == 6 }
        fcmpoddb { 0x2f[C0,C1,C3,C4] if cmp_op == 7 }
    }
    Op3mrgc(src1: Src1, src2: Src2, dst: Dst, cond: SrcCond) {
        merges { 0x0e[C0,C1,C2,C3,C4,C5] } // mrgc
        merged { 0x0f[C0,C1,C2,C3,C4,C5] } // mrgc
    }
    Op4mrgc(src1: Src1, src2: Src2, src3: Src3, dst: Dst, cond: SrcCond) {
        merge_ands { 0x0e(Ex8)[C1,C4] } // mrgc
        merge_andd { 0x0f(Ex8)[C1,C4] } // mrgc
        merge_andns { 0x2e(Ex8)[C1,C4] } // mrgc
        merge_andnd { 0x2f(Ex8)[C1,C4] } // mrgc
        merge_ors { 0x4e(Ex8)[C1,C4] } // mrgc
        merge_ord { 0x4f(Ex8)[C1,C4] } // mrgc
        merge_orns { 0x6e(Ex8)[C1,C4] } // mrgc
        merge_ornd { 0x6f(Ex8)[C1,C4] } // mrgc
        merge_xors { 0x0e(Ex9)[C1,C4] } // mrgc
        merge_xord { 0x0f(Ex9)[C1,C4] } // mrgc
        merge_xorns { 0x2e(Ex9)[C1,C4] } // mrgc
        merge_xornd { 0x2f(Ex9)[C1,C4] } // mrgc
        merge_rsubs { 0x4e(Ex9)[C1,C4] } // mrgc
        merge_rsubd { 0x4f(Ex9)[C1,C4] } // mrgc
        merge_adds { 0x0e(Exa)[C1,C4] } // mrgc
        merge_addd { 0x0f(Exa)[C1,C4] } // mrgc
        merge_subs { 0x2e(Exa)[C1,C4] } // mrgc
        merge_subd { 0x2f(Exa)[C1,C4] } // mrgc
    }
    Op3imm8(src2: Src2, src3: Imm8, dst: Dst) {
        pshufh { 0x17(Ex1)[C1,C4] }
    }
    Op4imm8(src1: Src1, src2: Src2, src3: Imm8, dst: Dst) {
        psrlqh { 0x0c(Ex1)[C1,C4] }
        psrlql { 0x0d(Ex1)[C1,C4] }
        psllqh { 0x0e(Ex1)[C1,C4] }
        psllql { 0x0f(Ex1)[C1,C4] }
        pshufw { 0x16(Ex1)[C1,C4] }
        pextrh { 0x1e(Ex1)[C1,C3] }
        pinsh { 0x1f(Ex1)[C1,C3] }
    }
    Op3load(addr: Addr, dst: Dst) {
        ldb { 0x64[C0,C2,C3,C5] }
        ldh { 0x65[C0,C2,C3,C5] }
        ldw { 0x66[C0,C2,C3,C5] }
        ldd { 0x67[C0,C2,C3,C5] }
        ldcsb { 0x68[C0,C2,C3,C5] }
        ldcsh { 0x69[C0,C2,C3,C5] }
        ldcsw { 0x6a[C0,C2,C3,C5] }
        ldcsd { 0x6b[C0,C2,C3,C5] }
        lddsb { 0x6c[C0,C2,C3,C5] }
        lddsh { 0x6d[C0,C2,C3,C5] }
        lddsw { 0x6e[C0,C2,C3,C5] }
        lddsd { 0x6f[C0,C2,C3,C5] }
        ldesb { 0x70[C0,C2,C3,C5] }
        ldesh { 0x71[C0,C2,C3,C5] }
        ldesw { 0x72[C0,C2,C3,C5] }
        ldesd { 0x73[C0,C2,C3,C5] }
        ldfsb { 0x74[C0,C2,C3,C5] }
        ldfsh { 0x75[C0,C2,C3,C5] }
        ldfsw { 0x76[C0,C2,C3,C5] }
        ldfsd { 0x77[C0,C2,C3,C5] }
        ldgsb { 0x78[C0,C2,C3,C5] }
        ldgsh { 0x79[C0,C2,C3,C5] }
        ldgsw { 0x7a[C0,C2,C3,C5] }
        ldgsd { 0x7b[C0,C2,C3,C5] }
        ldssb { 0x7c[C0,C2,C3,C5] }
        ldssh { 0x7d[C0,C2,C3,C5] }
        ldssw { 0x7e[C0,C2,C3,C5] }
        ldssd { 0x7f[C0,C2,C3,C5] }
        stcsq { 0x02[C2,C5] } // pair
        stdsq { 0x03[C2,C5] } // pair
        stesq { 0x04[C2,C5] } // pair
        stfsq { 0x05[C2,C5] } // pair
        stgsq { 0x06[C2,C5] } // pair
        stssq { 0x07[C2,C5] } // pair
        ldcsq { 0x42(Ex1)[C0,C2,C3,C5] } // pair
        lddsq { 0x43(Ex1)[C0,C2,C3,C5] } // pair
        ldesq { 0x44(Ex1)[C0,C2,C3,C5] } // pair
        ldfsq { 0x45(Ex1)[C0,C2,C3,C5] } // pair
        ldgsq { 0x46(Ex1)[C0,C2,C3,C5] } // pair
        ldssq { 0x47(Ex1)[C0,C2,C3,C5] } // pair
        ldcudb { 0x60(Ex1)[C2,C5] }
        ldcudh { 0x61(Ex1)[C2,C5] }
        ldcudw { 0x62(Ex1)[C2,C5] }
        ldcudd { 0x63(Ex1)[C2,C5] }
        ldgdb { 0x64(Ex1)[C2,C5] }
        ldgdh { 0x65(Ex1)[C2,C5] }
        ldgdw { 0x66(Ex1)[C2,C5] }
        ldgdd { 0x67(Ex1)[C2,C5] }
        ldapb { 0x68(Ex1)[C2,C5] }
        ldaph { 0x69(Ex1)[C2,C5] }
        ldapw { 0x6a(Ex1)[C2,C5] }
        ldapd { 0x6b(Ex1)[C2,C5] }
    }
    Op3store(src4: Src4, addr: Addr) {
        stb { 0x24[C2,C5] }
        sth { 0x25[C2,C5] }
        stw { 0x26[C2,C5] }
        std { 0x27[C2,C5] }
        stcsb { 0x28[C2,C5] }
        stcsh { 0x29[C2,C5] }
        stcsw { 0x2a[C2,C5] }
        stcsd { 0x2b[C2,C5] }
        stdsb { 0x2c[C2,C5] }
        stdsh { 0x2d[C2,C5] }
        stdsw { 0x2e[C2,C5] }
        stdsd { 0x2f[C2,C5] }
        stesb { 0x30[C2,C5] }
        stesh { 0x31[C2,C5] }
        stesw { 0x32[C2,C5] }
        stesd { 0x33[C2,C5] }
        stfsb { 0x34[C2,C5] }
        stfsh { 0x35[C2,C5] }
        stfsw { 0x36[C2,C5] }
        stfsd { 0x37[C2,C5] }
        stgsb { 0x38[C2,C5] }
        stgsh { 0x39[C2,C5] }
        stgsw { 0x3a[C2,C5] }
        stgsd { 0x3b[C2,C5] }
        stssb { 0x3c[C2,C5] }
        stssh { 0x3d[C2,C5] }
        stssw { 0x3e[C2,C5] }
        stssd { 0x3f[C2,C5] }
        stgdb { 0x24(Ex1)[C2,C5] }
        stgdh { 0x25(Ex1)[C2,C5] }
        stgdw { 0x26(Ex1)[C2,C5] }
        stgdd { 0x27(Ex1)[C2,C5] }
        stapb { 0x28(Ex1)[C2,C5] }
        staph { 0x29(Ex1)[C2,C5] }
        stapw { 0x2a(Ex1)[C2,C5] }
        stapd { 0x2b(Ex1)[C2,C5] }
        stgdq { 0x39[C2,C5] } // pair
        stapq { 0x3a[C2,C5] } // pair
    }
    Op2rw(src2: Src2, dst: DstState) {
        rws { 0x3c(Ex1)[C0] } // state
        rwd { 0x3d(Ex1)[C0] } // state
    }
    Op2rr(src1: SrcState, dst: Dst) {
        rrs { 0x3e(Ex1)[C0] } // state
        rrd { 0x3f(Ex1)[C0] } // state
    }
    OpAlaod(addr: AddrArray, dst: Dst) {
        ldaab { 0x5c(Ex1)[C2,C5] }
        ldaah { 0x5d(Ex1)[C2,C5] }
        ldaaw { 0x5e(Ex1)[C2,C5] } // aaurrs, mas
        ldaad { 0x5f(Ex1)[C2,C5] } // aaurrd, mas
        ldaaq { 0x7f(Ex1)[C2,C5] } // apurrq, pair, mas
    }
    OpAstore(src4: Src4, addr: AddrArray) {
        staab { 0x1c(Ex1)[C2,C5] }
        staah { 0x1d(Ex1)[C2,C5] }
        staaw { 0x1e(Ex1)[C2,C5] } // apurw, mas
        staad { 0x1f(Ex1)[C2,C5] } // apurwd, mas
        staaq { 0x3f[C2,C5] } // apurwq, pair, mas
    }
}
