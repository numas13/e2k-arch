use crate::alc::state::Reg;
use crate::raw::syllable::Aas;
use crate::raw::Unpacked;
use core::fmt;

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Area(u8) {
        const MASK = 0x3f;
        const FMT = "area = {}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Index(u8) {
        const MASK = 0x1f;
        const FMT = "ind = {}";
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Dst {
    Reg(Reg),
    Empty,
    Unknown(u8),
}

impl Dst {
    pub const EMPTY: u8 = 0xdf;
    pub fn from_u8(value: u8) -> Self {
        if value == Self::EMPTY {
            Self::Empty
        } else {
            Reg::from_raw(value)
                .map(Self::Reg)
                .unwrap_or(Self::Unknown(value))
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::Empty => Self::EMPTY,
            Self::Reg(reg) => reg.into_raw(),
            Self::Unknown(value) => value,
        }
    }
}

impl Default for Dst {
    fn default() -> Self {
        Self::Empty
    }
}

impl fmt::Display for Dst {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Reg(reg) => fmt::Display::fmt(reg, fmt),
            Self::Empty => fmt.write_str("%empty"),
            Self::Unknown(v) => write!(fmt, "unknown {:02x}", v),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Opcode {
    Movab,
    Movah,
    Movaw,
    Movad,
    Movaq,
    Unknown(u8),
}

impl Opcode {
    pub fn from_raw(value: u8) -> Self {
        match value {
            Aas::OP_MOVAB => Self::Movab,
            Aas::OP_MOVAH => Self::Movah,
            Aas::OP_MOVAW => Self::Movaw,
            Aas::OP_MOVAD => Self::Movad,
            Aas::OP_MOVAQ => Self::Movaq,
            _ => Self::Unknown(value),
        }
    }
    pub fn into_raw(self) -> u8 {
        match self {
            Self::Movab => Aas::OP_MOVAB,
            Self::Movah => Aas::OP_MOVAH,
            Self::Movaw => Aas::OP_MOVAW,
            Self::Movad => Aas::OP_MOVAD,
            Self::Movaq => Aas::OP_MOVAQ,
            Self::Unknown(value) => value,
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Movab => "movab",
            Self::Movah => "movah",
            Self::Movaw => "movaw",
            Self::Movad => "movad",
            Self::Movaq => "movaq",
            Self::Unknown(v) => return write!(fmt, "mova(unknown {})", v),
        };
        fmt.write_str(s)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Instr {
    pub opcode: Opcode,
    pub area: Area,
    pub index: Index,
    pub am: bool,
    pub be: bool,
    pub dst: Dst,
}

impl Instr {
    pub fn from_raw(aas: Aas, dst: u8) -> Self {
        Self {
            opcode: Opcode::from_raw(aas.op()),
            area: Area::new_truncate(aas.area()),
            index: Index::new_truncate(aas.index()),
            am: aas.am(),
            be: aas.be(),
            dst: Dst::from_u8(dst),
        }
    }
    pub fn into_raw(self) -> (Aas, u8) {
        let mut aas = Aas::default();
        aas.set_op(self.opcode.into_raw());
        aas.set_area(self.area.get());
        aas.set_index(self.index.get());
        aas.set_am(self.am);
        aas.set_be(self.be);
        (aas, self.dst.into_raw())
    }
    pub fn print(&self, fmt: &mut fmt::Formatter, channel: usize) -> fmt::Result {
        writeln!(
            fmt,
            "{},{} {}, {}, am = {}, be = {}, {}",
            self.opcode, channel, self.area, self.index, self.am as u8, self.be as u8, self.dst
        )
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Aau {
    channels: [Option<Instr>; 4],
}

impl Aau {
    pub fn unpack_from(bundle: &Unpacked) -> Self {
        let mut channels = [None; 4];
        for (i, instr) in channels.iter_mut().enumerate() {
            let aas = bundle.aas[i];
            if bundle.ss.aas_mask() & 1 << i != 0 && aas.op() != 0 {
                *instr = Instr::from_raw(aas, bundle.aas_dst[i]).into();
            }
        }
        Self { channels }
    }
    pub fn pack_into(&self, bundle: &mut Unpacked) {
        if !self.channels.iter().any(Option::is_some) {
            return;
        }
        for (i, instr) in self.channels.iter().enumerate() {
            if let Some(instr) = instr {
                let (aas, dst) = instr.into_raw();
                bundle.ss.set_aas_mask(bundle.ss.aas_mask() | 1 << i);
                bundle.aas[i] = aas;
                bundle.aas_dst[i] = dst;
            }
        }
    }
}

impl fmt::Display for Aau {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for (i, instr) in self.channels.iter().enumerate() {
            if let Some(instr) = instr {
                instr.print(fmt, i)?;
            }
        }
        Ok(())
    }
}
