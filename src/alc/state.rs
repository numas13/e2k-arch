use core::fmt;
use num_enum::{IntoPrimitive, TryFromPrimitive};

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Based(u8) {
        const MASK = 0x7f;
        const FMT = "%b[{}]";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Regular(u8) {
        const MASK = 0x3f;
        const FMT = "%r{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Global(u8) {
        const MASK = 0x1f;
        const FMT = "%g{}";
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Reg {
    Based(Based),
    Regular(Regular),
    Global(Global),
}

impl Reg {
    pub const REGULAR_FLAG: u8 = 0x80;
    pub const REGULAR_FLAG_MASK: u8 = 0xc0;
    pub const GLOBAL_FLAG: u8 = 0xe0;
    pub const GLOBAL_FLAG_MASK: u8 = 0xe0;

    pub const fn from_raw(value: u8) -> Option<Self> {
        if value.leading_zeros() >= 1 {
            Some(Self::Based(Based::new_truncate(value)))
        } else if value & Self::REGULAR_FLAG_MASK == Self::REGULAR_FLAG {
            Some(Self::Regular(Regular::new_truncate(value)))
        } else if value & Self::GLOBAL_FLAG_MASK == Self::GLOBAL_FLAG {
            Some(Self::Global(Global::new_truncate(value)))
        } else {
            None
        }
    }

    pub const fn into_raw(self) -> u8 {
        match self {
            Self::Based(b) => b.get(),
            Self::Regular(r) => r.get() | Self::REGULAR_FLAG,
            Self::Global(g) => g.get() | Self::GLOBAL_FLAG,
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Based(b) => fmt::Display::fmt(b, fmt),
            Self::Regular(r) => fmt::Display::fmt(r, fmt),
            Self::Global(g) => fmt::Display::fmt(g, fmt),
        }
    }
}

impl From<Based> for Reg {
    fn from(reg: Based) -> Self {
        Self::Based(reg)
    }
}

impl From<Regular> for Reg {
    fn from(reg: Regular) -> Self {
        Self::Regular(reg)
    }
}

impl From<Global> for Reg {
    fn from(reg: Global) -> Self {
        Self::Global(reg)
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum StateReg {
    psr = 0x00,
    wd = 0x01,
    core_mode = 0x04,
    cwd = 0x06,
    psp_hi = 0x07,
    psp_lo = 0x09,
    pshtp = 0x0b,
    pcsp_hi = 0x0d,
    pcsp_lo = 0x0f,
    pcshtp = 0x13,
    ctpr1 = 0x15,
    ctpr2 = 0x16,
    ctpr3 = 0x17,
    sbr = 0x1e,
    cutd = 0x21,
    eir = 0x23,
    // deleted
    tsd = 0x24,
    cuir = 0x25,
    oscud_hi = 0x26,
    oscud_lo = 0x27,
    osgd_hi = 0x28,
    osgd_lo = 0x29,
    osem = 0x2a,
    usd_hi = 0x2c,
    usd_lo = 0x2d,
    // deleted
    tr = 0x2e,
    osr0 = 0x2f,
    cud_hi = 0x30,
    cud_lo = 0x31,
    gd_hi = 0x32,
    gd_lo = 0x33,
    cs_hi = 0x34,
    cs_lo = 0x35,
    ds_hi = 0x36,
    ds_lo = 0x37,
    es_hi = 0x38,
    es_lo = 0x39,
    fs_hi = 0x3a,
    fs_lo = 0x3b,
    gs_hi = 0x3c,
    gs_lo = 0x3d,
    ss_hi = 0x3e,
    ss_lo = 0x3f,
    dibcr = 0x40,
    dimcr = 0x41,
    dibsr = 0x42,
    dtcr = 0x43,
    dibar0 = 0x48,
    dibar1 = 0x49,
    dibar2 = 0x4a,
    dibar3 = 0x4b,
    dimar0 = 0x4c,
    dimar1 = 0x4d,
    dtarf = 0x4e,
    dtart = 0x4f,
    cr0_hi = 0x51,
    cr0_lo = 0x53,
    cr1_hi = 0x55,
    cr1_lo = 0x57,
    sclkm1 = 0x70,
    sclkm2 = 0x71,
    cu_hw0 = 0x78,
    upsr = 0x80,
    ip = 0x81,
    nip = 0x82,
    lsr = 0x83,
    pfpfr = 0x84,
    fpcr = 0x85,
    fpsr = 0x86,
    ilcr = 0x87,
    br = 0x88,
    bgr = 0x89,
    idr = 0x8A,
    clkr = 0x90,
    rndpr = 0x91,
    sclkr = 0x92,
    tir_hi = 0x9C,
    tir_lo = 0x9D,
    rpr = 0xA0,
    sbbp = 0xA1,
    rpr_hi = 0xA2,
    upsrm = 0xC0,
}

impl fmt::Display for StateReg {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::psr => "%psr",
            Self::wd => "%wd",
            Self::core_mode => "%core_mode",
            Self::cwd => "%cwd",
            Self::psp_hi => "%psp.hi",
            Self::psp_lo => "%psp.lo",
            Self::pshtp => "%pshtp",
            Self::pcsp_hi => "%pcsp.hi",
            Self::pcsp_lo => "%pcsp.lo",
            Self::pcshtp => "%pcshtp",
            Self::ctpr1 => "%ctpr1",
            Self::ctpr2 => "%ctpr2",
            Self::ctpr3 => "%ctpr3",
            Self::sbr => "%sbr",
            Self::cutd => "%cutd",
            Self::eir => "%eir",
            Self::tsd => "%tsd",
            Self::cuir => "%cuir",
            Self::oscud_hi => "%oscud.hi",
            Self::oscud_lo => "%oscud.lo",
            Self::osgd_hi => "%osgd.hi",
            Self::osgd_lo => "%osgd.lo",
            Self::osem => "%osem",
            Self::usd_hi => "%usd.hi",
            Self::usd_lo => "%usd.lo",
            Self::tr => "%tr",
            Self::osr0 => "%osr0",
            Self::cud_hi => "%cud.hi",
            Self::cud_lo => "%cud.lo",
            Self::gd_hi => "%gd.hi",
            Self::gd_lo => "%gd.lo",
            Self::cs_hi => "%cs.hi",
            Self::cs_lo => "%cs.lo",
            Self::ds_hi => "%ds.hi",
            Self::ds_lo => "%ds.lo",
            Self::es_hi => "%es.hi",
            Self::es_lo => "%es.lo",
            Self::fs_hi => "%fs.hi",
            Self::fs_lo => "%fs.lo",
            Self::gs_hi => "%gs.hi",
            Self::gs_lo => "%gs.lo",
            Self::ss_hi => "%ss.hi",
            Self::ss_lo => "%ss.lo",
            Self::dibcr => "%dibcr",
            Self::dimcr => "%dimcr",
            Self::dibsr => "%dibsr",
            Self::dtcr => "%dtcr",
            Self::dibar0 => "%dibar0",
            Self::dibar1 => "%dibar1",
            Self::dibar2 => "%dibar2",
            Self::dibar3 => "%dibar3",
            Self::dimar0 => "%dimar0",
            Self::dimar1 => "%dimar1",
            Self::dtarf => "%dtarf",
            Self::dtart => "%dtart",
            Self::cr0_hi => "%cr0.hi",
            Self::cr0_lo => "%cr0.lo",
            Self::cr1_hi => "%cr1.hi",
            Self::cr1_lo => "%cr1.lo",
            Self::sclkm1 => "%sclkm1",
            Self::sclkm2 => "%sclkm2",
            Self::cu_hw0 => "%cu_hw0",
            Self::upsr => "%upsr",
            Self::ip => "%ip",
            Self::nip => "%nip",
            Self::lsr => "%lsr",
            Self::pfpfr => "%pfpfr",
            Self::fpcr => "%fpcr",
            Self::fpsr => "%fpsr",
            Self::ilcr => "%ilcr",
            Self::br => "%br",
            Self::bgr => "%bgr",
            Self::idr => "%idr",
            Self::clkr => "%clkr",
            Self::rndpr => "%rndpr",
            Self::sclkr => "%sclkr",
            Self::tir_hi => "%tir.hi",
            Self::tir_lo => "%tir.lo",
            Self::rpr => "%rpr",
            Self::sbbp => "%sbbp",
            Self::rpr_hi => "%rpr.hi",
            Self::upsrm => "%upsrm",
        };
        fmt.write_str(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reg_creation() {
        assert_eq!(Reg::from_raw(0x2c), Some(Based::new_truncate(44).into()));
        assert_eq!(Reg::from_raw(0x9c), Some(Regular::new_truncate(28).into()));
        assert_eq!(Reg::from_raw(0xef), Some(Global::new_truncate(15).into()));
        assert_eq!(Reg::from(Based::new_truncate(71)).into_raw(), 0x47);
        assert_eq!(Reg::from(Regular::new_truncate(45)).into_raw(), 0xad);
        assert_eq!(Reg::from(Global::new_truncate(7)).into_raw(), 0xe7);
    }
}
