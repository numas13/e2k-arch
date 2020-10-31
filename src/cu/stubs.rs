use crate::raw;
use core::fmt;

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Abg {
    pub abgi: bool,
    pub abgd: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Abn {
    pub abnf: bool,
    pub abnt: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Abp {
    pub abpf: bool,
    pub abpt: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Alc {
    pub alcf: bool,
    pub alct: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Stubs {
    pub eap: bool,
    pub bap: bool,
    pub srp: bool,
    pub vfdi: bool,
    pub abg: Abg,
    pub abn: Abn,
    pub abp: Abp,
    pub alc: Alc,
}

impl From<raw::Ss> for Stubs {
    fn from(ss: raw::Ss) -> Self {
        Stubs {
            eap: ss.eap(),
            bap: ss.bap(),
            srp: ss.srp(),
            vfdi: ss.vfdi(),
            abg: Abg {
                abgi: ss.abgi(),
                abgd: ss.abgd(),
            },
            abn: Abn {
                abnf: ss.abnf(),
                abnt: ss.abnt(),
            },
            abp: Abp {
                abpf: ss.abpf(),
                abpt: ss.abpt(),
            },
            alc: Alc {
                alcf: ss.alcf(),
                alct: ss.alct(),
            },
        }
    }
}

impl Into<raw::Ss> for Stubs {
    fn into(self) -> raw::Ss {
        let mut ss = raw::Ss::default();
        ss.set_eap(self.eap);
        ss.set_bap(self.bap);
        ss.set_srp(self.srp);
        ss.set_vfdi(self.vfdi);
        ss.set_abgi(self.abg.abgi);
        ss.set_abgd(self.abg.abgd);
        ss.set_abnf(self.abn.abnf);
        ss.set_abnt(self.abn.abnt);
        ss.set_abpf(self.abp.abpf);
        ss.set_abpt(self.abp.abpt);
        ss.set_alcf(self.alc.alcf);
        ss.set_alct(self.alc.alct);
        ss
    }
}

impl fmt::Display for Stubs {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.eap {
            writeln!(fmt, "eap")?;
        }
        if self.bap {
            writeln!(fmt, "bap")?;
        }
        if self.srp {
            writeln!(fmt, "srp")?;
        }
        if self.vfdi {
            writeln!(fmt, "vfdi")?;
        }
        let mut print_flag_pair = |s, v0, s0, v1, s1| {
            if v0 || v1 {
                writeln!(fmt, "{} {0}{}={}, {0}{}={}", s, s0, v0 as u8, s1, v1 as u8)?;
            }
            Ok(())
        };
        print_flag_pair("abg", self.abg.abgi, "i", self.abg.abgd, "d")?;
        print_flag_pair("abn", self.abn.abnf, "f", self.abn.abnt, "t")?;
        print_flag_pair("abp", self.abp.abpf, "f", self.abp.abpt, "t")?;
        print_flag_pair("alc", self.alc.alcf, "f", self.alc.alct, "t")?;
        Ok(())
    }
}
