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
    pub vfdi: bool,
    pub abg: Abg,
    pub abn: Abn,
    pub abp: Abp,
    pub alc: Alc,
}

impl Stubs {
    pub fn from_raw(ss: raw::Ss) -> Option<Self> {
        if ss.ty() {
            return None;
        }
        Some(Stubs {
            eap: ss.eap(),
            bap: ss.bap(),
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
        })
    }
    pub fn display_ap(&self) -> impl fmt::Display {
        struct Display(bool, bool);

        impl fmt::Display for Display {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if self.0 {
                    writeln!(f, "eap")?;
                }
                if self.1 {
                    writeln!(f, "bap")?;
                }
                Ok(())
            }
        }

        Display(self.eap, self.bap)
    }
    pub fn pack_into(self, ss: &mut raw::Ss) {
        ss.set_eap(self.eap);
        ss.set_bap(self.bap);
        ss.set_vfdi(self.vfdi);
        ss.set_abgi(self.abg.abgi);
        ss.set_abgd(self.abg.abgd);
        ss.set_abnf(self.abn.abnf);
        ss.set_abnt(self.abn.abnt);
        ss.set_abpf(self.abp.abpf);
        ss.set_abpt(self.abp.abpt);
        ss.set_alcf(self.alc.alcf);
        ss.set_alct(self.alc.alct);
    }
    pub fn display_rest<'a>(&'a self) -> impl fmt::Display + 'a {
        struct Display<'a>(&'a Stubs);

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if self.0.vfdi {
                    writeln!(f, "vfdi")?;
                }
                let mut print_flag_pair = |s, v0, s0, v1, s1| {
                    if v0 || v1 {
                        writeln!(f, "{} {0}{}={}, {0}{}={}", s, s0, v0 as u8, s1, v1 as u8)?;
                    }
                    Ok(())
                };
                print_flag_pair("abg", self.0.abg.abgi, "i", self.0.abg.abgd, "d")?;
                print_flag_pair("abn", self.0.abn.abnf, "f", self.0.abn.abnt, "t")?;
                print_flag_pair("abp", self.0.abp.abpf, "f", self.0.abp.abpt, "t")?;
                print_flag_pair("alc", self.0.alc.alcf, "f", self.0.alc.alct, "t")?;
                Ok(())
            }
        }

        Display(self)
    }
}
