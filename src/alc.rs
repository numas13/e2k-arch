pub mod instr;
pub mod state;

use self::instr::operands::SrcCond;
use self::instr::Instr;
use crate::plu::state::Pred;
use crate::raw::syllable::Rlp;
use crate::raw::syllable::{Ales, Als};
use crate::raw::Unpacked;
use crate::{Error, InsertInto};
use core::convert::TryFrom;
use core::fmt;
use num_enum::FromPrimitive;

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

#[derive(Copy, Clone)]
pub struct Raw {
    pub channel: Index,
    pub version: u8,
    pub als: Als,
    pub ales: Ales,
    pub cond: Option<SrcCond>,
}

impl Raw {
    fn src1(&self) -> u8 {
        self.als.src1()
    }
    fn cmp_op(&self) -> u8 {
        self.als.cmp_op()
    }
}

impl Default for Raw {
    fn default() -> Self {
        let mut als = Als::default();
        als.set_src1(0xc0);
        let mut ales = Ales::default();
        ales.set_src3(0xc0);
        Self {
            channel: Index::C0,
            version: 1,
            als,
            ales,
            cond: None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Cond {
    pub invert: bool,
    pub pred: Pred,
}

impl Cond {
    pub fn into_raw(self, index: usize) -> Rlp {
        let mut rlp = Rlp::default();
        rlp.set_cluster(index >= 3);
        rlp.set_alc_mask(1 << index % 3);
        rlp.set_psrc(self.pred.into_raw());
        rlp.set_invert_mask((self.invert as u8) << index % 3);
        rlp
    }
}

impl fmt::Display for Cond {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.invert {
            write!(fmt, "~")?;
        }
        write!(fmt, "{}", self.pred)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Am {
    pub pred: Pred,
}

impl Am {
    pub fn into_raw(self, index: usize) -> Rlp {
        let mut rlp = Rlp::default();
        rlp.set_cluster(index >= 3);
        rlp.set_am(true);
        rlp.set_psrc(self.pred.into_raw());
        rlp
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Channel {
    pub index: Index,
    pub sm: bool,
    pub instr: Instr,
    pub cond: Option<Cond>,
    pub am: Option<Am>,
}

impl Channel {
    fn from_raw(bundle: &Unpacked, i: usize) -> Self {
        let als = bundle.als[i];
        let ales =
            if (i == 2 || i == 5) && bundle.hs.ales_mask() & 1 << i != 0 && bundle.ales[i].0 == 0 {
                Ales(0x01c0)
            } else {
                bundle.ales[i]
            };
        let cond = bundle
            .rlp_iter()
            .find(|rlp| rlp.mrgc() && rlp.check_channel(i))
            .map(|rlp| SrcCond {
                invert: rlp.invert_mask() & 1 << i % 3 != 0,
                pred: Pred::from_raw(rlp.psrc()),
            });
        let raw = Raw {
            channel: Index::from_primitive(i as u8),
            version: u8::MAX,
            als,
            ales,
            cond,
        };
        let instr = Instr::try_from(&raw).unwrap();
        let cond = bundle
            .rlp_iter()
            .find(|rlp| !rlp.mrgc() && rlp.check_channel(i))
            .map(|rlp| Cond {
                invert: rlp.invert_mask() & 1 << i % 3 != 0,
                pred: Pred::from_raw(rlp.psrc()),
            });
        let cond_am = bundle
            .rlp_iter()
            .find(|rlp| !rlp.mrgc() && rlp.check_channel_am(i))
            .map(|rlp| Am {
                pred: Pred::from_raw(rlp.psrc()),
            });
        Channel {
            index: Index::try_from(i as u8).unwrap(),
            sm: als.sm(),
            instr,
            cond,
            am: cond_am,
        }
    }
    fn pack_into(&self, channel: usize, bundle: &mut Unpacked) {
        let raw = self
            .instr
            .into_raw(u8::MAX, Index::from_primitive(channel as u8))
            .unwrap();
        bundle.hs.set_als_mask(bundle.hs.als_mask() | 1 << channel);
        bundle.als[channel] = raw.als;
        if raw.ales.op() != 0 {
            bundle
                .hs
                .set_ales_mask(bundle.hs.ales_mask() | 1 << channel);
            bundle.ales[channel] = raw.ales;
        }

        if let Some(cond) = self.cond {
            if let Some(rlp) = bundle.find_rlp_mut(channel, cond.pred.into_raw()) {
                rlp.set_alc_mask(rlp.alc_mask() | 1 << channel % 3);
                rlp.set_invert_mask(rlp.invert_mask() | 1 << channel % 3);
            } else {
                bundle.push_rlp(cond.into_raw(channel));
            }
        }
        if let Some(am) = self.am {
            if let Some(rlp) = bundle.find_rlp_mut(channel, am.pred.into_raw()) {
                rlp.set_am(true);
            } else {
                bundle.push_rlp(am.into_raw(channel))
            }
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Alc {
    pub channels: [Option<Channel>; 6],
}

impl TryFrom<&'_ Unpacked> for Alc {
    type Error = Error;
    fn try_from(raw: &Unpacked) -> Result<Self, Self::Error> {
        let mut channels = [None; 6];
        for i in 0..6 {
            if raw.hs.als_mask() & 1 << i == 0 {
                continue;
            }
            channels[i] = Some(Channel::from_raw(raw, i));
        }
        Ok(Self { channels })
    }
}

impl InsertInto<Unpacked> for Alc {
    fn insert_into(self, raw: &mut Unpacked) {
        for (i, channel) in self.channels.iter().enumerate() {
            if let Some(channel) = channel {
                channel.pack_into(i, raw);
            }
        }
    }
}

impl Alc {
    pub fn print(&self, lts: &[u32; 4], fmt: &mut fmt::Formatter) -> fmt::Result {
        for i in self.channels.iter().filter_map(|i| *i) {
            instr::print_instr(&i.instr, fmt, i.index as usize, i.sm, lts)?;
            if let Some(cond) = i.cond {
                write!(fmt, " ? {}", cond)?;
            }
            writeln!(fmt)?;
            match i.instr {
                Instr::OpAlaod(_, arr, _) | Instr::OpAstore(_, _, arr) => {
                    if let Some(incr) = arr.incr {
                        write!(fmt, "incr,{} {}", i.index as u8, incr)?;
                        if let Some(cond) = i.am {
                            write!(fmt, " ? {}", cond.pred)?;
                        }
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }
}
