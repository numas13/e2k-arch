mod operands;
pub mod state;

pub use operands::*;

use crate::plu::state::Pred;
use crate::raw::syllable::{Ales, Als, Rlp};
use crate::raw::Unpacked;
use crate::Error;
use core::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct ShortInstr {
    pub op: u8,
    pub src1: u8,
    pub src2: u8,
    pub dst: u8,
    pub ale: bool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct LongInstr {
    pub op1: u8,
    pub op2: u8,
    pub src1: u8,
    pub src2: u8,
    pub src3: u8,
    pub dst: u8,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Instr {
    Short(ShortInstr),
    Long(LongInstr),
}

impl Instr {
    pub fn pack_into(self, bundle: &mut Unpacked, sm: bool, index: usize) {
        let mut als = Als::default();
        als.set_sm(sm);
        match self {
            Self::Short(i) => {
                als.set_op(i.op);
                als.set_src1(i.src1);
                als.set_src2(i.src2);
                als.set_dst(i.dst);
                if i.ale {
                    bundle.hs.set_ales_mask(bundle.hs.ales_mask() | 1 << index);
                }
            }
            Self::Long(i) => {
                als.set_op(i.op1);
                als.set_src1(i.src1);
                als.set_src2(i.src2);
                als.set_dst(i.dst);
                let mut ales = Ales::default();
                ales.set_op(i.op2);
                ales.set_src3(i.src3);
                bundle.hs.set_ales_mask(bundle.hs.ales_mask() | 1 << index);
                bundle.ales[index] = Some(ales);
            }
        }
        bundle.hs.set_als_mask(bundle.hs.als_mask() | 1 << index);
        bundle.als[index] = als;
    }
    fn print(&self, i: usize, sm: bool, fmt: &mut fmt::Formatter) -> fmt::Result {
        let sm = if sm { ",sm" } else { "" };
        match self {
            Self::Short(s) => {
                write!(fmt, "alc_unknown({:02x}),{}{}", s.op, i, sm)?;
                write!(fmt, " {:#02x}, {:#02x}, {:#02x}", s.src1, s.src2, s.dst)
            }
            Self::Long(l) => {
                write!(fmt, "alc_unknown({:02x}:{:02x}),{}{}", l.op2, l.op1, i, sm)?;
                write!(
                    fmt,
                    " {:#02x}, {:#02x}, {:#02x}, {:#02x}",
                    l.src1, l.src2, l.src3, l.dst
                )
            }
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

impl fmt::Display for Cond {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.invert {
            write!(fmt, "~")?;
        }
        write!(fmt, "{}", self.pred)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Channel {
    pub sm: bool,
    pub instr: Instr,
    pub cond: Option<Cond>,
    pub am: Option<Am>,
}

impl Channel {
    fn from_raw(bundle: &Unpacked, i: usize) -> Self {
        let als = bundle.als[i];
        let instr = if let Some(ales) = bundle.ales[i] {
            if ales.0 != 0 {
                Instr::Long(LongInstr {
                    op1: als.op(),
                    op2: ales.op(),
                    src1: als.src1(),
                    src2: als.src2(),
                    src3: ales.src3(),
                    dst: als.dst(),
                })
            } else {
                Instr::Short(ShortInstr {
                    op: als.op(),
                    src1: als.src1(),
                    src2: als.src2(),
                    dst: als.dst(),
                    ale: bundle.hs.ales_mask() & 1 << i != 0,
                })
            }
        } else {
            Instr::Short(ShortInstr {
                op: als.op(),
                src1: als.src1(),
                src2: als.src2(),
                dst: als.dst(),
                ale: bundle.hs.ales_mask() & 1 << i != 0,
            })
        };
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
            sm: als.sm(),
            instr,
            cond,
            am: cond_am,
        }
    }
    fn pack_into(&self, channel: usize, bundle: &mut Unpacked) {
        self.instr.pack_into(bundle, self.sm, channel);
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
    fn print(&self, i: usize, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.instr.print(i, self.sm, fmt)?;
        if let Some(cond) = self.cond {
            write!(fmt, " ? {}", cond)?;
        }
        writeln!(fmt)
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Alc {
    pub channels: [Option<Channel>; 6],
}

impl Alc {
    pub fn from_raw(bundle: &Unpacked) -> Result<Self, Error> {
        let mut channels = [None; 6];
        for i in 0..6 {
            if bundle.hs.als_mask() & 1 << i == 0 {
                continue;
            }
            channels[i] = Some(Channel::from_raw(bundle, i));
        }
        Ok(Self { channels })
    }
    pub fn pack_into(&self, bundle: &mut Unpacked) {
        for (i, channel) in self.channels.iter().enumerate() {
            if let Some(channel) = channel {
                channel.pack_into(i, bundle);
            }
        }
    }
}

impl fmt::Display for Alc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for (i, channel) in self.channels.iter().enumerate() {
            if let Some(channel) = channel {
                channel.print(i, fmt)?;
            }
        }
        Ok(())
    }
}
