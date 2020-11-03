pub mod instr;

use self::instr::operand::MergeCond;
use self::instr::{Instr, RawInstr};
use crate::raw;
use crate::state::lit::{LitPart, LitValue};
use crate::state::pred::Pred;
use core::convert::TryFrom;
use core::fmt;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum DecodeError {
    #[error("Failed to decode instruction")]
    InvalidInstr {
        #[from]
        source: self::instr::DecodeError,
    },
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum EncodeError {
    #[error("Failed to encode instruction")]
    InvalidInstr {
        #[from]
        source: self::instr::EncodeError,
    },
    #[error("Failed to place LTS {0}")]
    OccupiedLts(u8),
}

/// A predicate for execution of instructions.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ExecPred {
    IfTrue(Pred),
    IfFalse(Pred),
}

impl ExecPred {
    pub fn from_raw(index: usize, rlp: &raw::Rlp) -> Option<Self> {
        if rlp.check_channel_rlp(index) {
            let pred: Pred = rlp.psrc().into();
            if rlp.invert_mask() & 1 << index % 3 == 0 {
                Some(Self::IfTrue(pred))
            } else {
                Some(Self::IfFalse(pred))
            }
        } else {
            None
        }
    }
    pub fn into_raw(self, index: usize) -> raw::Rlp {
        let i = index % 3;
        let mut rlp = raw::Rlp::default();
        rlp.set_cluster(index >= 3);
        rlp.set_alc_mask(1 << i);
        let pred = match self {
            Self::IfTrue(pred) => pred,
            Self::IfFalse(pred) => {
                rlp.set_invert_mask(1 << i);
                pred
            }
        };
        rlp.set_psrc(pred.into());
        rlp
    }
    pub fn pred(&self) -> &Pred {
        match self {
            Self::IfTrue(pred) => pred,
            Self::IfFalse(pred) => pred,
        }
    }
}

impl fmt::Display for ExecPred {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IfTrue(pred) => write!(f, "{}", pred),
            Self::IfFalse(pred) => write!(f, "~{}", pred),
        }
    }
}

/// A predicate for address modification in synchronous array access.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AmPred(Pred);

impl AmPred {
    pub fn from_raw(index: usize, raw: &raw::Rlp) -> Option<Self> {
        if raw.check_channel_am(index) {
            Pred::try_from(raw.psrc()).ok().map(Self)
        } else {
            None
        }
    }
    pub fn into_raw(self, index: usize) -> raw::Rlp {
        let mut rlp = raw::Rlp::default();
        rlp.set_cluster(index >= 3);
        rlp.set_am(true);
        rlp.set_psrc(self.0.into());
        rlp
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct Channel {
    pub sm: bool,
    pub instr: Instr,
    pub exec_pred: Option<ExecPred>,
    pub am: Option<AmPred>,
}

impl Channel {
    pub fn new(sm: bool, instr: Instr) -> Self {
        Self {
            sm,
            instr,
            exec_pred: None,
            am: None,
        }
    }
    pub fn with_pred(sm: bool, instr: Instr, pred: ExecPred) -> Self {
        Self {
            sm,
            instr,
            exec_pred: Some(pred),
            am: None,
        }
    }
    pub fn from_raw(
        bundle: &raw::Unpacked,
        version: u8,
        index: usize,
    ) -> Result<Self, DecodeError> {
        let als = bundle.als[index];
        let ales = if (index == 2 || index == 5)
            && bundle.hs.is_set_ales(index)
            && bundle.ales[index].is_empty()
        {
            raw::Ales::DEFAULT_25_EXT
        } else {
            bundle.ales[index]
        };
        let mc = bundle
            .rlp_iter()
            .find(|rlp| rlp.check_channel_mrgc(index))
            .map(|rlp| MergeCond::from_raw(index, rlp).unwrap());
        let pred = bundle
            .rlp_iter()
            .find(|rlp| rlp.check_channel_rlp(index))
            .map(|rlp| ExecPred::from_raw(index, rlp).unwrap());
        let am = bundle
            .rlp_iter()
            .find(|rlp| rlp.check_channel_am(index))
            .map(|rlp| AmPred::from_raw(index, rlp).unwrap());
        let raw = RawInstr::new(version, index, als, ales, mc, None);
        let instr = Instr::new(&raw, &bundle.lts)?;
        Ok(Channel {
            sm: als.sm(),
            instr,
            exec_pred: pred,
            am: am,
        })
    }
    pub fn pack_into(
        &self,
        version: u8,
        channel: usize,
        bundle: &mut raw::Unpacked,
    ) -> Result<(), EncodeError> {
        let mut raw = self.instr.into_raw(version, channel)?;
        bundle.hs.set_als_mask(bundle.hs.als_mask() | 1 << channel);
        raw.als.set_sm(self.sm);
        bundle.als[channel as usize] = raw.als;
        if raw.ales.op() != 0 {
            bundle
                .hs
                .set_ales_mask(bundle.hs.ales_mask() | 1 << channel);
            bundle.ales[channel as usize] = raw.ales;
        }
        if let Some(lit) = raw.lit {
            match lit {
                LitValue::F16(l, p, v) => {
                    let loc = l.get() as usize;
                    if bundle.lts[loc].is_some() {
                        return Err(EncodeError::OccupiedLts(l.get()));
                    }
                    match p {
                        LitPart::Lo => {
                            bundle.lts[loc] = bundle.lts[loc].map(|i| (i & 0xffff_0000) | v as u32)
                        }
                        LitPart::Hi => {
                            bundle.lts[loc] =
                                bundle.lts[loc].map(|i| (i & 0x0000_ffff) | (v as u32) << 16)
                        }
                    }
                }
                LitValue::F32(l, v) => {
                    let loc = l.get() as usize;
                    if bundle.lts[loc].is_some() {
                        return Err(EncodeError::OccupiedLts(l.get()));
                    }
                    bundle.lts[loc] = Some(v);
                }
                LitValue::F64(l, v) => {
                    let loc = l.get() as usize;
                    if bundle.lts[loc].is_some() || bundle.lts[loc + 1].is_some() {
                        return Err(EncodeError::OccupiedLts(l.get()));
                    }
                    bundle.lts[loc] = Some(v as u32);
                    bundle.lts[loc + 1] = Some((v >> 32) as u32);
                }
            }
        }
        if let Some(exec_pred) = self.exec_pred {
            if let Some(rlp) = bundle.find_rlp_mut(channel, (*exec_pred.pred()).into()) {
                rlp.set_alc_mask(rlp.alc_mask() | 1 << channel % 3);
                rlp.set_invert_mask(rlp.invert_mask() | 1 << channel % 3);
            } else {
                bundle.push_rlp(exec_pred.into_raw(channel));
            }
        }
        if let Some(am_pred) = self.am {
            if let Some(rlp) = bundle.find_rlp_mut(channel, am_pred.0.into()) {
                rlp.set_am(true);
            } else {
                bundle.push_rlp(am_pred.into_raw(channel))
            }
        }
        Ok(())
    }
    pub fn display<'a>(&'a self, index: u8) -> impl fmt::Display + 'a {
        struct Display<'a> {
            channel: &'a Channel,
            index: u8,
        }

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let instr_display = self.channel.instr.display(self.index, self.channel.sm);
                write!(f, "{}", instr_display)?;
                if let Some(cond) = self.channel.exec_pred {
                    write!(f, " ? {}", cond)?;
                }
                writeln!(f)?;
                if let Instr::OpAload(_, arr, _) | Instr::OpAstore(_, _, arr) = self.channel.instr {
                    if let Some(incr) = arr.incr {
                        write!(f, "incr,{} {}", self.index as u8, incr)?;
                        self.channel
                            .am
                            .map(|am| write!(f, " ? {}", am.0))
                            .transpose()?;
                    }
                }
                Ok(())
            }
        }

        Display {
            channel: self,
            index,
        }
    }
}

#[derive(Clone, Default, PartialEq)]
pub struct Alc {
    pub channels: [Option<Channel>; 6],
}

impl Alc {
    pub fn unpack_from(version: u8, raw: &raw::Unpacked) -> Result<Self, DecodeError> {
        let mut channels = [None; 6];
        for i in (0..6).filter(|i| raw.hs.is_set_als(*i)) {
            channels[i as usize] = Some(Channel::from_raw(raw, version, i)?);
        }
        Ok(Self { channels })
    }
    pub fn insert_into(self, version: u8, raw: &mut raw::Unpacked) -> Result<(), EncodeError> {
        for (i, channel) in self.channels.iter().enumerate() {
            if let Some(channel) = channel {
                channel.pack_into(version, i, raw)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Alc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for (i, channel) in self.channels.iter().enumerate() {
            if let Some(channel) = channel {
                write!(fmt, "{}", channel.display(i as u8))?;
            }
        }
        Ok(())
    }
}
