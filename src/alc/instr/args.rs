use super::operands::*;
use super::Raw;
use crate::{Error, InsertInto};
use core::convert::TryFrom;
use core::fmt;

macro_rules! args {
    ( $($i:ident = $t:ty;)+ ) => (
        $(pub type $i = $t;)+

        pub enum Args { $($i($i)),+ }

        $(impl From<$t> for Args {
            fn from(args: $t) -> Self {
                Self::$i(args)
            }
        })+

        impl fmt::Display for Args {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    $(Self::$i(args) => fmt::Display::fmt(args, fmt)),+
                }
            }
        }
    );
}

args! {
    Args2 = Args5<Src2, Dst>;
    Args3 = Args5<Src1, Src2, Dst>;
    Args4 = Args5<Src1, Src2, Src3, Dst>;
    Args3i = Args5<Src2, Imm8, Dst>;
    Args4i = Args5<Src1, Src2, Imm8, Dst>;
    Args3ld = Args5<Addr, Dst>;
    Args3st = Args5<Src4, Addr>;
    Args2c = Args5<Src2, DstPreg>;
    Args3c = Args5<Src1, Src2, DstPreg>;
}

pub trait Displayable: fmt::Display {
    fn is_displayable() -> bool {
        true
    }
}

impl Displayable for Imm8 {}
impl Displayable for Src1 {}
impl Displayable for Src2 {}
impl Displayable for Src3 {}
impl Displayable for Src4 {}
impl Displayable for Addr {}
impl Displayable for Dst {}
impl Displayable for DstPreg {}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Empty;

impl Displayable for Empty {
    fn is_displayable() -> bool {
        false
    }
}

impl fmt::Display for Empty {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}

impl TryFrom<&'_ Raw> for Empty {
    type Error = Error;
    fn try_from(_: &Raw) -> Result<Self, Self::Error> {
        Ok(Empty)
    }
}

impl InsertInto<Raw> for Empty {
    fn insert_into(self, _: &mut Raw) {}
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Args5<A0, A1 = Empty, A2 = Empty, A3 = Empty, A4 = Empty> {
    pub arg0: A0,
    pub arg1: A1,
    pub arg2: A2,
    pub arg3: A3,
    pub arg4: A4,
}

impl<A0, A1, A2, A3, A4> Args5<A0, A1, A2, A3, A4> {
    pub fn new1(arg0: A0) -> Self
    where
        A1: Default,
        A2: Default,
        A3: Default,
        A4: Default,
    {
        Self::new2(arg0, A1::default())
    }

    pub fn new2(arg0: A0, arg1: A1) -> Self
    where
        A2: Default,
        A3: Default,
        A4: Default,
    {
        Self::new3(arg0, arg1, A2::default())
    }

    pub fn new3(arg0: A0, arg1: A1, arg2: A2) -> Self
    where
        A3: Default,
        A4: Default,
    {
        Self::new4(arg0, arg1, arg2, A3::default())
    }

    pub fn new4(arg0: A0, arg1: A1, arg2: A2, arg3: A3) -> Self
    where
        A4: Default,
    {
        Self::new5(arg0, arg1, arg2, arg3, A4::default())
    }

    pub fn new5(arg0: A0, arg1: A1, arg2: A2, arg3: A3, arg4: A4) -> Self {
        Self {
            arg0,
            arg1,
            arg2,
            arg3,
            arg4,
        }
    }
}

impl<'a, A0, A1, A2, A3, A4> TryFrom<&'a Raw> for Args5<A0, A1, A2, A3, A4>
where
    A0: TryFrom<&'a Raw, Error = Error>,
    A1: TryFrom<&'a Raw, Error = Error>,
    A2: TryFrom<&'a Raw, Error = Error>,
    A3: TryFrom<&'a Raw, Error = Error>,
    A4: TryFrom<&'a Raw, Error = Error>,
{
    type Error = Error;
    fn try_from(value: &'a Raw) -> Result<Self, Self::Error> {
        Ok(Self {
            arg0: A0::try_from(value)?,
            arg1: A1::try_from(value)?,
            arg2: A2::try_from(value)?,
            arg3: A3::try_from(value)?,
            arg4: A4::try_from(value)?,
        })
    }
}

impl<A0, A1, A2, A3, A4> InsertInto<Raw> for Args5<A0, A1, A2, A3, A4>
where
    A0: InsertInto<Raw>,
    A1: InsertInto<Raw>,
    A2: InsertInto<Raw>,
    A3: InsertInto<Raw>,
    A4: InsertInto<Raw>,
{
    fn insert_into(self, raw: &mut Raw) {
        self.arg0.insert_into(raw);
        self.arg1.insert_into(raw);
        self.arg2.insert_into(raw);
        self.arg3.insert_into(raw);
        self.arg4.insert_into(raw);
    }
}

impl<A0, A1, A2, A3, A4> fmt::Display for Args5<A0, A1, A2, A3, A4>
where
    A0: fmt::Display,
    A1: Displayable,
    A2: Displayable,
    A3: Displayable,
    A4: Displayable,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.arg0)?;
        if A1::is_displayable() {
            write!(fmt, ", {}", self.arg1)?;
        }
        if A2::is_displayable() {
            write!(fmt, ", {}", self.arg2)?;
        }
        if A3::is_displayable() {
            write!(fmt, ", {}", self.arg3)?;
        }
        if A4::is_displayable() {
            write!(fmt, ", {}", self.arg4)?;
        }
        Ok(())
    }
}
