use super::{RawInstr, Size, Src1, Src2};
use crate::InsertInto;
use core::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Addr(pub Src1, pub Src2);

impl Addr {
    pub fn display<'a>(&'a self, src1: Size, src2: Size) -> impl fmt::Display + 'a {
        struct Display<'a> {
            addr: &'a Addr,
            src1: Size,
            src2: Size,
        }

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                let src1 = self.addr.0.display(self.src1);
                let src2 = self.addr.1.display(self.src2);
                write!(f, "[ {} + {} ]", src1, src2)
            }
        }

        Display {
            addr: self,
            src1,
            src2,
        }
    }
    pub fn new(raw: &RawInstr, lts: &[Option<u32>; 4]) -> Result<Self, super::src2::DecodeError> {
        let src1 = Src1::from(raw.als.src1());
        let src2 = Src2::new(raw.als.src2(), lts)?;
        Ok(Addr(src1, src2))
    }
}

impl InsertInto<RawInstr> for Addr {
    fn insert_into(self, raw: &mut RawInstr) {
        self.0.insert_into(raw);
        self.1.insert_into(raw);
    }
}
