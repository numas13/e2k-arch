use crate::state::pred::{Pred, Preg};
use core::fmt::{self, Write};

pub type PredCond = Cond<Pred>;
pub type PregCond = Cond<Preg>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Cond<T> {
    IfTrue(T),
    IfFalse(T),
}

impl<T> Cond<T> {
    pub const fn new(flag: bool, value: T) -> Self {
        if flag {
            Self::IfTrue(value)
        } else {
            Self::IfFalse(value)
        }
    }
    pub fn into_parts(self) -> (bool, T) {
        match self {
            Self::IfTrue(value) => (true, value),
            Self::IfFalse(value) => (false, value),
        }
    }
}

impl<T: fmt::Display> fmt::Display for Cond<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IfTrue(s) => s.fmt(f),
            Self::IfFalse(s) => {
                f.write_char('~')?;
                s.fmt(f)
            }
        }
    }
}
