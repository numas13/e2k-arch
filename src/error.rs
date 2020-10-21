use core::fmt;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Error {
    NeedMoreBytes(usize),
    UnexpectedEnd,
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NeedMoreBytes(n) => write!(fmt, "need {} bytes", n),
            Self::UnexpectedEnd => fmt.write_str("unexpected bundle end"),
        }
    }
}
