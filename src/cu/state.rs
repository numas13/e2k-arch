use core::fmt;
use core::num::NonZeroU8;

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct Ctpr(NonZeroU8);

impl Ctpr {
    pub const MIN: Ctpr = unsafe { Self::new_unchecked(1) };
    pub const MAX: Ctpr = unsafe { Self::new_unchecked(3) };
    /// Creates a `Ctpr` without checking the given value.
    ///
    /// # Safety
    ///
    /// The value must be in range of 1 to 3.
    pub const unsafe fn new_unchecked(value: u8) -> Self {
        Self(NonZeroU8::new_unchecked(value))
    }
    /// Returns the value as a primitive type.
    pub const fn get(&self) -> u8 {
        self.0.get()
    }
    /// Creates a `Ctpr` if the given value within range of 1 to 3.
    pub const fn new(value: u8) -> Option<Self> {
        if Self::MIN.get() <= value && value <= Self::MAX.get() {
            Some(unsafe { Self::new_unchecked(value) })
        } else {
            None
        }
    }
    /// Creates a `Ctpr` from the given value or clamp it if out of range of 1 to 3.
    pub const fn new_clamp(value: u8) -> Self {
        if value <= Self::MAX.get() {
            if Self::MIN.get() <= value {
                unsafe { Self::new_unchecked(value) }
            } else {
                Self::MIN
            }
        } else {
            Self::MAX
        }
    }
}

impl Default for Ctpr {
    fn default() -> Self {
        Self::MIN
    }
}

impl fmt::Display for Ctpr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "%ctpr{}", self.get())
    }
}
