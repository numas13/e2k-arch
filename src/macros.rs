macro_rules! newtype {
    (
        $(#[$meta:meta])*
        $vis:vis struct $name:ident($ty_vis:vis $ty:ty) {
            $($body:tt)*
        }
    ) => (
        $(#[$meta])*
        $vis struct $name($ty_vis $ty);

        impl $name {
            /// Creates a newtype wrapper without checking the given value.
            ///
            /// # Safety
            ///
            /// The value must be in the range.
            pub const unsafe fn new_unchecked(value: $ty) -> Self {
                Self(value)
            }
            /// Returns the value as a primitive type.
            pub const fn get(&self) -> $ty {
                self.0
            }
        }

        newtype! { @body $name, $ty, $($body)* }
    );
    (@body $name:ident, $ty:ty, const MASK = $mask:literal; $($rest:tt)*) => (
        impl $name {
            pub const MASK: Self = Self($mask);

            pub const fn new_truncate(value: $ty) -> Self {
                Self(value & Self::MASK.get())
            }
            /// Creates a newtype wrapper if the given value within the range.
            pub const fn new(value: $ty) -> Option<Self> {
                if value & !Self::MASK.get() == 0 {
                    Some(Self(value))
                } else {
                    None
                }
            }
            /// Creates a newtype wrapper from the given value or clamp it if out of
            /// the range.
            pub const fn new_clamp(value: $ty) -> Self {
                if value & !Self::MASK.get() == 0 {
                    Self(value)
                } else {
                    Self::MASK
                }
            }
        }
        newtype! { @body $name, $ty, $($rest)* }
    );
    (@body $name:ident, $ty:ty, const RANGE = $min:literal..=$max:expr; $($rest:tt)*) => (
        impl $name {
            pub const MIN: Self = Self($min);
            pub const MAX: Self = Self($max);

            /// Creates a newtype wrapper if the given value within the range.
            pub const fn new(value: $ty) -> Option<Self> {
                if Self::MIN.get() <= value && value <= Self::MAX.get() {
                    Some(Self(value))
                } else {
                    None
                }
            }
            /// Creates a newtype wrapper from the given value or clamp it if out of
            /// the range.
            pub const fn new_clamp(value: $ty) -> Self {
                if value <= Self::MAX.get() {
                    if Self::MIN.get() <= value {
                        Self(value)
                    } else {
                        Self::MIN
                    }
                } else {
                    Self::MAX
                }
            }
        }

        newtype! { @body $name, $ty, $($rest)* }
    );
    (@body $name:ident, $ty:ty, const FMT = $fmt:literal; $($rest:tt)*) => (
        impl core::fmt::Display for $name {
            fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(fmt, $fmt, self.get())
            }
        }
        newtype! { @body $name, $ty, $($rest)* }
    );
    (@body $name:ident, $ty:ty, ) => ();
}

macro_rules! bitfield_from_into {
    ($t:ty, $r:ty) => {
        impl From<$r> for $t {
            fn from(value: $r) -> Self {
                Self(value)
            }
        }

        impl Into<$r> for $t {
            fn into(self) -> $r {
                self.0
            }
        }
    };
}
