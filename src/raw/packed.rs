use super::syllable::{Hs, Ss};
use crate::util;
use thiserror::Error;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum PreDecodeError {
    #[error("Need more {0} bytes to read bundle")]
    NeedMoreBytes(usize),
}

/// A packed bundle.
///
/// The minimum and maximum length of a packed bundle is 8 and 64 bytes.
#[repr(transparent)]
pub struct Packed {
    data: [u8],
}

impl Packed {
    /// Tries to parse a packed bundle from the given bytes.
    ///
    /// Returns the packed bundle and the remaining bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # use e2k_arch::raw::Packed;
    /// let bytes = [0x01, 0x00, 0x00, 0x04, 0x02, 0x01, 0x00, 0x00];
    /// let (packed, tail) = Packed::from_bytes(&bytes)?;
    /// assert_eq!(packed.as_slice(), bytes);
    /// assert_eq!(tail, &[]);
    /// # Ok::<(), e2k_arch::raw::PreDecodeError>(())
    /// ```
    ///
    /// # Errors
    ///
    /// `Error::NeedMoreBytes` will be returned if the `buf` does not have enough data.
    pub fn from_bytes(buf: &[u8]) -> Result<(&Self, &[u8]), PreDecodeError> {
        if buf.is_empty() {
            return Err(PreDecodeError::NeedMoreBytes(8));
        }
        let len = Hs(buf[0] as u32).len();
        if buf.len() < len {
            return Err(PreDecodeError::NeedMoreBytes(len - buf.len()));
        }
        let (data, tail) = buf.split_at(len);
        // safe because Packed has repr(transparent)
        let bundle = unsafe { Self::new_unchecked(data) };
        Ok((bundle, tail))
    }
    /// Wraps the given `slice` as a `Packed` bundle.
    ///
    /// # Safety
    ///
    /// The `slice` must be a valid packed bundle.
    pub unsafe fn new_unchecked(slice: &[u8]) -> &Self {
        &*(slice as *const [u8] as *const Self)
    }
    /// Yields the underlying `[u8]` slice.
    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }
    /// Returns the header syllable.
    pub fn hs(&self) -> Hs {
        Hs(util::u32_from_le_slice(&self.data[0..4]))
    }
    /// Returns the stubs syllable.
    pub fn ss(&self) -> Option<Ss> {
        if self.hs().ss() {
            Some(Ss(util::u32_from_le_slice(&self.data[4..8])))
        } else {
            None
        }
    }
}
