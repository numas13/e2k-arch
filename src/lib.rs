#![cfg_attr(all(not(test), feature = "no_std"), no_std)]

#[macro_use]
mod macros;

pub mod aau;
pub mod alc;
pub mod cu;
pub mod plu;
pub mod raw;

mod bundle;
mod error;
mod util;

pub use crate::bundle::Bundle;
pub use crate::error::Error;

trait InsertInto<T> {
    fn insert_into(self, raw: &mut T);
}

#[cfg(test)]
const TEST_BUNDLES_PATHS: &[&str] = &[
    "test-data/bundle-64-bytes.bin",
    "test-data/bundle-lts-pls-cds.bin",
    "test-data/bundle-align.bin",
    "test-data/bundle-ales25.bin",
    "test-data/bundle-hs-only.bin",
    "test-data/bundle-64bit-lts.bin",
    "test-data/bundle-lts-staab.bin",
];
