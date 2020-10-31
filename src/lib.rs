#![cfg_attr(not(any(test, feature = "std")), no_std)]

#[cfg(not(feature = "std"))]
compile_error!("currently do not support build with no std feature");

#[macro_use]
mod macros;

pub mod aau;
pub mod alc;
pub mod cu;
pub mod plu;
pub mod raw;
pub mod state;

mod bundle;
mod util;

pub use crate::aau::Aau;
pub use crate::alc::Alc;
pub use crate::bundle::Bundle;
pub use crate::cu::Cu;
pub use crate::plu::Plu;

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
    "test-data/bundle-setwd-lts.bin",
];
