pub mod addr;
pub mod addr_array;
pub mod dst;
pub mod merge_cond;
pub mod src1;
pub mod src2;

pub use self::addr::Addr;
pub use self::addr_array::AddrArray;
pub use self::dst::Dst;
pub use self::merge_cond::MergeCond;
pub use self::src1::Src1;
pub use self::src2::Src2;
pub use crate::state::reg::Size;

use super::RawInstr;
