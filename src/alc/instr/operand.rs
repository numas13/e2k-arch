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
pub use crate::raw::operand::{Based, Global, Imm4, Imm5, Regular};
pub use crate::state::lit::{LitLoc16, LitLoc32, LitLoc64, LitPart, LitValue};
pub use crate::state::pred::{Pcnt, Pred, Preg};
pub use crate::state::reg::{Reg, Size};
pub use crate::state::state_reg::StateReg;
pub use crate::state::Ctpr;

use super::RawInstr;

pub type Src3 = Reg;
pub type Src4 = Reg;
pub type DstPreg = Preg;
pub type DstState = StateReg;
pub type SrcState = StateReg;
pub type Imm8 = u8;
