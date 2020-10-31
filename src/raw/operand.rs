use super::types::Ctpr;
use bitfield::bitfield;

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Based(u8) {
        const MASK = 0x7f;
        const FMT = "%b[{}]";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Regular(u8) {
        const MASK = 0x3f;
        const FMT = "%r{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Global(u8) {
        const MASK = 0x1f;
        const FMT = "%g{}";
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Imm5(u8) {
        const MASK = 0x1f;
    }
}

newtype! {
    #[derive(Copy, Clone, Debug, Default, PartialEq, PartialOrd)]
    pub struct Imm4(u8) {
        const MASK = 0x0f;
    }
}

bitfield_from_into!(Lit, u8);
bitfield! {
    #[derive(Copy, Clone, Default, PartialEq)]
    pub struct Lit(u8);
    pub u8, loc, set_loc: 1, 0;
    pub u8, ty, set_ty: 3, 2;
}

bitfield_from_into!(Operand, u8);
bitfield! {
    #[derive(Copy, Clone, Default, PartialEq)]
    pub struct Operand(u8);
    pub u8, raw_based, set_raw_based: 6, 0;
    pub u8, raw_regular, set_raw_regular: 5, 0;
    pub u8, raw_global, set_raw_global: 3, 0;
    pub u8, src1_raw_imm5, set_src1_raw_imm5: 4, 0;
    pub u8, src2_raw_imm4, set_src2_raw_imm4: 3, 0;
    pub u8, from into Lit, src2_raw_lit, set_src2_raw_lit: 3, 0;
    pub u8, src3_imm8, set_src3_imm8: 7, 0;
    pub u8, dst_raw_ctpr, set_dst_raw_ctpr: 1, 0;
}

impl Operand {
    pub const REGULAR_FLAG: u8 = 0x80;
    pub const REGULAR_FLAG_MASK: u8 = 0xc0;
    pub const GLOBAL_FLAG: u8 = 0xe0;
    pub const SRC1_IMM_FLAG: u8 = 0xc0;
    pub const SRC1_IMM_FLAG_MASK: u8 = 0xe0;
    pub const SRC2_LIT_FLAG_MASK: u8 = 0xf0;
    pub const SRC2_LIT_FLAG: u8 = 0xd0;
    pub const SRC2_IMM_FLAG: u8 = 0xc0;
    pub const SRC2_IMM_FLAG_MASK: u8 = 0xf0;
    pub const DST_TST: u8 = 0xcd;
    pub const DST_TC: u8 = 0xce;
    pub const DST_TCD: u8 = 0xcf;
    pub const DST_CTPR_FLAG: u8 = 0xd0;
    pub const DST_CTPR_FLAG_MASK: u8 = 0xfc;
    pub const DST_EMPTY: u8 = 0xdf;
    pub fn based(&self) -> Option<Based> {
        if self.0.leading_zeros() >= 1 {
            Some(Based(self.raw_based()))
        } else {
            None
        }
    }
    pub fn set_based(&mut self, value: Based) {
        self.0 = 0;
        self.set_raw_based(value.0);
    }
    pub fn regular(&self) -> Option<Regular> {
        if self.0 & Self::REGULAR_FLAG_MASK == Self::REGULAR_FLAG {
            Some(Regular(self.raw_regular()))
        } else {
            None
        }
    }
    pub fn set_regular(&mut self, value: Regular) {
        self.0 = Self::REGULAR_FLAG;
        self.set_raw_regular(value.0);
    }
    pub fn global(&self) -> Option<Global> {
        if self.0.leading_ones() >= 3 {
            Some(Global(self.raw_global()))
        } else {
            None
        }
    }
    pub fn set_global(&mut self, value: Global) {
        self.0 = Self::GLOBAL_FLAG;
        self.set_raw_global(value.0);
    }
    pub fn src1_imm5(&self) -> Option<Imm5> {
        if self.0 & Self::SRC1_IMM_FLAG_MASK == Self::SRC1_IMM_FLAG {
            Some(Imm5(self.src1_raw_imm5()))
        } else {
            None
        }
    }
    pub fn set_src1_imm5(&mut self, value: Imm5) {
        self.0 = Self::SRC1_IMM_FLAG;
        self.set_src1_raw_imm5(value.0);
    }
    pub fn src2_lit(&self) -> Option<Lit> {
        if self.0 & Self::SRC2_LIT_FLAG == Self::SRC2_LIT_FLAG {
            Some(self.src2_raw_lit())
        } else {
            None
        }
    }
    pub fn set_src2_lit(&mut self, lit: Lit) {
        self.0 = Self::SRC2_LIT_FLAG;
        self.set_src2_raw_lit(lit);
    }
    pub fn src2_imm4(&self) -> Option<Imm4> {
        if self.0 & Self::SRC2_IMM_FLAG_MASK == Self::SRC2_IMM_FLAG {
            Some(Imm4(self.src2_raw_imm4()))
        } else {
            None
        }
    }
    pub fn set_src2_imm4(&mut self, value: Imm4) {
        self.0 = Self::SRC2_IMM_FLAG;
        self.set_src2_raw_imm4(value.0);
    }
    pub fn dst_ctpr(&self) -> Option<Ctpr> {
        if self.0 & Self::DST_CTPR_FLAG_MASK == Self::DST_CTPR_FLAG {
            Ctpr::new(self.dst_raw_ctpr())
        } else {
            None
        }
    }
    pub fn set_dst_ctpr(&mut self, value: Ctpr) {
        self.0 = Self::DST_CTPR_FLAG;
        self.set_dst_raw_ctpr(value.get());
    }
    pub fn dst_tst(&self) -> bool {
        self.0 == Self::DST_TST
    }
    pub fn set_dst_tst(&mut self) {
        self.0 = Self::DST_TST
    }
    pub fn dst_tc(&self) -> bool {
        self.0 == Self::DST_TC
    }
    pub fn set_dst_tc(&mut self) {
        self.0 = Self::DST_TC;
    }
    pub fn dst_tcd(&self) -> bool {
        self.0 == Self::DST_TCD
    }
    pub fn set_dst_tcd(&mut self) {
        self.0 = Self::DST_TCD;
    }
    pub fn dst_empty(&self) -> bool {
        self.0 == Self::DST_EMPTY
    }
    pub fn set_dst_empty(&mut self) {
        self.0 = Self::DST_EMPTY
    }
}
