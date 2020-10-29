//! Low-level representations of syllables.

use bitfield::bitfield;

bitfield! {
    /// A header syllable.
    ///
    /// The header comes as first and required syllable of every bundle.
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Hs(u32);
    /// An offset to a middle of bundle in 32-bit units minus one.
    pub u8, raw_offset, set_raw_offset: 3, 0;
    /// A size of bundle in 64-bit units minus one.
    pub u8, raw_len, set_raw_len: 6, 4;
    /// A latency of a bundle minus one.
    ///
    /// Increasing the value cause bubbles between bundles because CPU executes them in-order. It
    /// can be useful if a bundle depends on a long latency operation of previous bundle to
    /// prevent a hardware hazard detection.
    pub u8, raw_nop, set_raw_nop: 9, 7;
    /// A loop mode execution.
    pub loop_mode, set_loop_mode: 10;
    /// TODO
    pub sim, set_sim: 11;
    /// A presence of stubs syllable.
    pub ss, set_ss: 12;
    /// TODO
    pub mdl, set_mdl: 13;
    /// A bitmask of control commands syllables.
    pub u8, cs_mask, set_cs_mask: 15, 14;
    /// A presence of a 1st control commands syllable.
    pub cs0, set_cs0: 14;
    /// A presence of a 2nd control commands syllable.
    pub cs1, set_cs1: 15;
    /// A count of conditional execution syllables.
    pub u8, cds_len, set_cds_len: 17, 16;
    /// A count of logical predicate processing syllables.
    pub u8, pls_len, set_pls_len: 19, 18;
    /// A bitmask of half-syllable extensions for arithmetic logic channels.
    pub u8, ales_mask, set_ales_mask: 25, 20;
    /// A presence of `ALES0`.
    pub ales0, set_ales0: 20;
    /// A presence of `ALES1`.
    pub ales1, set_ales1: 21;
    /// A presence of `ALES2`.
    ///
    /// Note that, actual presence of `ALES2` depends on instruction because till version 4
    /// the half-syllable was not defined but the bit was used as an additional opcode bit.
    /// Since version 4 such instructions maps to `0x01` extension opcode but the actual
    /// presence of the half-syllable in such cases is not required until `ALES5` used
    /// for instruction in channel 5.
    pub ales2, set_ales2: 22;
    /// A presence of `ALES3`.
    pub ales3, set_ales3: 23;
    /// A presence of `ALES4`.
    pub ales4, set_ales4: 24;
    /// A presence of `ALES5`.
    ///
    /// Note that, actual presence of `ALES5` depends on instruction because till version 4
    /// the half-syllable was not defined but the bit was used as an additional opcode bit.
    /// Since version 4 such instructions maps to `0x01` extension opcode but the actual
    /// presence of the half-syllable in such cases is not required until `ALES2` used
    /// for instruction in channel 2.
    pub ales5, set_ales5: 25;
    /// A bitmask of arithmetic logic channels syllables.
    pub u8, als_mask, set_als_mask: 31, 26;
    /// A presence of `ALS0`.
    pub als0, set_als0: 26;
    /// A presence of `ALS1`.
    pub als1, set_als1: 27;
    /// A presence of `ALS2`.
    pub als2, set_als2: 28;
    /// A presence of `ALS3`.
    pub als3, set_als3: 29;
    /// A presence of `ALS4`.
    pub als4, set_als4: 30;
    /// A presence of `ALS5`.
    pub als5, set_als5: 31;
}

impl Hs {
    pub const SS_BIT: u32 = 1 << 12;
    pub const CS_MASK: u32 = 3 << 14;
    pub const CS0_BIT: u32 = 1 << 14;
    pub const ALS_MASK: u32 = 0x3f << 26;

    /// Returns the offset in bytes to a middle of bundle.
    pub fn offset(&self) -> usize {
        self.raw_offset() as usize * 4 + 4
    }

    /// Sets the offset to a middle of bundle.
    ///
    /// # Panics
    ///
    /// * Panics if `offset` is greater than 44.
    /// * Panics if `offset` is not multiples of 4.
    pub fn set_offset(&mut self, offset: usize) {
        assert!(offset <= 44 && offset.trailing_zeros() >= 2);
        self.set_raw_offset(offset as u8 / 4 - 1)
    }

    /// Returns the length of bundle.
    pub fn len(&self) -> usize {
        self.raw_len() as usize * 8 + 8
    }

    /// Sets the length of bundle to `new_len`.
    ///
    /// # Panics
    ///
    /// * Panics if `new_len` is greater than 64.
    /// * Panics if `new_len` is not multiples of 8.
    pub fn set_len(&mut self, new_len: usize) {
        assert!(new_len <= 64 && new_len.trailing_zeros() >= 3);
        self.set_raw_len(new_len as u8 / 8 - 1)
    }

    /// Returns `true` if a bundle has `ALES2+5` syllable.
    pub fn is_ales25(&self) -> bool {
        (self.0 & (Self::SS_BIT | Self::ALS_MASK | Self::CS_MASK)).count_ones()
            < self.raw_offset() as u32
    }

    /// Returns the offset to `ALES2+5` syllable.
    pub fn ales25_offset(&self) -> usize {
        4 + (self.0 & (Self::SS_BIT | Self::ALS_MASK | Self::CS0_BIT)).count_ones() as usize * 4
    }
}

bitfield! {
    /// The syllable for short instructions.
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Ss(u32);
    /// A predicate register for a conditional control transfer.
    pub u8, ct_pred, set_ct_pred: 4, 0;
    /// A control transfer opcode.
    pub u8, ct_op, set_ct_op: 8, 5;
    // TODO: SS[9]
    /// A pipeline register for a control transfer.
    pub u8, ct_ctpr, set_ct_ctpr: 11, 10;
    /// A bitmask of half-syllables for arrays access units.
    pub u8, aas_mask, set_aas_mask: 15, 12;
    /// A presence of `AAS0` and `AAS2`.
    pub aas2, set_aas2: 12;
    /// A presence of `AAS0` and `AAS3`.
    pub aas3, set_aas3: 13;
    /// A presence of `AAS1` and `AAS4`.
    pub aas4, set_aas4: 14;
    /// A presence of `AAS1` and `AAS5`.
    pub aas5, set_aas5: 15;
    /// Increment a cycle counter if branch was taken.
    pub alct, set_alct: 16;
    /// Increment a cycle counter if branch was not taken.
    pub alcf, set_alcf: 17;
    /// Increment a base for rotated predicate registers if branch was taken.
    pub abpt, set_abpt: 18;
    /// Increment a base for rotated predicate registers if branch was not taken.
    pub abpf, set_abpf: 19;
    // TODO: SS[20]
    /// Increment a base for rotated registers if branch was taken.
    pub abnt, set_abnt: 21;
    /// Increment a base for rotated registers if branch was not taken.
    pub abnf, set_abnf: 22;
    /// Decrement a base for rotated global registers.
    pub abgd, set_abgd: 23;
    /// Increment a base for rotated global registers.
    pub abgi, set_abgi: 24;
    // TODO: SS[25]
    /// TODO
    pub vfdi, set_vfdi: 26;
    /// TODO
    pub srp, set_srp: 27;
    /// Begin an arrays async prefetch.
    pub bap, set_bap: 28;
    /// End an arrays async prefetch.
    pub eap, set_eap: 29;
    /// TODO: Instruction prefetch depth.
    pub u8, ipd, set_ipd: 31, 30;
}

impl Ss {
    pub const CT_OP_NONE: u8 = 0;
    pub const CT_OP_EXPLICIT: u8 = 1;
    pub const CT_OP_PREG: u8 = 2;
    pub const CT_OP_NOT_PREG: u8 = 3;
    pub const CT_OP_LOOP_END: u8 = 4;
    pub const CT_OP_NOT_LOOP_END: u8 = 5;
    pub const CT_OP_PREG_OR_LOOP_END: u8 = 6;
    pub const CT_OP_NOT_PREG_AND_NOT_LOOP_END: u8 = 7;
    pub const CT_OP_MLOCK: u8 = 8;

    /// A bitmask of `AAS0` and `AAS1`.
    pub fn aas_dst_mask(&self) -> u8 {
        (self.aas_mask() & 5) | (self.aas_mask() >> 1 & 5)
    }
}

bitfield! {
    /// A syllable for arithmetic logic channels.
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Als(u32);
    /// A destination predicate register for compare instructions.
    pub u8, cmp_dst, set_cmp_dst: 4, 0;
    /// An opcode for compare instructions.
    pub u8, cmp_op, set_cmp_op: 7, 5;
    /// Used as 1st operand for memory write instructions.
    ///
    /// | Value         | Meaning           |
    /// |---------------|-------------------|
    /// | `0x00..=0x7f` | `%b[0]..=%b[127]` |
    /// | `0x80..=0xbf` | `%r0..=%r63`      |
    /// | `0xe0..=0xff` | `%g0..=%g31`      |
    pub u8, src4, set_src4: 7, 0;
    /// Used as a destination for most instructions.
    ///
    /// | Value         | Meaning           |
    /// |---------------|-------------------|
    /// | `0x00..=0x7f` | `%b[0]..=%b[127]` |
    /// | `0x80..=0xbf` | `%r0..=%r63`      |
    /// | `0xcd`        | `%tst`            |
    /// | `0xce`        | `%tc`             |
    /// | `0xcf`        | `%tcd`            |
    /// | `0xd1..=0xd3` | `%ctpr1..=%ctpr3` |
    /// | `0xbf`        | `%empty`          |
    /// | `0xe0..=0xff` | `%g0..=%g31`      |
    pub u8, dst, set_dst: 7, 0;
    /// Depending on an instruction can be:
    /// * 1st operand if `src1` is used as an additional opcode.
    /// * 2nd operand for most instructions.
    /// * 3rd operand for memory write instructions.
    ///
    /// | Value         | Meaning                                   |
    /// |---------------|-------------------------------------------|
    /// | `0x00..=0x7f` | `%b[0]..=%b[127]`                         |
    /// | `0x80..=0xbf` | `%r0..=%r63`                              |
    /// | `0xc0..=0xcf` | `imm4`                                    |
    /// | `0xd0..=0xd1` | 16-bit `lit` loc at low half of `LTS`     |
    /// | `0xd4..=0xd5` | 16-bit `lit` loc at high half of `LTS`    |
    /// | `0xd8..=0xdb` | 32-bit `lit` loc                          |
    /// | `0xdc..=0xde` | 64-bit `lit` loc                          |
    /// | `0xe0..=0xff` | `%g0..=%g31`                              |
    pub u8, src2, set_src2: 15, 8;
    /// Depending on an instruction can be:
    /// * 1st operand for most instructions.
    /// * 2nd operand for memory write instructions.
    /// * An additional opcode for some instructions that do not need this operand.
    ///
    /// | Value         | Meaning                           |
    /// |---------------|-----------------------------------|
    /// | `0x00..=0x7f` | `%b[0]..=%b[127]`                 |
    /// | `0x80..=0xbf` | `%r0..=%r63`                      |
    /// | `0xc0..=0xdf` | `imm5` or an additional opcode    |
    /// | `0xe0..=0xff` | `%g0..=%g31`                      |
    pub u8, src1, set_src1: 23, 16;
    // sync array access
    pub u8, aa_lts, set_aa_lts: 9, 8;
    pub aa_inc, set_aa_inc: 10;
    pub aa_is_ind, set_aa_is_ind: 11;
    pub u8, aa_incr, set_aa_incr: 14, 12;
    pub u8, aa_ind, set_aa_ind: 18, 15;
    pub u8, aa_sti, set_aa_sti: 18, 15;
    pub u8, aa_aad, set_aa_aad: 23, 19;
    /// An opcode.
    pub u8, op, set_op: 30, 24;
    /// A speculative mode execution.
    pub sm, set_sm: 31;
}

impl Als {
    pub fn aa_is_sti(&self) -> bool {
        !self.aa_is_ind()
    }
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Cs0(u32);
    // pref
    pub u8, pref_ipr, set_pref_ipr: 2, 0;
    pub pref_ipd, set_pref_ipd: 3;
    pub pref_disp, set_pref_disp: 27, 4;
    // done
    pub done_fdam, set_done_fdam: 26;
    pub done_trar, set_done_trar: 27;
    // disp, ldisp, sdisp, ibranch, puttsd
    pub u32, disp, set_disp: 27, 0;
    pub u8, op, set_op: 29, 28;
    pub u8, ctpr, set_ctpr: 31, 30;
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Cs1(u32);
    // call
    pub u8, wbs, set_wbs: 6, 0;
    // setbn
    pub u8, rbs, set_rbs: 5, 0;
    pub u8, rsz, set_rsz: 11, 6;
    pub u8, rcur, set_rcur: 17, 12;
    // setbp
    pub u8, psz, set_psz: 22, 18;
    // vfbg
    pub u8, umask, set_umask: 7, 0;
    pub u8, dmask, set_dmask: 15, 8;
    pub chkm4, set_chkm4: 16;
    // TODO: Cs1::vfrpsz in LTS0
    // TODO: Cs1::settr in LTS0
    // setei
    pub u8, ei, set_ei: 7, 0;
    // setmas
    // loads and stores with mas cannot be combined
    pub u8, mas5, set_mas5: 6, 0;
    pub u8, mas3, set_mas3: 13, 7;
    pub u8, mas2, set_mas2: 20, 14;
    pub u8, mas0, set_mas0: 27, 21;
    // wait
    pub all_c, set_all_c: 0;
    pub all_e, set_all_e: 1;
    pub st_c, set_st_c: 2;
    pub ld_c, set_ld_c: 3;
    pub fl_c, set_fl_c: 4;
    pub ma_c, set_ma_c: 5;
    pub trap, set_trap: 6;
    // flush
    pub flushr, set_flushr: 0;
    pub flushc, set_flushc: 1;

    pub settr, set_settr: 25;
    pub setbn, set_setbn: 26;
    pub setbp, set_setbp: 27;
    pub vfrpsz, set_vfrpsz: 28;

    pub u8, op5, set_op5: 31, 27;
    pub u8, op4, set_op4: 31, 28;
    pub u8, op3, set_op3: 31, 29;
}

impl Cs1 {
    pub const OP5_SETEI: u8 = 0x04;
    pub const OP5_SETSFT: u8 = 0x05;
    pub const OP5_WAIT: u8 = 0x06;
    pub const OP4_SETBP_SETBN: u8 = 0x04;
    pub const OP4_CALL: u8 = 0x05;
    pub const OP4_SETMAS: u8 = 0x06;
    pub const OP4_FLUSH: u8 = 0x07;
    pub const OP4_VFBG: u8 = 0x08;
    pub const OP3_SETWD: u8 = 0x00;

    pub fn is_lts0(&self) -> bool {
        self.0 & 0xe000_0000 == 0
    }
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Cs1Lts0(u32);
    // TODO: Cs1Lts0[2:0]
    // setwd
    pub dbl, set_dbl: 3;
    pub nfx, set_nfx: 4;
    pub u8, wsz, set_wsz: 11, 5;
    // vfrpsz
    pub u8, rpsz, set_rpsz: 16, 12;
    // settype
    pub u16, ty, set_ty: 31, 17;

}

bitfield! {
    /// The half-syllable extension for arithmetic logic channels.
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Ales(u16);
    /// Depending on an instruction can be:
    /// * A register see table below.
    /// * An 8-bit immediate value.
    /// * Must be `0xc0` if do not used.
    ///
    /// | Value         | Meaning                           |
    /// |---------------|-----------------------------------|
    /// | `0x00..=0x7f` | `%b[0]..=%b[127]`                 |
    /// | `0x80..=0xbf` | `%r0..=%r63`                      |
    /// | `0xe0..=0xff` | `%g0..=%g31`                      |
    pub u8, src3, set_src3: 7, 0;
    /// An extension opcode.
    pub u8, op, set_op: 15, 8;
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct AasDst(u16);
    pub u8, dst1, set_dst1: 7, 0;
    pub u8, dst0, set_dst0: 15, 8;
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Aas(u16);
    pub am, set_am: 0;
    pub u8, index, set_index: 5, 1;
    pub u8, area, set_area: 11, 6;
    pub u8, op, set_op: 14, 12;
    pub be, set_be: 15;
}

impl Aas {
    pub const OP_NONE: u8 = 0;
    pub const OP_MOVAB: u8 = 1;
    pub const OP_MOVAH: u8 = 2;
    pub const OP_MOVAW: u8 = 3;
    pub const OP_MOVAD: u8 = 4;
    pub const OP_MOVAQ: u8 = 5;
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Lts(u32);
    pub u16, lo, set_lo: 15, 0;
    pub u16, hi, set_hi: 31, 16;
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Pls(u32);
    pub u16, from into Clp, clp, set_clp: 15 ,0;
    pub u8, from into Elp, elp1, set_elp1: 23, 16;
    pub u8, from into Elp, elp0, set_elp0: 31, 24;
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Clp(u16);
    pub u8, pred, set_pred: 4, 0;
    pub write, set_write: 5;
    pub u8, lpsrc1, set_lpsrc1: 9, 6;
    pub u8, lpsrc0, set_lpsrc0: 13, 10;
    pub u8, op, set_op: 15, 14;
}

impl Clp {
    pub const OP_ANDP: u8 = 0;
    pub const OP_LANDP: u8 = 1;
    pub const OP_MOVEP: u8 = 3;
}

impl From<u16> for Clp {
    fn from(value: u16) -> Clp {
        Clp(value)
    }
}

impl Into<u16> for Clp {
    fn into(self) -> u16 {
        self.0
    }
}

// TODO: Elp
bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Elp(u8);
}

impl From<u8> for Elp {
    fn from(value: u8) -> Elp {
        Elp(value)
    }
}

impl Into<u8> for Elp {
    fn into(self) -> u8 {
        self.0
    }
}

// reverse order
#[derive(Copy, Clone, Default, Eq, PartialEq)]
#[repr(transparent)]
pub struct Cds {
    pub rlp: [Rlp; 2],
}

impl Cds {
    pub fn from_raw(raw: u32) -> Self {
        Self {
            rlp: [Rlp((raw >> 16) as u16), Rlp(raw as u16)],
        }
    }
    pub fn into_raw(self) -> u32 {
        (self.rlp[0].0 as u32) << 16 | self.rlp[1].0 as u32
    }
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    #[repr(transparent)]
    pub struct Rlp(u16);
    pub u8, psrc, set_psrc: 6, 0;
    pub u8, invert_mask, set_invert_mask: 9, 7;
    pub invert0, set_invert0: 7;
    pub invert1, set_invert1: 8;
    pub invert2, set_invert2: 9;
    pub u8, alc_mask, set_alc_mask: 12, 10;
    pub alc0, set_alc0: 10;
    pub alc1, set_alc1: 11;
    pub alc2, set_alc2: 12;
    pub am, set_am: 13;
    pub cluster, set_cluster: 14;
    pub mrgc, set_mrgc: 15;
}

impl Rlp {
    pub fn is_some(&self) -> bool {
        self.0 != 0
    }
    pub fn is_none(&self) -> bool {
        self.0 == 0
    }
    pub fn check_channel(&self, i: usize) -> bool {
        self.cluster() == (i >= 3) && self.alc_mask() & 1 << i % 3 != 0
    }
    pub fn check_channel_am(&self, i: usize) -> bool {
        self.cluster() == (i >= 3) && ((i == 2 || i == 5) && self.am())
    }
}
