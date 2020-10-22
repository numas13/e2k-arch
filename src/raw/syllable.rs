use bitfield::bitfield;

pub const HS_SS_BIT: u32 = 1 << 12;
pub const HS_CS_MASK: u32 = 3 << 14;
pub const HS_CS0_BIT: u32 = 1 << 14;
pub const HS_ALS_MASK: u32 = 0x3f << 26;

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Hs(u32);
    pub u8, raw_offset, set_raw_offset: 3, 0;
    pub u8, raw_len, set_raw_len: 6, 4;
    pub u8, raw_nop, set_raw_nop: 9, 7;
    pub loop_mode, set_loop_mode: 10;
    pub sim, set_sim: 11;
    pub ss, set_ss: 12;
    pub mdl, set_mdl: 13;
    pub u8, cs_mask, set_cs_mask: 15, 14;
    pub cs0, set_cs0: 14;
    pub cs1, set_cs1: 15;
    pub u8, cds_len, set_cds_len: 17, 16;
    pub u8, pls_len, set_pls_len: 19, 18;
    pub u8, ales_mask, set_ales_mask: 25, 20;
    pub ales0, set_ales0: 20;
    pub ales1, set_ales1: 21;
    pub ales2, set_ales2: 22;
    pub ales3, set_ales3: 23;
    pub ales4, set_ales4: 24;
    pub ales5, set_ales5: 25;
    pub u8, als_mask, set_als_mask: 31, 26;
    pub als0, set_als0: 26;
    pub als1, set_als1: 27;
    pub als2, set_als2: 28;
    pub als3, set_als3: 29;
    pub als4, set_als4: 30;
    pub als5, set_als5: 31;
}

impl Hs {
    /// Returns the offset in bytes to a middle of the bundle.
    pub fn offset(&self) -> usize {
        self.raw_offset() as usize * 4 + 4
    }

    /// Sets the offset to a middle of the bundle.
    ///
    /// # Panics
    ///
    /// * Panics if `offset` is greater than 44.
    /// * Panics if `offset` is not multiples of 4.
    pub fn set_offset(&mut self, offset: usize) {
        assert!(offset <= 44 && offset & 3 == 0);
        self.set_raw_offset(offset as u8 / 4 - 1)
    }

    /// Returns the length of the bundle.
    pub fn len(&self) -> usize {
        self.raw_len() as usize * 8 + 8
    }

    /// Sets the length of the bundle to `new_len`.
    ///
    /// # Panics
    ///
    /// * Panics if `new_len` is greater than 64.
    /// * Panics if `new_len` is not multiples of 8.
    pub fn set_len(&mut self, new_len: usize) {
        assert!(new_len <= 64 && new_len & 7 == 0);
        self.set_raw_len(new_len as u8 / 8 - 1)
    }

    /// Returns `true` if the bundle has `ALES2+5` syllable.
    pub fn is_ales25(&self) -> bool {
        (self.0 & (HS_SS_BIT | HS_ALS_MASK | HS_CS_MASK)).count_ones() < self.raw_offset() as u32
    }

    /// Returns the offset to `ALES2+5` syllable.
    pub fn ales25_offset(&self) -> usize {
        4 + (self.0 & (HS_SS_BIT | HS_ALS_MASK | HS_CS0_BIT)).count_ones() as usize * 4
    }
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Ss(u32);
    pub u8, ct_pred, set_ct_pred: 4, 0;
    pub u8, ct_op, set_ct_op: 8, 5;
    // TODO: SS[9]
    pub u8, ct_ctpr, set_ct_stpr: 11, 10;
    pub u8, aas_mask, set_aas_mask: 15, 12;
    pub aas2, set_aas2: 12;
    pub aas3, set_aas3: 13;
    pub aas4, set_aas4: 14;
    pub aas5, set_aas5: 15;
    pub alct, set_alct: 16;
    pub alcf, set_alcf: 17;
    pub abpt, set_abpt: 18;
    pub abpf, set_abpf: 19;
    pub abnt, set_abnt: 21;
    pub abnf, set_abnf: 22;
    pub abgd, set_abgd: 23;
    pub abgi, set_abgi: 24;
    // TODO: SS[25]
    pub vfdi, set_vfdi: 26;
    pub srp, set_srp: 27;
    pub bap, set_bap: 28;
    pub eap, set_eap: 29;
    pub u8, ipd, set_ipd: 31, 30;
}

impl Ss {
    pub fn aas_dst_mask(&self) -> u8 {
        (self.aas_mask() & 5) | (self.aas_mask() >> 1 & 5)
    }
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Als(u32);
    pub u8, cmp_dst, set_cmp_dst: 4, 0;
    pub u8, cmp_op, set_cmp_op: 7, 5;
    pub u8, src4, set_src4: 7, 0;
    pub u8, dst, set_dst: 7, 0;
    pub u8, src2, set_src2: 15, 8;
    pub u8, src1, set_src1: 23, 16;
    pub u8, op, set_op: 30, 24;
    pub sm, set_sm: 31;
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
    pub u8, mas, set_mas: 27, 21;

    pub tr, set_tr: 25;
    pub bn, set_bn: 26;
    pub bp, set_bp: 27;
    pub vfrpsz, set_vfrpsz: 28;
}

impl Cs1 {
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
    pub u32, ty, set_ty: 31, 17;

}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Ales(u16);
    pub u8, src3, set_src3: 7, 0;
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
    pub index, set_index: 5, 1;
    pub area, set_area: 11, 6;
    pub op, set_op: 14, 12;
    pub be, set_be: 15;
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
    pub u8, lpsrc0, set_lpsrc0: 31, 10;
    pub u8, op, set_op: 15, 14;
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

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
    pub struct Cds(u32);
    pub u16, from into Rlp, rlp1, set_rlp1: 15, 0;
    pub u16, from into Rlp, rlp0, set_rlp0: 31, 16;
}

bitfield! {
    #[derive(Copy, Clone, Default, Eq, PartialEq)]
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

impl From<u16> for Rlp {
    fn from(value: u16) -> Rlp {
        Rlp(value)
    }
}

impl Into<u16> for Rlp {
    fn into(self) -> u16 {
        self.0
    }
}
