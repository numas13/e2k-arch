use super::operands::*;
use super::Instr;
use crate::alc::state::Reg;
use core::fmt::{self, Write};

pub enum Size {
    // undefined
    U,
    B,
    H,
    W,
    D,
    X,
    Q,
}

impl Size {
    fn new_dxq(val: u8) -> Size {
        match val & 0x3 {
            super::OP_SIZE_D => Self::D,
            super::OP_SIZE_X => Self::X,
            super::OP_SIZE_Q => Self::Q,
            _ => Self::U,
        }
    }
}

impl fmt::Display for Size {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::D => fmt.write_char('d'),
            Self::X => fmt.write_char('x'),
            Self::Q => fmt.write_char('q'),
            _ => Ok(()),
        }
    }
}

#[derive(Copy, Clone)]
pub struct OperandSizes(pub u8);

impl OperandSizes {
    pub fn op0(&self) -> Size {
        Size::new_dxq(self.0)
    }
    pub fn op1(&self) -> Size {
        Size::new_dxq(self.0 >> 2)
    }
    pub fn op2(&self) -> Size {
        Size::new_dxq(self.0 >> 4)
    }
    pub fn op3(&self) -> Size {
        Size::new_dxq(self.0 >> 6)
    }
}

fn print_reg(reg: &Reg, sz: Size, fmt: &mut fmt::Formatter) -> fmt::Result {
    match reg {
        Reg::Based(r) => write!(fmt, "%{}b[{}]", sz, r.get()),
        Reg::Regular(r) => write!(fmt, "%{}r{}", sz, r.get()),
        Reg::Global(r) => write!(fmt, "%{}g{}", sz, r.get()),
    }
}

fn print_src1(src1: &Src1, sz: Size, fmt: &mut fmt::Formatter) -> fmt::Result {
    match src1 {
        Src1::Reg(reg) => print_reg(reg, sz, fmt),
        Src1::Imm(i) => write!(fmt, "{:#x}", i.get()),
    }
}

fn print_src2(src2: &Src2, sz: Size, lts: &[u32; 4], fmt: &mut fmt::Formatter) -> fmt::Result {
    match src2 {
        Src2::Reg(reg) => print_reg(reg, sz, fmt),
        Src2::Imm(i) => write!(fmt, "{:#x}", i.get()),
        Src2::Lit(lit) => {
            write!(fmt, "{} ", lit)?;
            match lit {
                LitLoc::F16(loc, LitPart::Lo) => {
                    write!(fmt, "{:#x}", lts[loc.get() as usize] as u16)
                }
                LitLoc::F16(loc, LitPart::Hi) => {
                    let lit = lts[loc.get() as usize];
                    write!(fmt, "{:#x}", lit >> 16)
                }
                LitLoc::F32(loc) => write!(fmt, "{:#x}", lts[loc.get() as usize]),
                LitLoc::F64(loc) => {
                    let i = loc.get() as usize;
                    let lo = lts[i];
                    let hi = lts[i + 1];
                    let lit = (hi as u64) << 32 | lo as u64;
                    write!(fmt, "{:#x}", lit)
                }
            }
        }
    }
}

fn print_src3(src3: &Src3, sz: Size, fmt: &mut fmt::Formatter) -> fmt::Result {
    print_reg(&src3.0, sz, fmt)
}

fn print_src4(src4: &Src4, sz: Size, fmt: &mut fmt::Formatter) -> fmt::Result {
    print_reg(&src4.0, sz, fmt)
}

fn print_dst(dst: &Dst, sz: Size, fmt: &mut fmt::Formatter) -> fmt::Result {
    match dst {
        Dst::Reg(reg) => print_reg(reg, sz, fmt),
        Dst::Tst => fmt.write_str("%tst"),
        Dst::Tc => fmt.write_str("%tc"),
        Dst::Tcd => fmt.write_str("%tcd"),
        Dst::Ctpr(ctpr) => fmt::Display::fmt(ctpr, fmt),
        Dst::Empty => fmt.write_str("%empty"),
    }
}

fn print_addr(
    addr: &Addr,
    sz0: Size,
    sz1: Size,
    lts: &[u32; 4],
    fmt: &mut fmt::Formatter,
) -> fmt::Result {
    fmt.write_str("[ ")?;
    print_src1(&addr.0, sz0, fmt)?;
    fmt.write_str(" + ")?;
    print_src2(&addr.1, sz1, lts, fmt)?;
    fmt.write_str(" ]")
}

fn print_instr_name(name: &str, channel: usize, sm: bool, fmt: &mut fmt::Formatter) -> fmt::Result {
    write!(fmt, "{},{}", name, channel)?;
    if sm {
        fmt.write_str(",sm")?;
    }
    fmt.write_char(' ')
}

pub fn print_instr(
    instr: &Instr,
    fmt: &mut fmt::Formatter,
    i: usize,
    sm: bool,
    lts: &[u32; 4],
) -> fmt::Result {
    match instr {
        Instr::Op2(o, src2, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src2(src2, sz.op0(), lts, fmt)?;
            fmt.write_str(", ")?;
            print_dst(dst, sz.op1(), fmt)
        }
        Instr::Op3(o, src1, src2, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src1(src1, sz.op0(), fmt)?;
            fmt.write_str(", ")?;
            print_src2(src2, sz.op1(), lts, fmt)?;
            fmt.write_str(", ")?;
            print_dst(dst, sz.op2(), fmt)
        }
        Instr::Op4(o, src1, src2, src3, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src1(src1, sz.op0(), fmt)?;
            fmt.write_str(", ")?;
            print_src2(src2, sz.op1(), lts, fmt)?;
            fmt.write_str(", ")?;
            print_src3(src3, sz.op2(), fmt)?;
            fmt.write_str(", ")?;
            print_dst(dst, sz.op2(), fmt)
        }
        Instr::Op2cmp(o, src2, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src2(src2, sz.op0(), lts, fmt)?;
            write!(fmt, ", {}", dst.0)
        }
        Instr::Op3cmp(o, src1, src2, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src1(src1, sz.op0(), fmt)?;
            fmt.write_str(", ")?;
            print_src2(src2, sz.op1(), lts, fmt)?;
            write!(fmt, ", {}", dst.0)
        }
        Instr::Op3mrgc(o, src1, src2, dst, cond) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src1(src1, sz.op0(), fmt)?;
            fmt.write_str(", ")?;
            print_src2(src2, sz.op1(), lts, fmt)?;
            fmt.write_str(", ")?;
            print_dst(dst, sz.op2(), fmt)?;
            write!(fmt, ", {}", cond)
        }
        Instr::Op4mrgc(o, src1, src2, src3, dst, cond) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src1(src1, sz.op0(), fmt)?;
            fmt.write_str(", ")?;
            print_src2(src2, sz.op1(), lts, fmt)?;
            fmt.write_str(", ")?;
            print_src3(src3, sz.op2(), fmt)?;
            fmt.write_str(", ")?;
            print_dst(dst, sz.op3(), fmt)?;
            write!(fmt, ", {}", cond)
        }
        Instr::Op3imm8(o, src2, imm, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src2(src2, sz.op0(), lts, fmt)?;
            fmt.write_str(", ")?;
            write!(fmt, "{}, ", imm.0)?;
            print_dst(dst, sz.op1(), fmt)
        }
        Instr::Op4imm8(o, src1, src2, imm, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src1(src1, sz.op0(), fmt)?;
            fmt.write_str(", ")?;
            print_src2(src2, sz.op1(), lts, fmt)?;
            fmt.write_str(", ")?;
            write!(fmt, "{}, ", imm.0)?;
            print_dst(dst, sz.op2(), fmt)
        }
        Instr::Op3load(o, addr, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_addr(addr, sz.op0(), sz.op1(), lts, fmt)?;
            fmt.write_str(", ")?;
            print_dst(dst, sz.op2(), fmt)
        }
        Instr::Op3store(o, src4, addr) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src4(src4, sz.op0(), fmt)?;
            fmt.write_str(", ")?;
            print_addr(addr, sz.op1(), sz.op2(), lts, fmt)
        }
        Instr::Op2rw(o, src2, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src2(src2, sz.op0(), lts, fmt)?;
            write!(fmt, ", {}", dst.0)
        }
        Instr::Op2rr(o, src1, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            write!(fmt, "{}, ", src1.0)?;
            print_dst(dst, sz.op0(), fmt)
        }
        Instr::OpAlaod(o, addr, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            write!(fmt, "{}, ", addr)?;
            print_dst(dst, sz.op0(), fmt)
        }
        Instr::OpAstore(o, src4, dst) => {
            print_instr_name(o.as_str(), i, sm, fmt)?;
            let sz = o.operand_sizes();
            print_src4(src4, sz.op0(), fmt)?;
            write!(fmt, ", {}", dst)
        }
    }
}
