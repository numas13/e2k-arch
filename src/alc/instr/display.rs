use super::Instr;
use core::fmt;

struct DisplayName {
    name: &'static str,
    channel: u8,
    sm: bool,
}

impl fmt::Display for DisplayName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{}", self.name, self.channel)?;
        if self.sm {
            f.write_str(",sm")?;
        }
        Ok(())
    }
}

pub struct Display<'a> {
    instr: &'a Instr,
    channel: u8,
    sm: bool,
}

impl<'a> Display<'a> {
    pub fn new(instr: &'a Instr, channel: u8, sm: bool) -> Self {
        Self { instr, channel, sm }
    }
    fn display_name(&self, name: &'static str) -> impl fmt::Display {
        DisplayName {
            name,
            channel: self.channel,
            sm: self.sm,
        }
    }
}

impl fmt::Display for Display<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.instr {
            Instr::Op2(op, src2, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src2 = src2.display(size.next());
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}", s, src2, dst)
            }
            Instr::Op3(op, src1, src2, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src1 = src1.display(size.next());
                let src2 = src2.display(size.next());
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}, {}", s, src1, src2, dst)
            }
            Instr::Op4(op, src1, src2, src3, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src1 = src1.display(size.next());
                let src2 = src2.display(size.next());
                let src3 = src3.display(size.next());
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}, {}, {}", s, src1, src2, src3, dst)
            }
            Instr::Op2cmp(op, src2, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src2 = src2.display(size.next());
                write!(f, "{} {}, {}", s, src2, dst)
            }
            Instr::Op3cmp(op, src1, src2, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src1 = src1.display(size.next());
                let src2 = src2.display(size.next());
                write!(f, "{} {}, {}, {}", s, src1, src2, dst)
            }
            Instr::Op3mrgc(op, src1, src2, dst, cond) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src1 = src1.display(size.next());
                let src2 = src2.display(size.next());
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}, {}, {}", s, src1, src2, dst, cond)
            }
            Instr::Op4mrgc(op, src1, src2, src3, dst, cond) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src1 = src1.display(size.next());
                let src2 = src2.display(size.next());
                let src3 = src3.display(size.next());
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}, {}, {}, {}", s, src1, src2, src3, dst, cond)
            }
            Instr::Op3imm8(op, src2, imm, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src2 = src2.display(size.next());
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}, {}", s, src2, imm, dst)
            }
            Instr::Op4imm8(op, src1, src2, imm, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src1 = src1.display(size.next());
                let src2 = src2.display(size.next());
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}, {}, {}", s, src1, src2, imm, dst)
            }
            Instr::Op3load(op, addr, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let addr = addr.display(size.next(), size.next());
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}", s, addr, dst)
            }
            Instr::Op3store(op, src4, addr) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src4 = src4.display(size.next());
                let addr = addr.display(size.next(), size.next());
                write!(f, "{} {}, {}", s, src4, addr)
            }
            Instr::Op2rw(op, src2, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src2 = src2.display(size.next());
                write!(f, "{} {}, {}", s, src2, dst)
            }
            Instr::Op2rr(op, src1, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}", s, src1, dst)
            }
            Instr::OpAload(op, addr, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let dst = dst.display(size.next());
                write!(f, "{} {}, {}", s, addr, dst)
            }
            Instr::OpAstore(op, src4, dst) => {
                let s = self.display_name(op.as_str());
                let mut size = op.operand_sizes();
                let src4 = src4.display(size.next());
                write!(f, "{} {}, {}", s, src4, dst)
            }
        }
    }
}
