# About

e2k-arch is a library for decoding and encoding an instruction for E2K architecture written in Rust.

# Example

```rust
use e2k_arch::alc::instr;
use e2k_arch::alc::instr::operand::{LitValue, Reg};
use e2k_arch::alc::Channel;
use e2k_arch::cu::ct::{Ct, CtOp};
use e2k_arch::cu::Ipd;
use e2k_arch::state::Ctpr;
use e2k_arch::Bundle;

// {
//      ct %ctpr1
//      ipd 3
//      fmul_adds,2,sm %b[54], _f32s,_lts0 0xdeadbeef, %b[17], %g8
// }
fn fmul_adds_and_jump() -> Bundle {
    let jump = Ct::new(Ctpr::new_clamp(1), CtOp::Explicit);

    let fmul_adds = instr::fmul_adds(
        Reg::based_truncate(54),
        LitValue::l32_truncate(0, 0xdeadbeef),
        Reg::based_truncate(17),
        Reg::global_truncate(8),
    );

    let mut bundle = Bundle::default();
    bundle.alc.channels[2] = Some(Channel::new(true, fmul_adds));
    bundle.cu.ct = Some(jump);
    bundle.cu.ipd = Ipd::new_truncate(3);
    bundle
}

fn main() {}

#[test]
fn encode_bundle() {
    let expected = [
        0x23, 0x10, 0x40, 0x10, // HS
        0x20, 0x04, 0x00, 0xc0, // SS
        0xe8, 0xd8, 0x36, 0x88, // ALS2
        0x00, 0x00, 0x11, 0x0c, // ALES2+5
        0x00, 0x00, 0x00, 0x00, // align
        0xef, 0xbe, 0xad, 0xde, // LTS0
    ];
    let bundle = fmul_adds_and_jump();
    let version = u8::MAX;
    let raw = bundle.into_raw(version).unwrap();
    let mut buffer = [0u8; 64];
    let (packed, _) = raw.pack(&mut buffer).unwrap();
    assert_eq!(packed.as_slice(), expected);
}

```