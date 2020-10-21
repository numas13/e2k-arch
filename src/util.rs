pub const fn u16_from_le_slice(s: &[u8]) -> u16 {
    u16::from_le_bytes([s[0], s[1]])
}

pub const fn u32_from_le_slice(s: &[u8]) -> u32 {
    u32::from_le_bytes([s[0], s[1], s[2], s[3]])
}
