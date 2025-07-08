pub fn bits8(byte: u8) -> [u8; 8] { // big-endian (8-0)
    [byte >> 7 & 0x1, byte >> 6 & 0x1, byte >> 5 & 0x1, byte >> 4 & 0x1, byte >> 3 & 0x1, byte >> 2 & 0x1, byte >> 1 & 0x1, byte & 0x1]
}

pub fn bits16(num: u16) -> [u8; 16] {
    [(num >> 15 & 0x1) as u8, (num >> 14 & 0x1) as u8, (num >> 13 & 0x1) as u8, (num >> 12 & 0x1) as u8, (num >> 11 & 0x1) as u8, (num >> 10 & 0x1) as u8, (num >> 9 & 0x1) as u8, (num >> 8 & 0x1) as u8, (num >> 7 & 0x1) as u8, (num >> 6 & 0x1) as u8, (num >> 5 & 0x1) as u8, (num >> 4 & 0x1) as u8, (num >> 3 & 0x1) as u8, (num >> 2 & 0x1) as u8, (num >> 1 & 0x1) as u8, (num & 0x1) as u8]
}

pub fn decomp16(num: u16) -> [u8; 2] {
    [((num & 0xF0) >> 8) as u8, (num & 0x0F) as u8]
}

pub fn decomp24(num: u32) -> [u8; 3] {
    [((num & 0xF00) >> 16) as u8, ((num & 0x0F0) >> 8) as u8, (num & 0x00F) as u8]
}

pub fn comp16(num: &[u8; 2]) -> u16 {
    ((num[0] as u16) << 8) | num[1] as u16
}

pub fn comp24(num: &[u8; 3]) -> u32 {
    ((num[0] as u32) << 16) |  ((num[1] as u32) << 8) | num[0] as u32
}