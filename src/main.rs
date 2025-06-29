use std::arch::asm;
use std::fs::{read, read_link, File};
use std::io::{BufReader, Read};
use std::ops::Deref;
use csv::Reader;

// ### GOAL ###
// Determine (A) date gaps, (B) per-date hour gaps.


#[derive(serde::Deserialize)]
struct Row {
    time: String,
    hr: f32
}

#[derive(Default)]
#[derive(Debug)]
struct UTime {
    year: u16,
    month: u8,
    day: u8,
    dh_since: u16 // minutes since Dark Hour
}

impl UTime {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from(year_since: u16, month: u8, day: u8, dh_since: u16) -> Self {
        UTime { year: 1970 + year_since, month, day, dh_since }
    }

    pub fn from_hr(hr_time: &str) -> Self{
        // Format: (M)M/(D)D/YY (H)H:MM

        let hr_mod = {
            let mut hr_clone = hr_time.clone().to_string();
            let cl_bytes = unsafe { hr_clone.as_bytes_mut() }; // <-- technically UTF-8 (1-4 var-width encoding) but @assume ASCII so 1 byte
            for c in 0..cl_bytes.len() {
                // Must be within 0x30-0x39... 0b110000-0b111001... must not be 0b111010-0b(1)000000-...
                // Supposedly may be faster if short-circuit high nibble then check lower nibble.
                // if (*b & 0xF0 == 0x30) && (*b & 0x0F < 0x0A) { <-- the more generic [^0-9]

                let mut b = cl_bytes[c];

                // Only delimiters are 0x20 ( ), 0x2F (/), 0x3A (:). All contain bit 5 (0x20), though we
                // can also ignore 0x20 since we'll keep it as the unified delimiter. This also conveniently
                // enables the bit 5 check.
                if (b & 0x20 != 0) && ((b == 0x2F) || (b == 0x3A)) {
                    cl_bytes[c] = 0x20;
                }
            }

            hr_clone
        };

        let mod_bytes = hr_mod.split(' ').map(|s| s.parse::<u8>().unwrap()).collect::<Vec<_>>(); // TODO: asm should fix all the redundant mem use
        UTime::from(2000 + mod_bytes[2] as u16, mod_bytes[0], mod_bytes[1], mod_bytes[3] as u16 * 60 + mod_bytes[4] as u16)

        // CHALLENGE: ARM64 assembly to repnz scasb thru the string.
        //            This would be more efficient than the current "string-level" crude implem!
        // unsafe {
        //     asm!(
        //     ...
        //     )
        // }
    }

    pub fn from_unix(unix_stamp: u64) -> Self {
        
    }

    pub fn to_unix(&self) -> u64 {

    }

    pub fn diff(&self, other: UTime) -> UTime {

    }
}

fn main() {
    let data = File::open("data/HR_001.csv").unwrap();
    let mut csr = Reader::from_reader(data);
    let mut rows: Vec<Row> = Vec::new();
    for res in csr.records() {
        let rec = res.unwrap();
        let row: Row = rec.deserialize(None).unwrap();
        rows.push(row);
    }

    let (splits, counts) = validate_chunking(&rows);
    let times: Vec<UTime> = splits.iter().map(|s| UTime::from_hr(&rows.get((s-1) as usize).unwrap().time)).collect();

    id_gaps(&times);
}

/// Validate whether time column is chunked in 60s.
/// Standard thinking would be to generify this but as Abrash/grug alludes to test first, overengineer later.
fn validate_chunking(csr: &Vec<Row>) -> (Vec<u64>, Vec<u8>) {
    let mut iter = csr.iter();
    let mut counts = (Vec::<u64>::new(), Vec::<u8>::new());
    let mut index: (u64, u8) = (1, 0); // date index, count index
    let mut curr: &String = &iter.next().unwrap().time;  // Prefer over loading/checking every previous

    while let Some(row) = iter.next() {
        let time = &row.time;

        if time.eq(curr) {
            index.1 += 1;
        } else {
            index.0 += index.1 as u64;
            counts.0.push(index.0);
            counts.1.push(index.1);

            if index.1 != 60 {
                println!("{:?} degenerate ({:?}/60 ▶▶ {:?})", curr, index.1, index.0);
            }

            index.1 = 1;
            curr = time;
        }
    }

    counts
}

fn id_gaps(times: &Vec<UTime>) {
    times.windows(2)
        .inspect(|)
}

pub(crate) fn mm2dd(mm: u8) -> u8 { // https://cmcenroe.me/2014/12/05/days-in-month-formula.html
    28 + (mm + (mm / 8)) % 2 + (2 % mm) + 2 * (mm / 8)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn s_mm22dd() { // [s] standard, non-obj test
        assert_eq!(mm2dd(1), 31);
        assert_eq!(mm2dd(2), 28);
        assert_eq!(mm2dd(3), 31);
        assert_eq!(mm2dd(4), 30);
        assert_eq!(mm2dd(5), 31);
        assert_eq!(mm2dd(6), 30);
        assert_eq!(mm2dd(7), 31);
        assert_eq!(mm2dd(8), 31);
        assert_eq!(mm2dd(9), 30);
        assert_eq!(mm2dd(10), 31);
        assert_eq!(mm2dd(11), 30);
        assert_eq!(mm2dd(12), 31);
    }

    #[test]
    fn r_utime_unix() { // [r] obj test (rationale: alphabetically close)
        let ut = UTime::from(34, 12, 8, 682);
        assert_eq!(ut.to_unix(), 1102522920);
    }
}

