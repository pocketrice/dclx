use csv::Reader;
use std::fmt::Display;
use std::fs::File;
use std::io::Read;
use std::ops::Deref;

// ### GOAL ###
// Determine (A) date gaps, (B) per-date hour gaps.


#[derive(serde::Deserialize)]
struct Row {
    time: String,
    hr: f32
}

#[derive(Default, Debug)]
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
        let year = 1970 + (unix_stamp / 525600) as u16;

        let mut raw = unix_stamp % 525600;
        let mut month = 1;
        while raw > 28 * 1440 {
            raw -= mm2dd(month) as u64 * 1440;
            month += 1;
        }

        let day = (1 + raw / 1440) as u8;
        let dh_since = (raw % 1440) as u16;

        Self { year, month, day, dh_since }
    }

    pub fn to_unix(&self) -> u64 { // "Unix minutes"
        (self.year - 1970) as u64 * 525600 + ((1..self.month).fold(0u64, |acc, mm| acc + mm2dd(mm) as u64) + self.day as u64 - 1) * 1440 + (self.dh_since as u64)
    }

    pub fn diff(&self, other: &UTime) -> UTime {
        Self::from_unix(self.to_unix() - other.to_unix())
    }
}

impl Display for UTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = format!("{:?}/{:?}/{:?} {:02}:{:02}", self.month, self.day, self.year - 1970, self.dh_since / 60, self.dh_since % 60);
        write!(f, "{}", str) // TODO year seemingly becomes 3990?
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

    let (splits, _) = validate_chunking(&rows);
    let times: Vec<UTime> = splits.iter().map(|s| UTime::from_hr(&rows.get((s-1) as usize).unwrap().time)).collect();

    println!();
    
    id_gaps(&times, &splits);
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
                println!("{:?} degenerate ({:?}/60 ▶▶ @{:?})", curr, index.1, index.0);
            }

            index.1 = 1;
            curr = time;
        }
    }

    counts
}

fn id_gaps(times: &Vec<UTime>, splits: &Vec<u64>) {
    let mut spl_ind = 0; // TODO maybe replace .windows() with std counting for loop
    for w in times.windows(2) {
        spl_ind += 1;
        let (w1, w2) = (w.first().unwrap(), w.last().unwrap());

        if w2.diff(w1).to_unix() > 1 {
            println!("\"{:} ⇢ {:}\" gap ✦ @{:?}", w1, w2, splits[spl_ind]);
        }
    }
}

pub(crate) fn mm2dd(mm: u8) -> u8 { // https://cmcenroe.me/2014/12/05/days-in-month-formula.html
    28 + (mm + (mm / 8)) % 2 + (2 % mm) + 2 * (1 / mm)
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
    fn utime_4unix() {
        let ut = UTime::from(34, 1, 1, 680);
        let ut2 = UTime::from_unix(ut.to_unix());
        assert_eq!(ut.diff(&ut2).to_unix(), 0);
    }

    // #[test]
    // fn utime_2unix_base() { // [struct_name] is-obj test
    //     let ut = UTime::from(0, 1, 1, 300);
    //     assert_eq!(ut.to_unix(), 18000);
    // }
    //
    // #[test]
    // fn utime_2unix_mmdd() {
    //     let ut: UTime = UTime::from(0, 12, 8, 300);
    //     assert_eq!(ut.to_unix(), 29480400);
    // }
    //
    // #[test]
    // fn utime_2unix_yymmdd() {
    //     let ut: UTime = UTime::from(34, 6,12,240);
    //     assert_eq!(ut.to_unix(), 1087012800);
    // }

}

