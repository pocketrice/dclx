mod bitops;

use std::ascii::AsciiExt;
use csv::Reader;
use intx::U24;
use plotters::prelude::*;
use std::fmt::Display;
use std::fs::File;
use std::io::{stdin, stdout, Read, Write};
use std::ops::Deref;

// ** GOAL **
// Date gaps, per-date hour gaps, mem2visplot

// IDEA - CLI tool "pin" (pin a location or file and unpin) to release alias
// IDEA - cctv-1

const ASCII_RESIDUAL: u32 = 0x1CFD2; // <-- most optimal would be intx::from_be_bytes() but not a const function

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
    sec: U24 // <-- originally "pseudo-unioned" sec and min (emulating behavior for corresponding use-case under a single U24) but easier to just assume second
}

// union VTime { <-- this may make sense but too much work/redundant
//     utime: UTime,
//     unix: U48
// }


impl UTime {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from_min(year_since: u16, month: u8, day: u8, min: u16) -> Self {
        UTime { year: 1970 + year_since, month, day, sec: U24::try_from(min as u32 * 60u32).unwrap() }
    }

    pub fn from_sec(year_since: u16, month: u8, day: u8, sec: U24) -> Self {
        UTime { year: 1970 + year_since, month, day, sec }
    }

    pub fn from_hr(hr_time: &str) -> Self {
        // HR_001: (M)M/(D)D/YY (H)H:MM
        // HR_00x: YYYY-MM-DD HH:MM:SS

        // let t_bytes = hr_time.as_bytes(); // technically can cram into an "if let & ==" signature but requires unstable let chains

        if hr_time.as_bytes()[4] == 0x2D { // f1 never contains dashes, this is O(1) check
            UTime::from_sec(
                hr_time[0..=3].parse::<u16>().unwrap(),
                hr_time[5..=6].parse::<u8>().unwrap(),
                hr_time[8..=9].parse::<u8>().unwrap(),
                // HH:MM:SS -> ( HH)(:MM)(:SS) -> [(2) * 60^2 + (1) * 60^1 + (0) * 60^0] - (0x20 * 60^2 + 0x3A * 60^1 + 0x3A * 60^0)
                {
                    let mut acc = 0u32; // Guaranteed to not overflow; U24 also doesn't impl Add
                    for (i, chunk) in hr_time[10..].as_bytes().chunks(3).rev().enumerate() {
                        // wonder if there is a way to replace multiplication with some clever shifting ...?
                        acc += ((chunk.get(1).unwrap() & 0x0F) * 10 + (chunk.get(2).unwrap() & 0x0F)) as u32 * 60u32.pow(i as u32); // <-- it may seem more obvious to just index [1..] to remove residual, but precalculating const might be faster...
                    }

                    U24::try_from(acc).unwrap()
                }
            )
        } else {
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
            UTime::from_min(2000 + mod_bytes[2] as u16, mod_bytes[0], mod_bytes[1], mod_bytes[3] as u16 * 60 + mod_bytes[4] as u16)
        }



        // CHALLENGE: ARM64 assembly to repnz scasb thru the string.
        //            This would be more efficient than the current "string-level" crude implem!
        // unsafe {
        //     asm!(
        //     ...
        //     )
        // }
    }

    pub fn from_unix_min(unix_min: u64) -> Self {
        Self::from_unix(unix_min * 60)
    }

    pub fn from_unix(unix_stamp: u64) -> Self {
        let year = 1970 + (unix_stamp / 31536000) as u16;

        let mut raw = unix_stamp % 31536000;
        let mut month = 1;
        while raw > 28 * 86400 {
            raw -= mm2dd(month) as u64 * 86400;
            month += 1;
        }

        let day = (1 + raw / 86400) as u8;
        let sec = U24::try_from(raw % 86400).unwrap();

        Self { year, month, day, sec }
    }

    pub fn to_unix(&self) -> u64 { // ...seconds
        (self.year - 1970) as u64 * 31536000 + ((1..self.month).fold(0u64, |acc, ss| acc + mm2dd(ss) as u64 * 60) + self.day as u64 - 1) * 86400 + u64::from(self.sec)
    }

    pub fn diff(&self, other: &UTime) -> UTime {
       // println!("{} + {} ///// {} - {}\n", self, other, self.to_unix(), other.to_unix());
        Self::from_unix(self.to_unix() - other.to_unix())
    }
}

impl Display for UTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let m_sec = u32::from(self.sec);
        let str = format!("{:?}/{:?}/{:?} {:02}:{:02}:{:02}", self.month, self.day, self.year - 1970, m_sec / 3600, (m_sec % 3600) / 60, m_sec % 60);
        write!(f, "{}", str) // TODO year seemingly becomes 3990?
    }
}

fn main() {
    let arg = std::env::args().skip(1).next();

    // ▼ looks like black magic, but take a close peek
    let file: String = if let Some(name) = arg { // tiny hint on if let — binding only valid for the true branch!
        format!("data/{}.csv", name)
    } else {
        let (mut s1, mut s2) = (String::new(), String::new());
        query(&mut s1, "Variable");
        query(&mut s2, "Dataset");

        format!("data/{}_{:03}.csv", s1.trim().to_ascii_uppercase(), s2.trim().parse::<u8>().unwrap())
    };
    
    println!();

    let data = File::open(file).unwrap();
    let mut csr = Reader::from_reader(data);
    let mut rows: Vec<Row> = Vec::new();
    for res in csr.records() {
        let rec = res.unwrap();
        let row: Row = rec.deserialize(None).unwrap();
        rows.push(row);
    }

    //let is_kgf = &rows.first().unwrap().time.as_bytes()[4] == &0x2D; // Known Good Format; type 2 format

    let (splits, _) = validate_chunking(&rows);
    let times = splits.iter().map(|s| UTime::from_hr(&rows.get((s-1) as usize).unwrap().time)).collect();

    println!();

    id_gaps(&times, &splits);
}


/// Indexed Run-Length Encode zipped data. This means retrieval still depends on
///  the original dataset.
//fn irle_data(data: &Vec<Row>) -> (Vec<u64>, Vec<u8>) {}

/// Validate whether time column is chunked in 60s. Returns (<splits>, <counts>).
/// Standard thinking would be to generify this but as Abrash/grug alludes to test first, overengineer later.
fn validate_chunking(csr: &Vec<Row>) -> (Vec<u64>, Vec<u8>) {
    let mut iter = csr.iter();
    let mut counts = (Vec::<u64>::new(), Vec::<u8>::new());
    let mut index: (u64, u8) = (1, 0); // date index, count index
    let mut curr: &String = &iter.next().unwrap().time;  // Prefer over loading/checking every previous

    while let Some(row) = iter.next() {
        let time = &row.time;
                                                                    // Type 1 has "second" terminator @ 10 or 13 ▼
        if time.chars().take(16).eq(curr.chars().take(16)) { // Type 2 has second terminator @ 16 ▼
            index.1 += 1;                                           // Just comparing first 16 characters guarantees only minutes retaining behavior for both Type 1 and 2 ✩
        } else {
            index.0 += index.1 as u64;
            counts.0.push(index.0);
            counts.1.push(index.1);

            if index.1 != 60 {
                println!("{:?} bad ({:?}/60 ▶▶ @{:?})", &curr[..curr.len() - 3], index.1, index.0);
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

        if w2.diff(w1).to_unix() > 60 {
            println!("\"{:} — {:}\" gap ▶▶ @{:?}", w1, w2, splits[spl_ind-1]);
        }
    }
}

// This is a terminal operation so "consume vector".
fn visplot(data: Vec<(usize, usize)>, caption: &str, label_x: &str, label_y: &str) {
    let axes_scl = unzip_max(&data);

    evcxr_figure((640, 480), |root| {
        let mut chart = ChartBuilder::on(&root)
            .caption(caption, ("FOT-Skip std", 20).into_font())
            .x_label_area_size(40)
            .y_label_area_size(40)
            .build_cartesian_2d(0usize..axes_scl.0, 0usize..axes_scl.1)?;

        chart.configure_mesh()
            .x_desc(label_x)
            .y_desc(label_y)
            .draw()?;

        Ok(())
    }).style("width: 60%");
}

// pub(crate) fn rle_decode<T>(rle_data: Vec<(T, usize)>) -> Vec<T> {
//     // i wonder if there is a way to reinterpret the same vec memory as <T> instead of <(T, usize)> upon conversion? asm?
// }

// should be easy to adapt this to unzip_min using Rev()
pub(crate) fn unzip_max<T: Ord + Clone>(data: &Vec<(T, T)>) -> (T, T) {
    let (dat_x, dat_y): (Vec<_>, Vec<_>) = data.iter().cloned().unzip();
    (dat_x.iter().max().unwrap().clone(), dat_y.iter().max().unwrap().clone())
}

pub(crate) fn query(bfr: &mut String, msg: &str) {
    print!("{}", format!("{} ... ", msg));
    let _ = stdout().flush();
    stdin().read_line(bfr).expect("Bad user string");
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

