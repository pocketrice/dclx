extern crate core;

mod bitops;
mod irgex;

use csv::{Reader, Writer};
use intx::U24;
use std::cmp::PartialEq;
use std::fmt::Display;
use std::fs;
use std::fs::File;
use std::io::{stdin, stdout, Write};
use std::ops::{Add, Range};
use std::process::Command;

// ** GOAL **
// Date gaps, per-date hour gaps, mem2visplot

// IDEA - CLI tool "pin" (pin a location or file and unpin) to release alias

// #[macro_export]
// macro_rules! query {
//     () => {
//
//     };
//     ( $($msg:tt)* ) => {
//
//     }
// }


const _ASCII_RESIDUAL: u32 = 0x1CFD2; // <-- most optimal would be intx::from_be_bytes() but not a const function
const PROG_MODE: DclxMode = DclxMode::RewriteAppendTsepV2;
const _IS_LINE_ANNOTATED: bool = false;
const NA: &'static str = "NA";
const PY_PATH: &'static str = "~/Documents/IntelliJ/RustRover/dclx/py/"; //"../py/";

enum GlycemicVariable {
    Generic,
    Dexcom,
    FoodLog,
    HeartRate
}

impl GlycemicVariable {
    fn _value(&self) -> &'static str {
        match *self {
            GlycemicVariable::Generic => "",
            GlycemicVariable::Dexcom => "1",
            GlycemicVariable::FoodLog => "2",
            GlycemicVariable::HeartRate => "HR"
        }
    }
}

#[derive(Debug)]
enum DclxMode {
    FixT1, //               ## Patches T1 into a T2 file.
    RewriteTsepV1, //       ## Separates timestamps into d & hh:mm:ss.
    RewriteAppendTsepV1, // ## Applies RewriteTsepV1 + fills NA values.
    Analyze, //             ## Output .log file for missing values etc for a particular dataset.
    RewriteAppendTsepV2, // ## Separates timestamps into d,h,m,s,hh:mm:ss + fills NA values.
    MergeN, //              ## Condenses dataset by averaging HR values into N min chunks.
}

impl DclxMode {
    // HR_000-{suffix}.csv
    fn suffix(&self) -> &'static str {
        match *self {
            DclxMode::FixT1 => "fixed",
            DclxMode::RewriteTsepV1 => "rt1",
            DclxMode::RewriteAppendTsepV1 => "rat1",
            DclxMode::Analyze => "log",
            DclxMode::RewriteAppendTsepV2 => "rt2",
            DclxMode::MergeN => "minN",
        }
    }

    fn header(&self) -> &'static [&str] {
        match *self {
            DclxMode::FixT1 => &["datetime", "hr"],
            DclxMode::RewriteTsepV1 | DclxMode::RewriteAppendTsepV1 => &["day", "ts", "datetime", "hr"],
            DclxMode::Analyze => &["time_start", "time_end", "ind_f_start", "ind_f_end", "hr_start", "hr_end", "gap_time", "gap_len", "ind_s"],
            DclxMode::RewriteAppendTsepV2 => &["day", "hour", "min", "sec", "ts", "time", "hr"],
            DclxMode::MergeN => &["day", "hour", "min", "hr"]
        }
    }
}

#[derive(serde::Deserialize, serde::Serialize, Clone)]
struct Row {
    time: String,
    hr: f32
}

impl Row {
    fn new(time: String, hr: f32) -> Row {
        Row { time, hr }
    }
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
    /// Default constructor.
    pub fn new() -> Self {
        Default::default()
    }

    /// Construct from Type 1 data.
    pub fn from_min(year_since: u16, month: u8, day: u8, min: u16) -> Self {
        UTime { year: 1970 + year_since, month, day, sec: U24::try_from(min as u32 * 60u32).unwrap() }
    }

    /// Construct from Type 2 data.,
    pub fn from_sec(year_since: u16, month: u8, day: u8, sec: U24) -> Self {
        UTime { year: 1970 + year_since, month, day, sec }
    }

    /// Construct from Type 1 or 2 raw CSV string.
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
                let mut hr_clone = hr_time.to_string();
                let cl_bytes = unsafe { hr_clone.as_bytes_mut() }; // <-- technically UTF-8 (1-4 var-width encoding) but @assume ASCII so 1 byte
                for c in 0..cl_bytes.len() {
                    // Must be within 0x30-0x39... 0b110000-0b111001... must not be 0b111010-0b(1)000000-...
                    // Supposedly may be faster if short-circuit high nibble then check lower nibble.
                    // if (*b & 0xF0 == 0x30) && (*b & 0x0F < 0x0A) { <-- the more generic [^0-9]

                    let b = cl_bytes[c];

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

    /// Construct from Unix timestamp (in minutes — this truncates seconds!)
    pub fn from_unix_min(unix_min: u64) -> Self {
        Self::from_unix(unix_min * 60)
    }

    /// Construct from Unix timestamp (in seconds).
    pub fn from_unix(unix_stamp: u64) -> Self {
        let year = 1970 + (unix_stamp / 31557600) as u16;

        let mut raw = unix_stamp % 31557600;
        let mut month = 1;
        while raw > 28 * 86400 {
            if raw < mm2dd(month) as u64 * 86400 { // $$$
                println!("PANIC SOON! us={}, raw={}, mm={}", unix_stamp, raw, month);
            }

            raw -= mm2dd(month) as u64 * 86400;
            month += 1;
        }
        println!("{}", month); // $$$

        let day = (1 + raw / 86400) as u8;
        let sec = U24::try_from(raw % 86400).unwrap();

        Self { year, month, day, sec }
    }

    /// Convert to Unix timestamp (in seconds).
    pub fn to_unix(&self) -> u64 { // ...seconds
        (self.year - 1970) as u64 * 31557600 // 365d = 31536000s, 366d = 31622400s, 365.25d = 31557600s
            + ((1..self.month).fold(0u64, |acc, ss| acc + mm2dd(ss) as u64) // 1d = 86400s
            + self.day as u64 - 1) * 86400
            + u64::from(self.sec)
    }

    /// Convert to human-readable time string (e.g. 4yr12mo8d 8h3m52s)
    pub fn to_hrt(&self) -> String {
        let (yy, mm, dd, h, m, s) = self.segment();
        let decons: [(u8, &str); 6] = [ ((yy - 1970) as u8, "yr"), (mm-1, "mo"), (dd-1, "d"), (h, "h"), (m, "m"), (s, "s") ]; // <-- sadly (dyn Zero) or (dyn [custom UnInt=Display+Eq trait]) doesn't work since they're not dyn-compatible :c

        let mut hrt = String::new();
        for (i, (n, label)) in decons.iter().enumerate() {
            match i { // ...clarity over concision?
                0 => {
                    if n > &0u8 { hrt.push_str(format!("{}{}", *n as u16 + 1970, label).as_str()) }
                }
                3 => {
                    if !hrt.is_empty() { hrt.push(' '); }
                    if n > &0u8 { hrt.push_str(format!("{}{}", n, label).as_str()) }
                }
                _ => {
                    if n > &0u8 { hrt.push_str(format!("{}{}", n, label).as_str()) }
                }
            }
        }

        hrt
    }

    /// Convert to parsable time string — for now excluding yy:mm:dd (e.g. 08:03:52)
    pub fn to_prt(&self) -> String { // <-- wanted to call it parstr :< aaaaaaa curse u legibility!!!
        let (_, _, _, h, m, s) = self.segment();
        format!("{:02}:{:02}:{:02}", h, m, s)
    }

    /// Calculate the diff between times (positive-only) stored as UTime. Errors if produces a negative.
    pub fn diff(&self, other: &UTime) -> Result<UTime, ()> {
       // println!("{} + {} ///// {} - {}\n", self, other, self.to_unix(), other.to_unix());
        let (u1, u2) = (self.to_unix(), other.to_unix());
        if let Some(result) = u1.checked_sub(u2) {
            Ok(Self::from_unix(result))
        } else {
            Err(())
        }
    }

    /// Step forwards/backwards the designated amount of seconds. Returns self for chaining.
    pub fn step(&mut self, sec: i64) -> &mut Self {
        let abs_sec: u64 = sec.abs() as u64;

        *self = Self::from_unix(
            if sec.is_negative() {
                self.to_unix() - abs_sec
            } else {
                self.to_unix() + abs_sec
            }
        );

        self
    }

    /// Segment sec into YY:MM:DD:hh:mm:ss (akin to py's datetime::timetuple)
    pub fn segment(&self) -> (u16, u8, u8, u8, u8, u8) {
        let m_sec = u32::from(self.sec);
        (self.year, self.month, self.day, (m_sec / 3600) as u8, ((m_sec % 3600) / 60) as u8, (m_sec % 60) as u8)
    }

    /// Strip second segment from sec. Returns self for chaining.
    ///
    /// Handy for Type 2 → Type 1 data conversion. Produces new object in lieu of modification as Type 1 conversions are typically temporary.
    pub fn strip(&mut self) -> &mut Self {
       self.step(-i64::from(self.segment().2))
    }
}

impl Display for UTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (_, _, _, h, m, s) = self.segment();

        let str = format!("{:?}-{:02}-{:02} {:02}:{:02}:{:02}", self.year - 1970, self.month, self.day, h, m, s);
        write!(f, "{}", str) // TODO year seemingly becomes 3990?
    }
}

impl Clone for UTime {
    fn clone(&self) -> Self {
        Self { year: self.year, month: self.month, day: self.day, sec: self.sec }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let arg = std::env::args().skip(1).next();

    println!("\n\n**********************************\n CURRENT MODE IS {:?} \n**********************************\n", PROG_MODE);

    // ▼ looks like black magic, but take a close peek
    let (filename, var, dataset): (String, String, String) = if let Some(name) = arg { // tiny hint on if let — binding only valid for the true branch!
       // contingent on matching [A-Za-z]+_[A-Za-z]{3}\.csv TODO throw a fit if not compliant
        let segs = name.split_once('_').expect("Improper format");
        (name.clone(), segs.0.to_string(), segs.1[..3].to_string())
    } else {
        let (q1, q2) = (query("Variable"), query("Dataset"));

        let s1 = q1.to_ascii_uppercase();
        let s2 = format!("{:03}", q2.parse::<u8>()?);

        (format!("data/{}/{}_{}.csv", s2, s1, s2), s1, s2)
    };

    println!();

    let data = File::options()
        .read(true)
        .write(true)
        .open(&filename)?;

    let mut csr = Reader::from_reader(&data);
    let mut csw: Writer<File> = {
        let w_path = format!("{}.tmp", filename);
        if fs::exists(w_path).is_ok() {
            println!("!!! warning: rewriting existing .tmp\n");
        }

        Writer::from_path(format!("{}.tmp", filename))?
    };

    let mut rows: Vec<Row> = Vec::new();

    for res in csr.records() {
        let rec = res.expect("Bad record");
        let row: Row = rec.deserialize(None).unwrap();
        rows.push(row);
    }

    let (splits, _) = validate_chunking(&rows); // <-- csv indices; RLE representation of times
    let times = splits.iter().map(|s| UTime::from_hr(&rows.get(s-1).unwrap().time)).collect(); // <-- UTime; timestamp counterpart for splits
    let mut gaps = id_gaps(&times, &splits); // <-- csv indices; low-adjacent to offending line, in other words [i, i+1] encapsulates the evil gap

    println!();

    match PROG_MODE {
        DclxMode::FixT1 => {
           // csw.write_record(vec!["time", "hr"])?; <-- seems to be already written?!

            let mut curr: (String, u8) = (String::new(), 0);

            for row in rows {
                let rt = row.time.clone();

                if curr.0.eq(&*rt) {
                    curr.1 += 1;
                } else {
                    curr = (rt, 0u8);
                }

                let mut ut = UTime::from_hr(&row.time); // <-- note! do not append seconds yet, that doesn't fit Type 1 format.
                ut.step(curr.1 as i64);



                csw.serialize(Row { time: ut.to_string(), hr: row.hr }).expect("Failed to serialize row");
            }
        }

        DclxMode::Analyze => {
            // 08/12/25 change: HR_001.csv -> HR_001-log.csv
            // format:                                                  indices in "filled" (rewrite/appended) csv (b/c normal would just be i and i+1)
            //                                                       -------------------
            //                                                       |                 |                                                   prefer parsable time over human-readable
            //                                                       v                 v                                                    v
            // ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
            // |       time_start      |        time_end        |   ind_f_start   |    ind_f_end    |    hr_start    |    hr_end    |     gap_time    |    gap_len    |    ind_s    |  <-- index in "standard" (lower bound)
            // ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
            // |  2020-12-08 15:29:03  |  2020-12-08 19:49:00   |      192240     |      223923     |       92       |      104     |     08:48:03    |     31683     |    172303   |
            // /                       /                        /                 /                 /                /              /                 /               /             /


           // print!("{}", gaps.iter().take(5).map(|g| g.to_string()).join(";")); <-- for use with script.py

            let mut wtr_log: Writer<File> = Writer::from_path(format!("data/{}/{}_{}-log.csv", dataset, var, dataset))?;
            wtr_log.write_record(vec!["time_start", "time_end", "ind_f_start", "ind_f_end", "hr_start", "hr_end", "gap_time", "gap_len", "ind_s"])?;

            let mut fill = 0usize; // <-- fill padding to emulate filled indices using standard
            for gap in gaps {
                let (row_start, row_end) = (rows[gap-1].clone(), rows[gap].clone());
                let mut diff = {
                    let (ut_start, ut_end) = (UTime::from_hr(&row_start.time), UTime::from_hr(&row_end.time));
                    ut_end.diff(&ut_start)
                }.unwrap();


                diff.step(-1); // <-- plain diff is off-by-1 for physical counting
                let ind_offset = gap + fill; // <-- note: user-facing indices all assume 0-start indexing. csvlens reports using 1-start but 'ts fine.

                let record: Vec<String> = vec![row_start.time, row_end.time, ind_offset.to_string(), (ind_offset + diff.to_unix() as usize).to_string(), row_start.hr.to_string(), row_end.hr.to_string(), diff.to_prt(), diff.to_unix().to_string(), gap.to_string()];
                wtr_log.write_record(&record)?;

                fill = fill + diff.to_unix() as usize; // update fill with size of gap = num entries added = emulated index in filled csv. -1 due to fence shenanigans
            }
        }

        DclxMode::RewriteTsepV1 | DclxMode::RewriteAppendTsepV1 | DclxMode::RewriteAppendTsepV2 => { // For sake of ease and scope assuming all accepted dates are within the same year. Most (all?)  of the data is set this way anyway and i'm not writing a datetime lib..
            csw.write_record(PROG_MODE.header())?;

            let ut_init = UTime::from_hr(&rows[0].time);
            let day_marker = ut_init.day + cm2dd(ut_init.month) - 1; // start @ day 1

            let is_mode_append = matches!(PROG_MODE, DclxMode::RewriteAppendTsepV1) || matches!(PROG_MODE, DclxMode::RewriteAppendTsepV2); // TODO: either enum bitmask OR matches! for n > 1?

            for (i, row) in rows.iter().enumerate() {
                let ut_curr = UTime::from_hr(&row.time);
                let (_, mm, dd, h, m, s) = ut_curr.segment();
                let day_delta = ((dd + cm2dd(mm)) - day_marker).to_string();

                if is_mode_append && i == *gaps.first().unwrap_or(&0) {
                    let gap = gaps.remove(0);
                    println!("mending {}", gap);

                    let ut_gap = (UTime::from_hr(&rows[gap-1].time), UTime::from_hr(&rows[gap].time));
                    let diff = ut_gap.1.diff(&ut_gap.0).unwrap().to_unix();

                    for j in 0..diff-1 { // <-- insert reverse order @ gap pos so no index recalc!
                        let mut gap_clone = ut_gap.0.clone();
                        let nu_time = gap_clone.step(j as i64);

                        let (_, nmm, ndd, nh, nm, ns) = nu_time.segment();
                        let nu_delta = (ndd + cm2dd(nmm)) - day_marker;

                        match PROG_MODE {
                            DclxMode::RewriteAppendTsepV1 => {
                                csw.write_record(vec![&nu_delta.to_string(), &format!("{:02}:{:02}:{:02}", nh, nm, ns), &nu_time.to_string(), NA])?;
                            }
                            DclxMode::RewriteAppendTsepV2 => {
                                csw.write_record(vec![&nu_delta.to_string(), &nh.to_string(), &nm.to_string(), &ns.to_string(), &format!("{:02}:{:02}:{:02}", nh, nm, ns), &nu_time.to_string(), NA])?;
                            }
                            _ => unreachable!(),
                        }
                    }
                }


                let mut record = vec![day_delta, format!("{:02}:{:02}:{:02}", &h, &m, &s), row.time.clone(), row.hr.clone().to_string()];

                // insert add'l V2 values
                if matches!(PROG_MODE, DclxMode::RewriteAppendTsepV2) {
                    record.insert(1, s.to_string());
                    record.insert(1, m.to_string());
                    record.insert(1, h.to_string());
                }

                csw.write_record(record)?;
            }
        }

        DclxMode::MergeN => { // NOTE: run this on a non-filled dataset
            let n = query("# of minutes to merge?")
                .parse::<u64>().expect("Bad merge n");

            csw.write_record(DclxMode::MergeN.header())?;

            let ut_init = UTime::from_hr(&rows[0].time);
            let mut ut_counter = ut_init.clone();
            let day_marker = ut_init.day + cm2dd(ut_init.month) - 1;

            // rather than .iter() instead manually go thru all times (indirection requires manual check)
            let mut row_iter = rows.iter().peekable();
            while row_iter.len() > 0 {
                let stopper = ut_counter.to_unix() + (60 * n); // stop accumulation @ X minutes from initial counter
                let (mut hr_tot, mut hr_len) = (0f32, 0usize);

                // Add all rows within
                while let Some(&row) = row_iter.peek() {
                    if UTime::from_hr(&row.time).to_unix() < stopper {
                        let _ = row_iter.next();
                        hr_tot += row.hr;
                        hr_len += 1;
                    } else {
                        break
                    }
                }

                // fetch values to write
                let hr_avg = if hr_len > 0 {
                    &format!("{:.02}", hr_tot / hr_len as f32) // 2 sigfigs.
                } else {
                    NA
                };
                let (_, mm, dd, h, m, _) = ut_counter.segment();
                let day_delta = (dd + cm2dd(mm) - day_marker).to_string();


                // write record
                if hr_len != 60 {
                    println!("merged {} entries @ {}", hr_len, stopper);
                }
                csw.write_record(vec![&day_delta, &h.to_string(), &m.to_string(), hr_avg])?;

                // update counter to next 5m segment
                ut_counter = UTime::from_unix(stopper);
            }
        }
    }

    println!("OK");
    Ok(())
}

// ACC = YYYY-MM-DD HH:MM:SS.mmmmmm, ±0.1f/±0.1f/±0.1f

// BVP = YYYY-MM-DD HH:MM:SS.mmmmmm, ±.02f

// Dexcom = YYYY-MM-DD HH:MM:SS,
//      [event_type *0..12 & 12..],
//      event_subtype = 5..12,
//      patient_info = 0..4],
//      device_info = 4,
//      [source_device = 4..]
//      [glucose_value = *6,7,11,12 & 12..]
//      insulin_value = nil
//      carb_value = nil
//      duration = 8
//      glucose dx = 5,9
//      [transmitter_time = 12..]

// EDA = YYYY-MM-DD HH:MM:SS.mmm, .018f

// Food Log = YYYY-MM-DD, HH:MM:SS, YYYY-MM-DD HH:MM:SS, _, [logged_food], .02f, [unit], .01f, .01f, .01f, .01f, .01f, .01f

// HR = Type 1 or Type 2 format

// IBI = YYYY-MM-DD HH:MM:SS.mmmmmm, .016f

// TEMP = YYYY-MM-DD HH:MM:SS.mmm, 2.02f


// fn analyze_data(dataset)... <-- not the best idea... too situational this is overeng

/// Indexed Run-Length Encode zipped data. This means retrieval still depends on
///  the original dataset.
//fn irle_data(data: &Vec<Row>) -> (Vec<u64>, Vec<u8>) {}

/// Validate whether time column is chunked in 60s. Returns (<splits>, <counts>).
/// Standard thinking would be to generify this but as Abrash/grug alludes to test first, overengineer later.
fn validate_chunking(csr: &Vec<Row>) -> (Vec<usize>, Vec<u8>) {
    let mut iter = csr.iter();
    let mut counts = (Vec::<usize>::new(), Vec::<u8>::new());
    let mut index: (usize, u8) = (1, 0); // date index, count index
    let mut curr: &str = &iter.next().unwrap().time;  // Prefer over loading/checking every previous

    while let Some(row) = iter.next() {
        let time = &row.time;
                                                                    // Type 1 has "second" terminator @ 10 or 13 ▼
        if time.chars().take(16).eq(curr.chars().take(16)) { // Type 2 has second terminator @ 16 ▼
            index.1 += 1;                                           // Just comparing first 16 characters guarantees only minutes retaining behavior for both Type 1 and 2 ✩
        } else {
            index.0 += index.1 as usize;
            counts.0.push(index.0);
            counts.1.push(index.1);

            if index.1 != 60 {
                println!("{:?} bad ({:?}/60 >> @{:?})", &curr[..curr.len() - 3], index.1, index.0); // replace with ▶▶
            }

            index.1 = 1;
            curr = time;
        }
    }

    counts
}

/// Identify gaps within Type 1 dataset. Returns CSV row low-adjacent to offending line (e.g. [index, index+1] is gap)
fn id_gaps(times: &Vec<UTime>, splits: &Vec<usize>) -> Vec<usize> {
    println!("{}", splits.len());

    let mut gaps = Vec::<usize>::new();
    let mut spl_ind = 0; // TODO maybe replace .windows() with std counting for loop

    for w in times.windows(2) {
        spl_ind += 1;
        let (w1, w2) = (w.first().unwrap(), w.last().unwrap()); // To get rid of seconds, notice that rather than strip() — resulting in clone — some clever manip works!

        let mut diff = w2.diff(w1)
            .expect(format!("Bad diff: {} @ {}, {} @ {}", w1, splits[spl_ind-1], w2, splits[spl_ind]).as_str());
        let diff_u = diff.to_unix();

        if diff_u > 60 {
            let spl = splits[spl_ind - 1];
            let hrt: String = if (diff_u % 3600) / 60 == 1 { "<1m".parse().unwrap() } else { diff.strip().to_hrt() };
            println!("\"{:} — {:}\" gap >> ({} @{:?})", detail(w1.to_string().as_str(), 3), detail(w2.to_string().as_str(), 3), hrt, splits[spl_ind - 1]);
            gaps.push(spl);
        }
    }

    gaps
}

fn vis_gap(gap: u64) {
   ffi_py("script.py", "mpl_gap", vec![gap.to_string().as_str()]);
}

//
//
//
// // This is a terminal operation so "consume vector".
// fn visplot(data: Vec<(usize, usize)>, caption: &str, label_x: &str, label_y: &str) {
//     let axes_scl = unzip_max(&data);
//
//     evcxr_figure((640, 480), |root| {
//         let mut chart = ChartBuilder::on(&root)
//             .caption(caption, ("FOT-Skip std", 20).into_font())
//             .x_label_area_size(40)
//             .y_label_area_size(40)
//             .build_cartesian_2d(0usize..axes_scl.0, 0usize..axes_scl.1)?;
//
//         chart.configure_mesh()
//             .x_desc(label_x)
//             .y_desc(label_y)
//             .draw()?;
//
//         Ok(())
//     }).style("width: 60%");
// }

// pub(crate) fn rle_decode<T>(rle_data: Vec<(T, usize)>) -> Vec<T> {
//     // i wonder if there is a way to reinterpret the same vec memory as <T> instead of <(T, usize)> upon conversion? asm?
// }

// should be easy to adapt this to unzip_min using Rev()
pub(crate) fn unzip_max<T: Ord + Clone>(data: &Vec<(T, T)>) -> (T, T) {
    let (dat_x, dat_y): (Vec<_>, Vec<_>) = data.iter().cloned().unzip();
    (dat_x.iter().max().unwrap().clone(), dat_y.iter().max().unwrap().clone())
}
pub(crate) fn query(msg: &str) -> String {
    print!("{}", format!("{} ... ", msg));
    let _ = stdout().flush();

    let mut bfr = String::new();
    stdin().read_line(&mut bfr).expect("Bad user string");
    bfr.trim().to_string()
}

// pub(crate) fn query_til(bfr: &mut String, )

// Single month to days (aka PDF?) TODO: replace the paltry leap year patch with actual work iff several years involved
pub(crate) fn mm2dd(mm: u8) -> u8 { // https://cmcenroe.me/2014/12/05/days-in-month-formula.html
    if mm == 2 {
        29
    } else {
        28 + (mm + (mm / 8)) % 2 + (2 % mm) + 2 * (1 / mm)
    }
}

// Cumulative months to days (aka CDF?) TODO better naming please -n-
pub(crate) fn cm2dd(until_mm: u8) -> u8 {
    (1..until_mm).map(mm2dd).sum::<u8>()
}

//                                                          ▼ "borrow burrowing"... falling into a hole of borrowing. Best to learn how to
//                                                          ▼                       outright manage this instead of turning a blind eye.
/// (Brutal) macro for snipping string head into clone to avoid burrowing.
pub(crate) fn behead(str: &str, len: usize) -> String {
    str[len..].to_string()
}

/// Tail equivalent of behead.
pub(crate) fn detail(str: &str, len: usize) -> String {
    str[..str.len()-len].to_string()
}

/// Offset a range by specified amount.
pub(crate) fn offset_range<T: Add<Output = T> + Clone>(range: &Range<T>, offset: T) -> Range<T> {
    Range { start: range.start.clone() + offset.clone(), end: range.end.clone() + offset }
}

/// Stringify vec of ToString trait objects.
pub(crate) fn stringify_all<T: ToString>(items: &Vec<T>) -> Vec<String> {
    items.iter().map(|x| x.to_string()).collect()
}


//    ▼ see hn:44586064
//    ▼
/// "Artisanal" FFI for python :)
pub(crate) fn ffi_py(fname: &str, _func: &str, args: Vec<&str>) -> String {
 //   let (a,b,c)=(format!("{}venv/bin/python", PY_PATH), format!("{}{}", PY_PATH, fname), format!("import {}; print {}.{}({})", fname, fname, func, args.join(",")));

    let cmd = Command::new(format!("{}venv/bin/python", PY_PATH))
        .arg(format!("{}{}", PY_PATH, fname))
        .args(args)
        .output();

    String::from_utf8(cmd.unwrap().stderr).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn s_mm22dd() { // [s] standard, non-obj test
        assert_eq!(mm2dd(1), 31);
        assert_eq!(mm2dd(2), 29);
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
        let ut = UTime::from_sec(34, 1, 26, U24::try_from(680).unwrap());
        let ut2 = UTime::from_unix(ut.to_unix());
        assert_eq!(ut.diff(&ut2).unwrap().to_unix(), 0);
    }

    #[test]
    fn utime_step() {
        let mut ut = UTime::from_sec(34,4,23,U24::try_from(680).unwrap()); // 2004, 4, 23 ... 168, 2, 0
        ut.step(1);
        println!("{:?}", ut);
    }

    #[test]
    fn sc_hr15() { // sc = sanch = sanity check
        let (w1, w2) = (UTime::from_hr("2020-07-05 15:11:58"), UTime::from_hr("2020-07-05 15:11:59"));
        let diff = w2.diff(&w1);
    }

    #[test]
    fn sc_feb29() {
        let (w1, w2) = (UTime::from_hr("2020-02-29 23:59:59"), UTime::from_hr("2020-03-01 00:00:59"));
        let (d1, d2) = (w1.to_unix(), w2.to_unix());
        assert_eq!(w2.diff(&w1).unwrap().to_unix(), 60)
    }



    #[test]
    fn utime_2unix_base() { // [struct_name] is-obj test
        let ut = UTime::from_min(0, 1, 1, 300);
        assert_eq!(ut.to_unix(), 18000);
    }

    #[test]
    fn utime_2unix_mmdd() {
        let ut: UTime = UTime::from_min(0, 12, 8, 300);
        assert_eq!(ut.to_unix(), 29480400 + 86400);
    }

    #[test]
    fn utime_2unix_yymmdd() {
        let ut: UTime = UTime::from_min(34, 6,12,240);
        assert_eq!(ut.to_unix(), 1087012800 + 86400);
    }

    #[test]
    fn utime_24unix_fm() { // 2(u) and 4(rom) unix test
        let ut: UTime = UTime::from_min(32,3,8,300);
        let ut2: UTime = UTime::from_unix(ut.to_unix());
        assert_eq!(ut.to_unix(), ut2.to_unix());
    }

    #[test]
    fn utime_24unix_etern() {
        let ut = UTime::from_unix(63751447600);
        let ut2: UTime = UTime::from_unix(ut.to_unix());

        for i in 0..99999 {
            let mut utc = ut.clone();
            utc.step(i);
            println!("{}", utc);
            let ut_clone2 = UTime::from_unix(utc.to_unix());
        }

        assert_eq!(ut.to_unix(), ut2.to_unix());
    }

    #[test]
    fn utime_22920() {
        let year = UTime::from_hr("2020-01-01 00:00:00");
        assert_eq!(year.to_unix(), 333);
    }

}

