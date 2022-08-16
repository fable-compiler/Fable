
#[cfg(feature = "date")]
pub mod DateTime_ {
    use crate::{Native_::Lrc, String_::{string, self}};
    use chrono::{DateTime as CDT, TimeZone, Utc, Local, Datelike};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    enum LocalUtcWrap {
        CLocal(CDT<Local>),
        CUtc(CDT<Utc>),
    }

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct DateTime(LocalUtcWrap);

    // impl core::fmt::Display for DateTime {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }

    pub fn new_ymd(y: i32, m: i32, d: i32) -> DateTime {
        let l = Local.ymd(y, m as u32, d as u32).and_hms(0, 0, 0);
        DateTime(LocalUtcWrap::CLocal(l))
    }

    pub fn new_ymdhms(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32) -> DateTime {
        let l = Local.ymd(y, m as u32, d as u32).and_hms(h as u32, min as u32, s as u32);
        DateTime(LocalUtcWrap::CLocal(l))
    }

    pub fn new_ymdhmsms(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32, ms: i32) -> DateTime {
        let l = Local.ymd(y, m as u32, d as u32).and_hms_milli(h as u32, min as u32, s as u32, ms as u32);
        DateTime(LocalUtcWrap::CLocal(l))
    }

    impl DateTime {
        pub fn to_string(&self, stringFormat: string) -> string {
            let chronoFriendlyStringFormat = stringFormat
                .replace("yyyy", "%Y")
                .replace("MM", "%m")
                .replace("dd", "%d")
                .replace("ss", "%S")
                .replace("fff", "%3f");
            let s =
                match self.0 {
                    LocalUtcWrap::CLocal(dt) => dt.format(&chronoFriendlyStringFormat),
                    LocalUtcWrap::CUtc(dt) => dt.format(&chronoFriendlyStringFormat),
                };
            string(s.to_string().as_str())
        }

        pub fn year(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CLocal(dt) => dt.year(),
                LocalUtcWrap::CUtc(dt) => dt.year()
            }
        }
    }

    pub fn year() {}
}

#[cfg(feature = "date")]
pub mod DateTimeOffset_ {
    use crate::{Native_::Lrc, String_::string};
    use chrono::{DateTime as CDT, TimeZone, Utc};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct DateTimeOffset;

    // impl core::fmt::Display for DateTimeOffset {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }
}

#[cfg(feature = "date")]
pub mod TimeSpan_ {
    use crate::{Native_::Lrc, String_::string};
    use chrono::{DateTime as CDT, TimeZone, Utc};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct TimeSpan;

    // impl core::fmt::Display for TimeSpan {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }
}
