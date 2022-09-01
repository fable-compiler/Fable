#[cfg(feature = "date")]
pub mod DateTime_ {
    use crate::String_::{string, stringFrom};
    use chrono::{DateTime as CDT, Datelike, Local, TimeZone, Utc, NaiveDateTime, NaiveDate, Timelike};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    enum LocalUtcWrap {
        CLocal(CDT<Local>),
        CUtc(CDT<Utc>),
        CUnspecified(NaiveDateTime)
    }

    #[derive(Clone, Copy, PartialEq, Debug)]
    pub struct DateTime(LocalUtcWrap);

    // impl core::fmt::Display for DateTime {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }

    impl PartialOrd for DateTime {
        fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
            //self.0.partial_cmp(&other.0)
            // This is probably not sufficient and likely requires normalizing all dates to UTC first. Also what about Unspecified - perhaps these should return None?
            let x = self.get_timestamp();
            let y = other.get_timestamp();
            if x > y { Some(core::cmp::Ordering::Greater) }
            else if x < y { Some(core::cmp::Ordering::Less) }
            else { Some(core::cmp::Ordering::Equal) }
        }
    }

    pub fn new_ymd(y: i32, m: i32, d: i32) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32).and_hms(0, 0, 0);
        DateTime(LocalUtcWrap::CUnspecified(l))
    }

    pub fn new_ymdhms(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32)
            .and_hms(h as u32, min as u32, s as u32);
        DateTime(LocalUtcWrap::CUnspecified(l))
    }

    pub fn new_ymdhmsms(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32, ms: i32) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32)
            .and_hms_milli(h as u32, min as u32, s as u32, ms as u32);
        DateTime(LocalUtcWrap::CUnspecified(l))
    }

    pub fn new_ymdhms_withkind(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32, kind: i32) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32)
            .and_hms(h as u32, min as u32, s as u32);
        let dt =
            match kind {
                1 => LocalUtcWrap::CUtc(l.and_local_timezone(Utc).unwrap()),
                2 => LocalUtcWrap::CLocal(l.and_local_timezone(Local).unwrap()),
                0 => LocalUtcWrap::CUnspecified(l),
                _ => panic!("unsupported date kind. Only valid values are: 0 - Unspecified, 1 - Utc, 2 -> Local")
            };
        DateTime(dt)
    }

    pub fn now() -> DateTime {
        DateTime(LocalUtcWrap::CLocal(Local::now()))
    }

    pub fn minValue() -> DateTime {
        DateTime(LocalUtcWrap::CUtc(CDT::<Utc>::MIN_UTC))
    }

    pub fn maxValue() -> DateTime {
        DateTime(LocalUtcWrap::CUtc(CDT::<Utc>::MAX_UTC))
    }

    pub fn compare(x: DateTime, y: DateTime) -> i32 {
        if x > y { 1i32 } else { if x < y { -1i32 } else { 0i32 } }
    }

    pub fn equals(a: DateTime, b: DateTime) -> bool {
        a == b
    }

    impl DateTime {

        pub fn kind(&self) -> i32 {
            match self.0 {
                LocalUtcWrap::CUtc(t) => 1,
                LocalUtcWrap::CLocal(t) => 2,
                LocalUtcWrap::CUnspecified(t) => 0,
            }
        }

        pub fn to_local_time(&self) -> DateTime {
            match self.0 {
                LocalUtcWrap::CLocal(t) => self.clone(),
                LocalUtcWrap::CUtc(t) => DateTime(LocalUtcWrap::CLocal(t.into())),
                LocalUtcWrap::CUnspecified(t) => DateTime(LocalUtcWrap::CLocal(Local.from_local_datetime(&t).unwrap())),
            }
        }

        fn get_timestamp(&self) -> i64 {
            match self.0 {
                LocalUtcWrap::CLocal(dt) => dt.timestamp(),
                LocalUtcWrap::CUtc(dt)  => dt.timestamp(),
                LocalUtcWrap::CUnspecified(dt) => dt.timestamp(),
            }
        }

        pub fn to_string(&self, stringFormat: string) -> string {
            let chronoFriendlyStringFormat = stringFormat
                .replace("yyyy", "%Y")
                .replace("MM", "%m")
                .replace("dd", "%d")
                .replace("ss", "%S")
                .replace("fff", "%3f");
            let s = match self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.format(&chronoFriendlyStringFormat),
                LocalUtcWrap::CLocal(dt) => dt.format(&chronoFriendlyStringFormat),
                LocalUtcWrap::CUtc(dt) => dt.format(&chronoFriendlyStringFormat),
            };
            stringFrom(s.to_string())
        }

        pub fn year(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.year(),
                LocalUtcWrap::CLocal(dt) => dt.year(),
                LocalUtcWrap::CUtc(dt) => dt.year(),
            }
        }

        pub fn hour(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.hour() as i32,
                LocalUtcWrap::CLocal(dt) => dt.hour() as i32,
                LocalUtcWrap::CUtc(dt) => dt.hour() as i32,
            }
        }
    }

    pub fn year() {}
}

#[cfg(feature = "date")]
pub mod DateTimeOffset_ {
    use crate::String_::string;
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
    use crate::String_::string;
    use chrono::{DateTime as CDT, TimeZone, Utc};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct TimeSpan;

    // impl core::fmt::Display for TimeSpan {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }
}
