#[cfg(feature = "date")]
pub mod DateTime_ {
    use crate::{String_::{string, stringFrom}, DateTimeOffset_::DateTimeOffset};
    use chrono::{
        DateTime as CDT, Datelike, Local, NaiveDate, NaiveDateTime, TimeZone, Timelike, Utc, FixedOffset, Offset,
    };

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    enum LocalUtcWrap {
        CLocal(CDT<Local>),
        CUtc(CDT<Utc>),
        CUnspecified(NaiveDateTime),
    }

    #[derive(Clone, Copy, Debug)]
    pub struct DateTime(LocalUtcWrap);

    // impl core::fmt::Display for DateTime {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }

    impl PartialEq for DateTime {
        fn eq(&self, other: &Self) -> bool {
            if(self.0 == other.0) {
                true
            }
            else {
                let selfUtc = self.to_universal_time();
                let otherUtc = other.to_universal_time();
                selfUtc.0 == otherUtc.0
            }
        }
    }

    impl PartialOrd for DateTime {
        fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
            let x = self.to_universal_time().get_timestamp();
            let y = other.to_universal_time().get_timestamp();
            if x > y {
                Some(core::cmp::Ordering::Greater)
            } else if x < y {
                Some(core::cmp::Ordering::Less)
            } else {
                Some(core::cmp::Ordering::Equal)
            }
        }
    }

    pub fn new_ymd(y: i32, m: i32, d: i32) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32).and_hms(0, 0, 0);
        DateTime(LocalUtcWrap::CUnspecified(l))
    }

    pub fn new_ymdhms(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32).and_hms(h as u32, min as u32, s as u32);
        DateTime(LocalUtcWrap::CUnspecified(l))
    }

    pub fn new_ymdhmsms(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32, ms: i32) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32)
            .and_hms_milli(h as u32, min as u32, s as u32, ms as u32);
        DateTime(LocalUtcWrap::CUnspecified(l))
    }

    pub fn new_ymdhms_withkind(
        y: i32,
        m: i32,
        d: i32,
        h: i32,
        min: i32,
        s: i32,
        kind: i32,
    ) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32).and_hms(h as u32, min as u32, s as u32);
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
        if x > y {
            1i32
        } else {
            if x < y {
                -1i32
            } else {
                0i32
            }
        }
    }

    pub fn from_date_time_offset(dto: DateTimeOffset, kind: i32) -> DateTime {
        let cdt = dto.get_cdt_with_offset();
        let l = cdt.naive_utc() - cdt.offset().fix();
        let dt =
            match kind {
                1 => LocalUtcWrap::CUtc(l.and_local_timezone(Utc).unwrap()),
                2 => LocalUtcWrap::CLocal(l.and_local_timezone(Local).unwrap()),
                0 => LocalUtcWrap::CUnspecified(l),
                _ => panic!("unsupported date kind. Only valid values are: 0 - Unspecified, 1 - Utc, 2 -> Local")
            };
        DateTime(dt)
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
                LocalUtcWrap::CUnspecified(t) => {
                    // todo - need to check .NET implementation to see how it handles unspecified
                    DateTime(LocalUtcWrap::CLocal(Local.from_local_datetime(&t).unwrap()))
                }
            }
        }

        pub fn to_universal_time(&self) -> DateTime {
            match self.0 {
                LocalUtcWrap::CLocal(t) => DateTime(LocalUtcWrap::CUtc(t.into())),
                LocalUtcWrap::CUtc(t) => self.clone(),
                LocalUtcWrap::CUnspecified(t) => {
                    // todo - need to check .NET implementation to see how it handles unspecified
                    DateTime(LocalUtcWrap::CUtc(Utc.from_utc_datetime(&t)))
                }
            }
        }

        fn get_timestamp(&self) -> i64 {
            match self.0 {
                LocalUtcWrap::CLocal(dt) => dt.timestamp(),
                LocalUtcWrap::CUtc(dt) => dt.timestamp(),
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

        pub(crate) fn to_cdt_with_offset(&self) -> CDT<FixedOffset> {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => Utc.from_utc_datetime(&dt).into(),
                LocalUtcWrap::CLocal(dt) => dt.with_timezone(&dt.offset().fix()),
                LocalUtcWrap::CUtc(dt) => dt.with_timezone(&FixedOffset::west(0)),
            }
        }
    }

    pub fn year() {}
}

#[cfg(feature = "date")]
pub mod DateTimeOffset_ {
    use crate::{String_::string, DateTime_::DateTime};
    use chrono::{DateTime as CDT, TimeZone, Utc, NaiveDateTime, FixedOffset};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct DateTimeOffset(CDT<FixedOffset>);

    pub fn from_date(d: DateTime) -> DateTimeOffset {
        let cdto = d.to_cdt_with_offset();
        DateTimeOffset(cdto)
    }

    impl DateTimeOffset {
        pub(crate) fn get_cdt_with_offset(&self) -> CDT<FixedOffset> {
            self.0
        }
    }

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
