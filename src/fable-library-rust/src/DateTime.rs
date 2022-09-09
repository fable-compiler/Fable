#[cfg(feature = "date")]
pub mod DateTime_ {
    use core::ops::Add;

    use crate::{
        DateTimeOffset_::DateTimeOffset,
        String_::{string, stringFrom}, TimeSpan_::{TimeSpan, num_ticks_per_second},
    };
    use chrono::{
        DateTime as CDT, Datelike, FixedOffset, Local, NaiveDate, NaiveDateTime, Offset, TimeZone,
        Timelike, Utc, Duration,
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
            if (self.0 == other.0) {
                true
            } else {
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

    pub fn op_Subtraction(a: DateTime, b: DateTime) -> TimeSpan {
        crate::TimeSpan_::new_ticks(get_ticks(a) - get_ticks(b))
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

    pub fn new_ymdhmsms_withkind(
        y: i32,
        m: i32,
        d: i32,
        h: i32,
        min: i32,
        s: i32,
        ms: i32,
        kind: i32,
    ) -> DateTime {
        let l = NaiveDate::from_ymd(y, m as u32, d as u32).and_hms_milli(h as u32, min as u32, s as u32, ms as u32);
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

    // as per docs here:
    // https://docs.microsoft.com/en-us/dotnet/api/system.datetime.ticks?view=net-6.0
    fn get_ticks_from_ndt(ndt: NaiveDateTime) -> i64 {
        let dayTicks = ((ndt.num_days_from_ce() - 1) as i64) * 24 * 60 * 60 * num_ticks_per_second;
        let secondsTicks = (ndt.num_seconds_from_midnight() as i64) * num_ticks_per_second;
        let subsecondTicks = (ndt.timestamp_subsec_millis() as i64) * num_ticks_per_second / 1000;
        dayTicks + secondsTicks + subsecondTicks
    }

    pub fn get_ticks(dt: DateTime) -> i64 {
        match dt.0 {
            LocalUtcWrap::CUtc(dt) => get_ticks_from_ndt(dt.naive_utc()),
            LocalUtcWrap::CLocal(dt) => get_ticks_from_ndt(dt.naive_local()),
            LocalUtcWrap::CUnspecified(dt) => get_ticks_from_ndt(dt),
        }
    }

    pub fn today() -> DateTime {
        let utcNow = chrono::Utc::now();
        let next = new_ymdhms_withkind(utcNow.year(), utcNow.month() as i32, utcNow.day() as i32,0, 0, 0, 1);
        next
    }

    pub fn specify_kind(dt: DateTime, kind: i32) -> DateTime {
        let naive_dt =
            match dt.0 {
                LocalUtcWrap::CLocal(dt) => dt.naive_local(),
                LocalUtcWrap::CUtc(dt) => dt.naive_utc(),
                LocalUtcWrap::CUnspecified(dt) => dt,
            };

        let res = match kind {
            1 => LocalUtcWrap::CUtc(naive_dt.and_local_timezone(Utc).unwrap()),
            2 => LocalUtcWrap::CLocal(naive_dt.and_local_timezone(Local).unwrap()),
            0 => LocalUtcWrap::CUnspecified(naive_dt),
            _ => panic!("unsupported date kind. Only valid values are: 0 - Unspecified, 1 - Utc, 2 -> Local")
        };
        DateTime(res)
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

        pub fn month(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.month() as i32,
                LocalUtcWrap::CLocal(dt) => dt.month() as i32,
                LocalUtcWrap::CUtc(dt) => dt.month() as i32,
            }
        }

        pub fn day(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.day() as i32,
                LocalUtcWrap::CLocal(dt) => dt.day() as i32,
                LocalUtcWrap::CUtc(dt) => dt.day() as i32,
            }
        }

        pub fn hour(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.hour() as i32,
                LocalUtcWrap::CLocal(dt) => dt.hour() as i32,
                LocalUtcWrap::CUtc(dt) => dt.hour() as i32,
            }
        }

        pub fn minute(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.minute() as i32,
                LocalUtcWrap::CLocal(dt) => dt.minute() as i32,
                LocalUtcWrap::CUtc(dt) => dt.minute() as i32,
            }
        }


        pub fn second(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.second() as i32,
                LocalUtcWrap::CLocal(dt) => dt.second() as i32,
                LocalUtcWrap::CUtc(dt) => dt.second() as i32,
            }
        }

        pub fn millisecond(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.timestamp_subsec_millis() as i32,
                LocalUtcWrap::CLocal(dt) => dt.timestamp_subsec_millis() as i32,
                LocalUtcWrap::CUtc(dt) => dt.timestamp_subsec_millis() as i32,
            }
        }

        pub fn date(&self) -> DateTime {
            //new_ymdhms_withkind(utcNow.year(), utcNow.month() as i32, utcNow.day() as i32,0, 0, 0, 1);
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32, dt.day() as i32, 0, 0, 0, 0),
                LocalUtcWrap::CLocal(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32, dt.day() as i32, 0, 0, 0, 2),
                LocalUtcWrap::CUtc(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32, dt.day() as i32, 0, 0, 0, 1),
            }
        }

        // todo implement as DayOfWeek enum https://docs.microsoft.com/en-us/dotnet/api/system.dayofweek?view=net-6.0
        pub fn day_of_week(&self) -> i32 {
            let weekday =
                match &self.0 {
                    LocalUtcWrap::CUnspecified(dt) => dt.weekday(),
                    LocalUtcWrap::CLocal(dt) =>dt.weekday(),
                    LocalUtcWrap::CUtc(dt) => dt.weekday(),
                };

            match weekday {
                chrono::Weekday::Mon => 1,
                chrono::Weekday::Tue => 2,
                chrono::Weekday::Wed => 3,
                chrono::Weekday::Thu => 4,
                chrono::Weekday::Fri => 5,
                chrono::Weekday::Sat => 6,
                chrono::Weekday::Sun => 0,
            }
        }

        pub fn day_of_month(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.day() as i32,
                LocalUtcWrap::CLocal(dt) =>dt.day() as i32,
                LocalUtcWrap::CUtc(dt) => dt.day() as i32,
            }
        }

        pub fn day_of_year(&self) -> i32 {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => dt.ordinal() as i32,
                LocalUtcWrap::CLocal(dt) =>dt.ordinal() as i32,
                LocalUtcWrap::CUtc(dt) => dt.ordinal() as i32,
            }
        }

        // Placeholder implementation
        pub fn add_years(&self, years: i32) -> DateTime {
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => new_ymdhms_withkind(dt.year() + years, dt.month() as i32, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 0),
                LocalUtcWrap::CLocal(dt) => new_ymdhms_withkind(dt.year() + years, dt.month() as i32, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 2),
                LocalUtcWrap::CUtc(dt) => new_ymdhms_withkind(dt.year() + years, dt.month() as i32, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 1),
            }
        }

        // Placeholder implementation
        //https://stackoverflow.com/questions/64081289/how-do-i-add-a-month-to-a-chrono-naivedate
        pub fn add_months(&self, months: i32) -> DateTime {
            // match &self.0 {
            //     LocalUtcWrap::CUnspecified(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 0),
            //     LocalUtcWrap::CLocal(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 2),
            //     LocalUtcWrap::CUtc(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 1),
            // }
            match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, 0, 0, 0, 0),
                LocalUtcWrap::CLocal(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, 0, 0, 0, 2),
                LocalUtcWrap::CUtc(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, 0, 0, 0, 1),
            }
        }

        pub fn add_days(&self, days: f64) -> DateTime {
            let next = match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => LocalUtcWrap::CUnspecified(dt.add(Duration::days(days as i64))),
                LocalUtcWrap::CLocal(dt) =>LocalUtcWrap::CLocal(dt.add(Duration::days(days as i64))),
                LocalUtcWrap::CUtc(dt) => LocalUtcWrap::CUtc(dt.add(Duration::days(days as i64))),
            };
            DateTime(next)
        }

        pub fn add_hours(&self, hours: f64) -> DateTime {
            let next = match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => LocalUtcWrap::CUnspecified(dt.add(Duration::hours(hours as i64))),
                LocalUtcWrap::CLocal(dt) =>LocalUtcWrap::CLocal(dt.add(Duration::hours(hours as i64))),
                LocalUtcWrap::CUtc(dt) => LocalUtcWrap::CUtc(dt.add(Duration::hours(hours as i64))),
            };
            DateTime(next)
        }

        pub fn add_minutes(&self, minutes: f64) -> DateTime {
            self.add_milliseconds(minutes * 60.0 * 1000.0)
        }

        pub fn add_seconds(&self, seconds: f64) -> DateTime {
            self.add_milliseconds(seconds * 1000.0)
        }

        pub fn add_milliseconds(&self, milliseconds: f64) -> DateTime {
            let next = match &self.0 {
                LocalUtcWrap::CUnspecified(dt) => LocalUtcWrap::CUnspecified(dt.add(Duration::milliseconds(milliseconds as i64))),
                LocalUtcWrap::CLocal(dt) =>LocalUtcWrap::CLocal(dt.add(Duration::milliseconds(milliseconds as i64))),
                LocalUtcWrap::CUtc(dt) => LocalUtcWrap::CUtc(dt.add(Duration::milliseconds(milliseconds as i64))),
            };
            DateTime(next)
        }

        pub fn add_ticks(&self, ticks: i64) -> DateTime {
            let ticks_per_ms = num_ticks_per_second / 1000;
            let ms = ticks / ticks_per_ms;
            self.add_milliseconds(ms as f64)
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
