#[cfg(feature = "datetime")]
pub mod DateTime_ {
    use crate::{
        DateTimeOffset_::DateTimeOffset,
        Native_::compare,
        String_::{fromString, string},
        TimeSpan_::{
            from_days, from_hours, from_microseconds, from_milliseconds, from_minutes,
            from_seconds, from_ticks, nanoseconds_per_tick, ticks_per_second, TimeSpan,
        },
    };
    use chrono::{
        DateTime as CDateTime, Datelike, Duration, FixedOffset, Local, NaiveDate, NaiveDateTime,
        NaiveTime, Offset, TimeZone, Timelike, Utc,
    };
    use core::ops::{Add, Sub};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    enum DateTimeKind {
        Utc(CDateTime<Utc>),
        Local(CDateTime<Local>),
        Unspecified(NaiveDateTime),
    }

    #[derive(Clone, Copy, Debug)]
    pub struct DateTime(DateTimeKind);

    impl core::fmt::Display for DateTime {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            let str = match self.0 {
                DateTimeKind::Utc(dt) => dt.to_string(),
                DateTimeKind::Local(dt) => dt.to_string(),
                DateTimeKind::Unspecified(dt) => dt.to_string(),
            };
            write!(f, "{}", str)
        }
    }

    impl PartialEq for DateTime {
        fn eq(&self, other: &Self) -> bool {
            if (self.0 == other.0) {
                true
            } else {
                let x = self.get_cdt_with_offset();
                let y = other.get_cdt_with_offset();
                x == y
            }
        }
    }

    impl PartialOrd for DateTime {
        fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
            let x = self.get_cdt_with_offset();
            let y = other.get_cdt_with_offset();
            x.partial_cmp(&y)
        }
    }

    pub fn compareTo(x: DateTime, y: DateTime) -> i32 {
        compare(&x, &y)
    }

    pub fn equals(a: DateTime, b: DateTime) -> bool {
        a == b
    }

    pub fn new() -> DateTime {
        minValue()
    }

    pub fn new_ticks(ticks: i64) -> DateTime {
        minValue().add(from_ticks(ticks))
    }

    pub fn new_ymd(y: i32, m: i32, d: i32) -> DateTime {
        let dt = NaiveDate::from_ymd_opt(y, m as u32, d as u32)
            .unwrap()
            .and_hms_opt(0, 0, 0)
            .unwrap();
        DateTime(DateTimeKind::Unspecified(dt))
    }

    pub fn new_ymdhms(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32) -> DateTime {
        let dt = NaiveDate::from_ymd_opt(y, m as u32, d as u32)
            .unwrap()
            .and_hms_opt(h as u32, min as u32, s as u32)
            .unwrap();
        DateTime(DateTimeKind::Unspecified(dt))
    }

    pub fn new_ymdhms_milli(y: i32, m: i32, d: i32, h: i32, min: i32, s: i32, ms: i32) -> DateTime {
        let dt = NaiveDate::from_ymd_opt(y, m as u32, d as u32)
            .unwrap()
            .and_hms_milli_opt(h as u32, min as u32, s as u32, ms as u32)
            .unwrap();
        DateTime(DateTimeKind::Unspecified(dt))
    }

    pub fn new_ndt_withkind(dt: NaiveDateTime, kind: i32) -> DateTime {
        let dtKind =
            match kind {
                1 => DateTimeKind::Utc(dt.and_local_timezone(Utc).unwrap()),
                2 => DateTimeKind::Local(dt.and_local_timezone(Local).unwrap()),
                0 => DateTimeKind::Unspecified(dt),
                _ => panic!("unsupported date kind. Only valid values are: 0 - Unspecified, 1 - Utc, 2 -> Local")
            };
        DateTime(dtKind)
    }

    // pub fn new_Date_Time(d: DateOnly, t: TimeOnly, kind: i32) -> DateTime {
    //     let dtKind =
    //         match kind {
    //             1 => DateTimeKind::Utc(dt.and_local_timezone(Utc).unwrap()),
    //             2 => DateTimeKind::Local(dt.and_local_timezone(Local).unwrap()),
    //             0 => DateTimeKind::Unspecified(dt),
    //             _ => panic!("unsupported date kind. Only valid values are: 0 - Unspecified, 1 - Utc, 2 -> Local")
    //         };
    //     DateTime(dtKind)
    // }

    pub fn new_ymdhms_withkind(
        y: i32,
        m: i32,
        d: i32,
        h: i32,
        min: i32,
        s: i32,
        kind: i32,
    ) -> DateTime {
        let dt = NaiveDate::from_ymd_opt(y, m as u32, d as u32)
            .unwrap()
            .and_hms_opt(h as u32, min as u32, s as u32)
            .unwrap();
        new_ndt_withkind(dt, kind)
    }

    pub fn new_ymdhms_milli_withkind(
        y: i32,
        m: i32,
        d: i32,
        h: i32,
        min: i32,
        s: i32,
        milli: i32,
        kind: i32,
    ) -> DateTime {
        let dt = NaiveDate::from_ymd_opt(y, m as u32, d as u32)
            .unwrap()
            .and_hms_milli_opt(h as u32, min as u32, s as u32, milli as u32)
            .unwrap();
        new_ndt_withkind(dt, kind)
    }

    pub fn new_ymdhms_micro_withkind(
        y: i32,
        m: i32,
        d: i32,
        h: i32,
        min: i32,
        s: i32,
        milli: i32,
        micro: i32,
        kind: i32,
    ) -> DateTime {
        let dt = NaiveDate::from_ymd_opt(y, m as u32, d as u32)
            .unwrap()
            .and_hms_micro_opt(
                h as u32,
                min as u32,
                s as u32,
                (milli * 1000 + micro) as u32,
            )
            .unwrap();
        new_ndt_withkind(dt, kind)
    }

    pub fn now() -> DateTime {
        DateTime(DateTimeKind::Local(Local::now()))
    }

    pub fn utcNow() -> DateTime {
        DateTime(DateTimeKind::Utc(Utc::now()))
    }

    pub fn minValue() -> DateTime {
        let d = NaiveDate::from_ymd_opt(1, 1, 1).unwrap();
        let dt = d.and_hms_opt(0, 0, 0).unwrap();
        DateTime(DateTimeKind::Utc(CDateTime::from_utc(dt, Utc)))
    }

    pub fn maxValue() -> DateTime {
        let ns = Duration::nanoseconds(nanoseconds_per_tick);
        let d = NaiveDate::from_ymd_opt(10000, 1, 1).unwrap();
        let dt = d.and_hms_opt(0, 0, 0).unwrap() - ns; // one tick before year 10000
        DateTime(DateTimeKind::Utc(CDateTime::from_utc(dt, Utc)))
    }

    pub fn unixEpoch() -> DateTime {
        DateTime(DateTimeKind::Utc(Utc.timestamp_millis_opt(0).unwrap()))
    }

    // https://docs.microsoft.com/en-us/dotnet/api/system.datetime.ticks?view=net-6.0
    fn get_ticks_from_ndt(ndt: NaiveDateTime) -> i64 {
        let dayTicks = ((ndt.num_days_from_ce() - 1) as i64) * 24 * 60 * 60 * ticks_per_second;
        let secondsTicks = (ndt.num_seconds_from_midnight() as i64) * ticks_per_second;
        let subsecondTicks = (ndt.timestamp_subsec_nanos() as i64) / 100;
        dayTicks + secondsTicks + subsecondTicks
    }

    pub fn today() -> DateTime {
        let cdt = Utc::now();
        new_ymdhms_withkind(cdt.year(), cdt.month() as i32, cdt.day() as i32, 0, 0, 0, 1)
    }

    pub fn specifyKind(dt: DateTime, kind: i32) -> DateTime {
        let naive_dt = match dt.0 {
            DateTimeKind::Utc(dt) => dt.naive_utc(),
            DateTimeKind::Local(dt) => dt.naive_local(),
            DateTimeKind::Unspecified(dt) => dt,
        };
        let dtKind = match kind {
            1 => DateTimeKind::Utc(naive_dt.and_local_timezone(Utc).unwrap()),
            2 => DateTimeKind::Local(naive_dt.and_local_timezone(Local).unwrap()),
            0 => DateTimeKind::Unspecified(naive_dt),
            _ => panic!("Unsupported date kind. Only valid values are: 0 - Unspecified, 1 - Utc, 2 -> Local")
        };
        DateTime(dtKind)
    }

    impl DateTime {
        pub fn add(&self, ts: TimeSpan) -> DateTime {
            let ns = Duration::nanoseconds(ts.ticks() * nanoseconds_per_tick);
            let dtKind = match self.0 {
                DateTimeKind::Utc(dt) => DateTimeKind::Utc(dt + ns),
                DateTimeKind::Local(dt) => DateTimeKind::Local(dt + ns),
                DateTimeKind::Unspecified(dt) => DateTimeKind::Unspecified(dt + ns),
            };
            DateTime(dtKind)
        }

        pub fn subtract(&self, ts: TimeSpan) -> DateTime {
            let ns = Duration::nanoseconds(ts.ticks() * nanoseconds_per_tick);
            let dtKind = match self.0 {
                DateTimeKind::Utc(dt) => DateTimeKind::Utc(dt - ns),
                DateTimeKind::Local(dt) => DateTimeKind::Local(dt - ns),
                DateTimeKind::Unspecified(dt) => DateTimeKind::Unspecified(dt - ns),
            };
            DateTime(dtKind)
        }

        pub fn subtract2(&self, other: DateTime) -> TimeSpan {
            let x = self.get_cdt_with_offset();
            let y = other.get_cdt_with_offset();
            let ns = (x - y).num_nanoseconds().unwrap();
            from_ticks(ns / nanoseconds_per_tick)
        }

        pub fn kind(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => 1,
                DateTimeKind::Local(dt) => 2,
                DateTimeKind::Unspecified(dt) => 0,
            }
        }

        pub fn ticks(&self) -> i64 {
            // self.toUniversalTime().subtract2(minValue()).ticks()
            get_ticks_from_ndt(self.get_naive_dt())
        }

        pub fn date(&self) -> DateTime {
            //new_ymdhms_withkind(utcNow.year(), utcNow.month() as i32, utcNow.day() as i32,0, 0, 0, 1);
            match self.0 {
                DateTimeKind::Utc(dt) => {
                    new_ymdhms_withkind(dt.year(), dt.month() as i32, dt.day() as i32, 0, 0, 0, 1)
                }
                DateTimeKind::Local(dt) => {
                    new_ymdhms_withkind(dt.year(), dt.month() as i32, dt.day() as i32, 0, 0, 0, 2)
                }
                DateTimeKind::Unspecified(dt) => {
                    new_ymdhms_withkind(dt.year(), dt.month() as i32, dt.day() as i32, 0, 0, 0, 0)
                }
            }
        }

        pub fn toLocalTime(&self) -> DateTime {
            match self.0 {
                DateTimeKind::Utc(dt) => DateTime(DateTimeKind::Local(dt.into())),
                DateTimeKind::Local(dt) => self.clone(),
                DateTimeKind::Unspecified(dt) => {
                    // todo - need to check .NET implementation to see how it handles unspecified
                    DateTime(DateTimeKind::Local(Local.from_local_datetime(&dt).unwrap()))
                }
            }
        }

        pub fn toUniversalTime(&self) -> DateTime {
            match self.0 {
                DateTimeKind::Utc(dt) => self.clone(),
                DateTimeKind::Local(dt) => DateTime(DateTimeKind::Utc(dt.into())),
                DateTimeKind::Unspecified(dt) => {
                    // todo - need to check .NET implementation to see how it handles unspecified
                    DateTime(DateTimeKind::Utc(Utc.from_utc_datetime(&dt)))
                }
            }
        }

        pub fn localDateTime(&self) -> DateTime {
            self.toLocalTime()
        }

        pub fn utcDateTime(&self) -> DateTime {
            self.toUniversalTime()
        }

        pub fn toString(&self, format: string) -> string {
            let fmt = format
                .replace("yyyy", "%Y")
                .replace("MM", "%m")
                .replace("dd", "%d")
                .replace("ss", "%S")
                .replace("fff", "%3f");
            let df = match self.0 {
                DateTimeKind::Utc(dt) => dt.format(&fmt),
                DateTimeKind::Local(dt) => dt.format(&fmt),
                DateTimeKind::Unspecified(dt) => dt.format(&fmt),
            };
            fromString(df.to_string())
        }

        pub fn year(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.year(),
                DateTimeKind::Local(dt) => dt.year(),
                DateTimeKind::Unspecified(dt) => dt.year(),
            }
        }

        pub fn month(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.month() as i32,
                DateTimeKind::Local(dt) => dt.month() as i32,
                DateTimeKind::Unspecified(dt) => dt.month() as i32,
            }
        }

        pub fn day(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.day() as i32,
                DateTimeKind::Local(dt) => dt.day() as i32,
                DateTimeKind::Unspecified(dt) => dt.day() as i32,
            }
        }

        pub fn hour(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.hour() as i32,
                DateTimeKind::Local(dt) => dt.hour() as i32,
                DateTimeKind::Unspecified(dt) => dt.hour() as i32,
            }
        }

        pub fn minute(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.minute() as i32,
                DateTimeKind::Local(dt) => dt.minute() as i32,
                DateTimeKind::Unspecified(dt) => dt.minute() as i32,
            }
        }

        pub fn second(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.second() as i32,
                DateTimeKind::Local(dt) => dt.second() as i32,
                DateTimeKind::Unspecified(dt) => dt.second() as i32,
            }
        }

        pub fn millisecond(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.timestamp_subsec_millis() as i32,
                DateTimeKind::Local(dt) => dt.timestamp_subsec_millis() as i32,
                DateTimeKind::Unspecified(dt) => dt.timestamp_subsec_millis() as i32,
            }
        }

        pub fn microsecond(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.timestamp_subsec_micros() as i32,
                DateTimeKind::Local(dt) => dt.timestamp_subsec_micros() as i32,
                DateTimeKind::Unspecified(dt) => dt.timestamp_subsec_micros() as i32,
            }
        }

        pub fn nanosecond(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.timestamp_subsec_nanos() as i32,
                DateTimeKind::Local(dt) => dt.timestamp_subsec_nanos() as i32,
                DateTimeKind::Unspecified(dt) => dt.timestamp_subsec_nanos() as i32,
            }
        }

        pub fn timeOfDay(&self) -> TimeSpan {
            let t = match self.0 {
                DateTimeKind::Utc(dt) => dt.time(),
                DateTimeKind::Local(dt) => dt.time(),
                DateTimeKind::Unspecified(dt) => dt.time(),
            };
            let ns = (t - NaiveTime::MIN).num_nanoseconds().unwrap();
            from_ticks(ns / nanoseconds_per_tick)
        }

        // todo implement as DayOfWeek enum https://docs.microsoft.com/en-us/dotnet/api/system.dayofweek?view=net-6.0
        pub fn dayOfWeek(&self) -> i32 {
            let weekday = match self.0 {
                DateTimeKind::Utc(dt) => dt.weekday(),
                DateTimeKind::Local(dt) => dt.weekday(),
                DateTimeKind::Unspecified(dt) => dt.weekday(),
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

        pub fn dayOfMonth(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.day() as i32,
                DateTimeKind::Local(dt) => dt.day() as i32,
                DateTimeKind::Unspecified(dt) => dt.day() as i32,
            }
        }

        pub fn dayOfYear(&self) -> i32 {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.ordinal() as i32,
                DateTimeKind::Local(dt) => dt.ordinal() as i32,
                DateTimeKind::Unspecified(dt) => dt.ordinal() as i32,
            }
        }

        pub fn addYears(&self, years: i32) -> DateTime {
            match self.0 {
                DateTimeKind::Utc(dt) => new_ymdhms_withkind(
                    dt.year() + years,
                    dt.month() as i32,
                    dt.day() as i32,
                    dt.hour() as i32,
                    dt.minute() as i32,
                    dt.second() as i32,
                    1,
                ),
                DateTimeKind::Local(dt) => new_ymdhms_withkind(
                    dt.year() + years,
                    dt.month() as i32,
                    dt.day() as i32,
                    dt.hour() as i32,
                    dt.minute() as i32,
                    dt.second() as i32,
                    2,
                ),
                DateTimeKind::Unspecified(dt) => new_ymdhms_withkind(
                    dt.year() + years,
                    dt.month() as i32,
                    dt.day() as i32,
                    dt.hour() as i32,
                    dt.minute() as i32,
                    dt.second() as i32,
                    0,
                ),
            }
        }

        // Placeholder implementation
        //https://stackoverflow.com/questions/64081289/how-do-i-add-a-month-to-a-chrono-naivedate
        pub fn addMonths(&self, months: i32) -> DateTime {
            // match self.0 {
            //     DateTimeKind::Utc(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 1),
            //     DateTimeKind::Local(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 2),
            //     DateTimeKind::Unspecified(dt) => new_ymdhms_withkind(dt.year(), dt.month() as i32 + months, dt.day() as i32, dt.hour() as i32, dt.minute() as i32, dt.second() as i32, 0),
            // }
            match self.0 {
                DateTimeKind::Utc(dt) => new_ymdhms_withkind(
                    dt.year(),
                    dt.month() as i32 + months,
                    dt.day() as i32,
                    0,
                    0,
                    0,
                    1,
                ),
                DateTimeKind::Local(dt) => new_ymdhms_withkind(
                    dt.year(),
                    dt.month() as i32 + months,
                    dt.day() as i32,
                    0,
                    0,
                    0,
                    2,
                ),
                DateTimeKind::Unspecified(dt) => new_ymdhms_withkind(
                    dt.year(),
                    dt.month() as i32 + months,
                    dt.day() as i32,
                    0,
                    0,
                    0,
                    0,
                ),
            }
        }

        pub fn addDays(&self, days: f64) -> DateTime {
            self.add(from_days(days))
        }

        pub fn addHours(&self, hours: f64) -> DateTime {
            self.add(from_hours(hours))
        }

        pub fn addMinutes(&self, minutes: f64) -> DateTime {
            self.add(from_minutes(minutes))
        }

        pub fn addSeconds(&self, seconds: f64) -> DateTime {
            self.add(from_seconds(seconds))
        }

        pub fn addMilliseconds(&self, millis: f64) -> DateTime {
            self.add(from_milliseconds(millis))
        }

        pub fn addMicroseconds(&self, micros: f64) -> DateTime {
            self.add(from_microseconds(micros))
        }

        pub fn addTicks(&self, ticks: i64) -> DateTime {
            self.add(from_ticks(ticks))
        }

        pub(crate) fn get_naive_dt(&self) -> NaiveDateTime {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.naive_utc(),
                DateTimeKind::Local(dt) => dt.naive_local(),
                DateTimeKind::Unspecified(dt) => dt,
            }
        }

        pub(crate) fn get_cdt_with_offset(&self) -> CDateTime<FixedOffset> {
            match self.0 {
                DateTimeKind::Utc(dt) => dt.with_timezone(&FixedOffset::west_opt(0).unwrap()),
                DateTimeKind::Local(dt) => dt.with_timezone(&dt.offset().fix()),
                DateTimeKind::Unspecified(dt) => Utc.from_utc_datetime(&dt).into(),
            }
        }
    }

    impl Add<TimeSpan> for DateTime {
        type Output = DateTime;

        fn add(self, rhs: TimeSpan) -> Self::Output {
            DateTime::add(&self, rhs)
        }
    }

    impl Sub<TimeSpan> for DateTime {
        type Output = DateTime;

        fn sub(self, rhs: TimeSpan) -> Self::Output {
            self.subtract(rhs)
        }
    }

    impl Sub<DateTime> for DateTime {
        type Output = TimeSpan;

        fn sub(self, rhs: DateTime) -> Self::Output {
            self.subtract2(rhs)
        }
    }
}
