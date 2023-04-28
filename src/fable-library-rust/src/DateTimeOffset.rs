#[cfg(feature = "datetime")]
pub mod DateTimeOffset_ {
    use crate::{
        DateTime_::{new_ndt_withkind, new_ymd, DateTime},
        Native_::compare,
        String_::{fromString, string},
        TimeSpan_::{
            from_days, from_hours, from_microseconds, from_milliseconds, from_minutes,
            from_seconds, from_ticks, nanoseconds_per_tick, TimeSpan,
        },
    };
    use chrono::{
        DateTime as CDateTime, Datelike, Duration, FixedOffset, Local, NaiveDate, NaiveDateTime,
        NaiveTime, TimeZone, Timelike, Utc,
    };
    use core::ops::{Add, Sub};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct DateTimeOffset(CDateTime<FixedOffset>);

    impl core::fmt::Display for DateTimeOffset {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.0.to_string())
        }
    }

    pub fn new() -> DateTimeOffset {
        minValue()
    }

    pub fn new_ticks(ticks: i64, ts: TimeSpan) -> DateTimeOffset {
        minValue().add(from_ticks(ticks))
    }

    pub fn new_DateTime(dt: DateTime, ts: TimeSpan) -> DateTimeOffset {
        let cdt = dt.get_cdt_with_offset();
        DateTimeOffset(cdt.into())
    }

    // pub fn new_Date_Time(d: DateOnly, t: TimeOnly, ts: TimeSpan) -> DateTimeOffset {
    //     // let cdt = dt.get_cdt_with_offset();
    //     // DateTimeOffset(cdt.into())
    // }

    pub fn fromDateTime(dt: DateTime) -> DateTimeOffset {
        let cdt = dt.get_cdt_with_offset();
        DateTimeOffset(cdt.into())
    }

    // pub fn fromNativeDateTime(dt: NaiveDateTime) -> DateTimeOffset {
    //     let cdt = dt.get_cdt_with_offset();
    //     DateTimeOffset(cdt.into())
    // }

    // pub fn fromUnixTimeSeconds(...) -> DateTimeOffset {
    //     let cdt = d.to_cdt_with_offset();
    //     DateTimeOffset(cdt.into())
    // }

    // pub fn fromUnixTimeMilliseconds(...) -> DateTimeOffset {
    //     let cdt = d.to_cdt_with_offset();
    //     DateTimeOffset(cdt.into())
    // }

    pub fn minValue() -> DateTimeOffset {
        let d = NaiveDate::from_ymd_opt(1, 1, 1).unwrap();
        let dt = d.and_hms_opt(0, 0, 0).unwrap();
        let cdt = Utc.from_utc_datetime(&dt);
        DateTimeOffset(cdt.into())
    }

    pub fn maxValue() -> DateTimeOffset {
        let ns = Duration::nanoseconds(nanoseconds_per_tick);
        let d = NaiveDate::from_ymd_opt(10000, 1, 1).unwrap();
        let dt = d.and_hms_opt(0, 0, 0).unwrap() - ns; // one tick before year 10000
        let cdt = Utc.from_utc_datetime(&dt);
        DateTimeOffset(cdt.into())
    }

    pub fn unixEpoch() -> DateTimeOffset {
        let cdt = Utc.timestamp_millis_opt(0).unwrap();
        DateTimeOffset(cdt.into())
    }

    impl DateTimeOffset {
        pub fn add(&self, ts: TimeSpan) -> DateTimeOffset {
            let ns = Duration::nanoseconds(ts.ticks() * nanoseconds_per_tick);
            DateTimeOffset(self.0 + ns)
        }

        pub fn subtract(&self, ts: TimeSpan) -> DateTimeOffset {
            let ns = Duration::nanoseconds(ts.ticks() * nanoseconds_per_tick);
            DateTimeOffset(self.0 - ns)
        }

        pub fn subtract2(&self, other: DateTimeOffset) -> TimeSpan {
            let ns = (self.0 - other.0).num_nanoseconds().unwrap();
            from_ticks(ns / nanoseconds_per_tick)
        }

        pub fn ticks(&self) -> i64 {
            self.subtract2(minValue()).ticks()
        }

        pub fn date(&self) -> DateTime {
            new_ymd(self.0.year(), self.0.month() as i32, self.0.day() as i32)
        }

        pub fn dateTime(&self) -> DateTime {
            new_ndt_withkind(self.0.naive_local(), 1)
        }

        pub fn toLocalTime(&self) -> DateTimeOffset {
            DateTimeOffset(self.0.with_timezone(&Local).into())
        }

        pub fn toUniversalTime(&self) -> DateTimeOffset {
            DateTimeOffset(self.0.with_timezone(&Utc).into())
        }

        pub fn localDateTime(&self) -> DateTimeOffset {
            self.toLocalTime()
        }

        pub fn utcDateTime(&self) -> DateTimeOffset {
            self.toUniversalTime()
        }

        fn toUnixTimeMilliseconds(&self) -> i64 {
            self.0.timestamp_millis()
        }

        fn toUnixTimeSeconds(&self) -> i64 {
            self.0.timestamp()
        }

        pub fn toString(&self, format: string) -> string {
            let fmt = format
                .replace("yyyy", "%Y")
                .replace("MM", "%m")
                .replace("dd", "%d")
                .replace("ss", "%S")
                .replace("fff", "%3f");
            let df = self.0.format(&fmt);
            fromString(df.to_string())
        }

        pub fn year(&self) -> i32 {
            self.0.year()
        }

        pub fn month(&self) -> i32 {
            self.0.month() as i32
        }

        pub fn day(&self) -> i32 {
            self.0.day() as i32
        }

        pub fn hour(&self) -> i32 {
            self.0.hour() as i32
        }

        pub fn minute(&self) -> i32 {
            self.0.minute() as i32
        }

        pub fn second(&self) -> i32 {
            self.0.second() as i32
        }

        pub fn millisecond(&self) -> i32 {
            self.0.timestamp_subsec_millis() as i32
        }

        pub fn microsecond(&self) -> i32 {
            self.0.timestamp_subsec_micros() as i32
        }

        pub fn nanosecond(&self) -> i32 {
            self.0.timestamp_subsec_nanos() as i32
        }

        pub fn timeOfDay(&self) -> TimeSpan {
            let ns = (self.0.time() - NaiveTime::MIN).num_nanoseconds().unwrap();
            from_ticks(ns / nanoseconds_per_tick)
        }

        // pub fn totalOffsetMinutes(&self) -> i32 {
        // }

        // todo implement as DayOfWeek enum https://docs.microsoft.com/en-us/dotnet/api/system.dayofweek?view=net-6.0
        pub fn dayOfWeek(&self) -> i32 {
            match self.0.weekday() {
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
            self.0.day() as i32
        }

        pub fn dayOfYear(&self) -> i32 {
            self.0.ordinal() as i32
        }

        pub fn addDays(&self, days: f64) -> DateTimeOffset {
            self.add(from_days(days))
        }

        pub fn addHours(&self, hours: f64) -> DateTimeOffset {
            self.add(from_hours(hours))
        }

        pub fn addMinutes(&self, minutes: f64) -> DateTimeOffset {
            self.add(from_minutes(minutes))
        }

        pub fn addSeconds(&self, seconds: f64) -> DateTimeOffset {
            self.add(from_seconds(seconds))
        }

        pub fn addMilliseconds(&self, millis: f64) -> DateTimeOffset {
            self.add(from_milliseconds(millis))
        }

        pub fn addMicroseconds(&self, micros: f64) -> DateTimeOffset {
            self.add(from_microseconds(micros))
        }

        pub fn addTicks(&self, ticks: i64) -> DateTimeOffset {
            self.add(from_ticks(ticks))
        }
    }

    impl Add<TimeSpan> for DateTimeOffset {
        type Output = DateTimeOffset;

        fn add(self, rhs: TimeSpan) -> Self::Output {
            DateTimeOffset::add(&self, rhs)
        }
    }

    impl Sub<TimeSpan> for DateTimeOffset {
        type Output = DateTimeOffset;

        fn sub(self, rhs: TimeSpan) -> Self::Output {
            self.subtract(rhs)
        }
    }

    impl Sub<DateTimeOffset> for DateTimeOffset {
        type Output = TimeSpan;

        fn sub(self, rhs: DateTimeOffset) -> Self::Output {
            self.subtract2(rhs)
        }
    }
}
