#[cfg(feature = "datetime")]
pub mod DateTimeOffset_ {
    use crate::{
        DateOnly_::DateOnly,
        DateTime_::{duration_to_ticks, ticks_to_duration, DateTime, DateTimeKind},
        Native_::{compare, MutCell},
        String_::{fromString, string},
        TimeOnly_::TimeOnly,
        TimeSpan_::{nanoseconds_per_tick, TimeSpan},
    };
    use chrono::{
        DateTime as CDateTime, Datelike, FixedOffset, Local, NaiveDate, NaiveDateTime,
        NaiveTime, TimeZone, Timelike, Utc,
    };
    use core::ops::{Add, Sub};

    #[repr(transparent)]
    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct DateTimeOffset(CDateTime<FixedOffset>);

    impl core::fmt::Display for DateTimeOffset {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.0.to_string())
        }
    }

    pub fn compareTo(x: DateTimeOffset, y: DateTimeOffset) -> i32 {
        compare(&x, &y)
    }

    pub fn equals(x: DateTimeOffset, y: DateTimeOffset) -> bool {
        x == y
    }

    pub fn zero() -> DateTimeOffset {
        DateTimeOffset::minValue()
    }

    impl DateTimeOffset {
        pub fn new(ndt: NaiveDateTime, offset: TimeSpan) -> DateTimeOffset {
            // TODO: offset validation (in whole minutes, pos or neg, up to 24h)
            let ofs = FixedOffset::west_opt(offset.total_seconds() as i32).unwrap();
            let cdt = ofs.from_utc_datetime(&ndt);
            DateTimeOffset(cdt.into())
        }

        pub fn new_empty() -> DateTimeOffset {
            Self::minValue()
        }

        pub fn new_ticks(ticks: i64, offset: TimeSpan) -> DateTimeOffset {
            let ts = TimeSpan::from_ticks(ticks);
            let ndt = Self::minValue().add(ts).0.naive_utc();
            DateTimeOffset::new(ndt, offset)
        }

        pub fn new_datetime(dt: DateTime) -> DateTimeOffset {
            let cdt = dt.get_cdt_with_offset();
            DateTimeOffset(cdt.into())
        }

        pub fn new_datetime2(dt: DateTime, offset: TimeSpan) -> DateTimeOffset {
            let cdt = dt.get_cdt_with_offset();
            DateTimeOffset(cdt.into())
        }

        pub fn new_date_time(d: DateOnly, t: TimeOnly, offset: TimeSpan) -> DateTimeOffset {
            let ndt = d.naive_date().and_time(t.naive_time());
            DateTimeOffset::new(ndt, offset)
        }

        pub fn new_ymdhms(
            y: i32,
            m: i32,
            d: i32,
            h: i32,
            mins: i32,
            secs: i32,
            offset: TimeSpan,
        ) -> DateTimeOffset {
            let nd = NaiveDate::from_ymd_opt(y, m as u32, d as u32).unwrap();
            let ndt = nd.and_hms_opt(h as u32, mins as u32, secs as u32).unwrap();
            DateTimeOffset::new(ndt, offset)
        }

        pub fn new_ymdhms_milli(
            y: i32,
            m: i32,
            d: i32,
            h: i32,
            mins: i32,
            secs: i32,
            millis: i32,
            offset: TimeSpan,
        ) -> DateTimeOffset {
            let nd = NaiveDate::from_ymd_opt(y, m as u32, d as u32).unwrap();
            let ndt = nd
                .and_hms_milli_opt(h as u32, mins as u32, secs as u32, millis as u32)
                .unwrap();
            DateTimeOffset::new(ndt, offset)
        }

        pub fn new_ymdhms_micro(
            y: i32,
            m: i32,
            d: i32,
            h: i32,
            mins: i32,
            secs: i32,
            millis: i32,
            micros: i32,
            offset: TimeSpan,
        ) -> DateTimeOffset {
            let nd = NaiveDate::from_ymd_opt(y, m as u32, d as u32).unwrap();
            let ndt = nd
                .and_hms_micro_opt(
                    h as u32,
                    mins as u32,
                    secs as u32,
                    (millis * 1000 + micros) as u32,
                )
                .unwrap();
            DateTimeOffset::new(ndt, offset)
        }

        pub fn fromUnixTimeSeconds(seconds: i64) -> DateTimeOffset {
            let cdt = Utc.timestamp_opt(seconds, 0).unwrap();
            DateTimeOffset(cdt.into())
        }

        pub fn fromUnixTimeMilliseconds(millis: i64) -> DateTimeOffset {
            let cdt = Utc.timestamp_millis_opt(millis).unwrap();
            DateTimeOffset(cdt.into())
        }

        pub fn now() -> DateTimeOffset {
            DateTimeOffset(Local::now().into())
        }

        pub fn utcNow() -> DateTimeOffset {
            DateTimeOffset(Utc::now().into())
        }

        pub fn minValue() -> DateTimeOffset {
            let nd = NaiveDate::from_ymd_opt(1, 1, 1).unwrap();
            let ndt = nd.and_hms_opt(0, 0, 0).unwrap();
            let cdt = Utc.from_utc_datetime(&ndt);
            DateTimeOffset(cdt.into())
        }

        pub fn maxValue() -> DateTimeOffset {
            let d = ticks_to_duration(1);
            let nd = NaiveDate::from_ymd_opt(10000, 1, 1).unwrap();
            let ndt = nd.and_hms_opt(0, 0, 0).unwrap() - d; // one tick before year 10000
            let cdt = Utc.from_utc_datetime(&ndt);
            DateTimeOffset(cdt.into())
        }

        pub fn unixEpoch() -> DateTimeOffset {
            let cdt = Utc.timestamp_millis_opt(0).unwrap();
            DateTimeOffset(cdt.into())
        }

        pub fn add(&self, ts: TimeSpan) -> DateTimeOffset {
            let d = ticks_to_duration(ts.ticks());
            DateTimeOffset(self.0 + d)
        }

        pub fn subtract(&self, ts: TimeSpan) -> DateTimeOffset {
            let d = ticks_to_duration(ts.ticks());
            DateTimeOffset(self.0 - d)
        }

        pub fn subtract2(&self, other: DateTimeOffset) -> TimeSpan {
            TimeSpan::from_ticks(duration_to_ticks((self.0 - other.0)))
        }

        pub fn offset(&self) -> TimeSpan {
            let seconds = self.0.offset().local_minus_utc();
            TimeSpan::from_seconds(seconds as f64)
        }

        pub fn ticks(&self) -> i64 {
            duration_to_ticks(self.0 - Self::minValue().0)
        }

        pub fn date(&self) -> DateTime {
            DateTime::new_ymd(self.0.year(), self.0.month() as i32, self.0.day() as i32)
        }

        pub fn dateTime(&self) -> DateTime {
            DateTime::new(self.0.naive_utc(), DateTimeKind::Unspecified)
        }

        pub fn toLocalTime(&self) -> DateTimeOffset {
            DateTimeOffset(self.0.with_timezone(&Local).into())
        }

        pub fn toUniversalTime(&self) -> DateTimeOffset {
            DateTimeOffset(self.0.with_timezone(&Utc).into())
        }

        pub fn localDateTime(&self) -> DateTime {
            DateTime::new(self.0.naive_local(), DateTimeKind::Local)
        }

        pub fn utcDateTime(&self) -> DateTime {
            DateTime::new(self.0.naive_utc(), DateTimeKind::Utc)
        }

        fn toUnixTimeMilliseconds(&self) -> i64 {
            self.0.timestamp_millis()
        }

        fn toUnixTimeSeconds(&self) -> i64 {
            self.0.timestamp()
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
            let d = self.0.time() - NaiveTime::MIN;
            TimeSpan::from_ticks(duration_to_ticks(d))
        }

        // pub fn totalOffsetMinutes(&self) -> i32 {
        // }

        pub fn dayNumber(&self) -> i32 {
            self.0.num_days_from_ce()
        }

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

        pub fn dayOfYear(&self) -> i32 {
            self.0.ordinal() as i32
        }

        pub fn addDays(&self, days: f64) -> DateTimeOffset {
            self.add(TimeSpan::from_days(days))
        }

        pub fn addHours(&self, hours: f64) -> DateTimeOffset {
            self.add(TimeSpan::from_hours(hours))
        }

        pub fn addMinutes(&self, minutes: f64) -> DateTimeOffset {
            self.add(TimeSpan::from_minutes(minutes))
        }

        pub fn addSeconds(&self, seconds: f64) -> DateTimeOffset {
            self.add(TimeSpan::from_seconds(seconds))
        }

        pub fn addMilliseconds(&self, millis: f64) -> DateTimeOffset {
            self.add(TimeSpan::from_milliseconds(millis))
        }

        pub fn addMicroseconds(&self, micros: f64) -> DateTimeOffset {
            self.add(TimeSpan::from_microseconds(micros))
        }

        pub fn addTicks(&self, ticks: i64) -> DateTimeOffset {
            self.add(TimeSpan::from_ticks(ticks))
        }

        pub fn toString(&self, format: string) -> string {
            let fmt = format
                .replace("yyyy", "%Y")
                .replace("MM", "%m")
                .replace("dd", "%d")
                .replace("hh", "%H")
                .replace("mm", "%M")
                .replace("ss", "%S")
                .replace("ffffff", "%6f")
                .replace("fff", "%3f");
            let df = self.0.format(&fmt);
            fromString(df.to_string())
        }

        pub fn tryParse(s: string, res: &MutCell<DateTimeOffset>) -> bool {
            match CDateTime::parse_from_rfc3339(s.trim())
                .or(CDateTime::parse_from_rfc2822(s.trim()))
            {
                Ok(dt) => {
                    res.set(DateTimeOffset(dt.into()));
                    true
                }
                Err(e) => false,
            }
        }

        pub fn parse(s: string) -> DateTimeOffset {
            match CDateTime::parse_from_rfc3339(s.trim())
                .or(CDateTime::parse_from_rfc2822(s.trim()))
            {
                Ok(dt) => DateTimeOffset(dt.into()),
                Err(e) => panic!("Input string was not in a correct format."),
            }
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
