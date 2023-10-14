#[cfg(feature = "datetime")]
pub mod DateTimeOffset_ {
    use crate::{
        DateOnly_::DateOnly,
        DateTime_::{duration_to_ticks, ticks_to_duration, DateTime, DateTimeKind},
        Native_::{compare, MutCell, ToString},
        String_::{fromString, string},
        TimeOnly_::TimeOnly,
        TimeSpan_::{
            nanoseconds_per_tick, ticks_per_hour, ticks_per_minute, ticks_per_second, TimeSpan,
        },
    };
    use chrono::{
        DateTime as CDateTime, Datelike, FixedOffset, Local, Months, NaiveDate, NaiveDateTime,
        NaiveTime, ParseResult, TimeZone, Timelike, Utc,
    };
    use core::ops::{Add, Sub};

    #[repr(transparent)]
    #[derive(Clone, Copy, Debug)]
    pub struct DateTimeOffset(CDateTime<FixedOffset>);

    impl core::fmt::Display for DateTimeOffset {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.toString(string("")))
        }
    }

    impl PartialEq for DateTimeOffset {
        fn eq(&self, other: &Self) -> bool {
            self.utcDateTime() == other.utcDateTime()
        }
    }

    impl PartialOrd for DateTimeOffset {
        fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
            self.utcDateTime().partial_cmp(&other.utcDateTime())
        }
    }

    pub fn compareTo(x: DateTimeOffset, y: DateTimeOffset) -> i32 {
        compare(&x.utcDateTime(), &y.utcDateTime())
    }

    pub fn equals(x: DateTimeOffset, y: DateTimeOffset) -> bool {
        x.utcDateTime() == y.utcDateTime()
    }

    pub fn zero() -> DateTimeOffset {
        DateTimeOffset::minValue()
    }

    impl DateTimeOffset {
        fn validate(ndt: NaiveDateTime, offset: TimeSpan) {
            let min_ndt = Self::minValue().0.naive_utc();
            let max_ndt = Self::maxValue().0.naive_utc();
            if offset.ticks() % ticks_per_minute != 0 {
                panic!("Offset must be specified in whole minutes.");
            }
            if offset.ticks().abs() > 14 * ticks_per_hour {
                panic!("Offset must be within plus or minus 14 hours.");
            }
            if ndt < min_ndt || ndt > max_ndt {
                panic!("Invalid datetime range.");
            }
        }

        pub fn new_utc(ndt: NaiveDateTime, offset: TimeSpan) -> DateTimeOffset {
            Self::validate(ndt, offset);
            let ofs = FixedOffset::east_opt(offset.total_seconds() as i32).unwrap();
            let cdt = ofs.from_utc_datetime(&ndt);
            DateTimeOffset(cdt.into())
        }

        pub fn new_local(ndt: NaiveDateTime, offset: TimeSpan) -> DateTimeOffset {
            Self::validate(ndt, offset);
            let ofs = FixedOffset::east_opt(offset.total_seconds() as i32).unwrap();
            let cdt = ofs.from_local_datetime(&ndt).unwrap();
            DateTimeOffset(cdt.into())
        }

        pub fn toOffset(&self, offset: TimeSpan) -> DateTimeOffset {
            Self::new_utc(self.0.naive_utc(), offset)
        }

        pub fn new_empty() -> DateTimeOffset {
            Self::minValue()
        }

        pub fn new_ticks(ticks: i64, offset: TimeSpan) -> DateTimeOffset {
            let ts = TimeSpan::from_ticks(ticks);
            let ndt = Self::minValue().add(ts).0.naive_local();
            Self::new_local(ndt, offset)
        }

        pub fn new_datetime(dt: DateTime) -> DateTimeOffset {
            let cdt = dt.to_cdt_fixed();
            DateTimeOffset(cdt.into())
        }

        pub fn new_datetime2(dt: DateTime, offset: TimeSpan) -> DateTimeOffset {
            let cdt = dt.to_cdt_fixed();
            let offsetSeconds = offset.ticks() / ticks_per_second;
            let dtOffsetSeconds = cdt.offset().local_minus_utc() as i64;
            if dt.kind_enum() == DateTimeKind::Utc && offsetSeconds != 0 {
                panic!("The UTC Offset for Utc DateTime instances must be 0.");
            }
            if dt.kind_enum() == DateTimeKind::Local && offsetSeconds != dtOffsetSeconds {
                panic!("The UTC Offset of the local dateTime parameter does not match the offset argument.");
            }
            Self::new_local(cdt.naive_local(), offset)
        }

        pub fn new_date_time(d: DateOnly, t: TimeOnly, offset: TimeSpan) -> DateTimeOffset {
            let ndt = d.naive_date().and_time(t.naive_time());
            Self::new_local(ndt, offset)
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
            Self::new_local(ndt, offset)
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
            Self::new_local(ndt, offset)
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
            Self::new_local(ndt, offset)
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
            let now = Local::now();
            let localTz = now.offset();
            DateTimeOffset(now.with_timezone(localTz).into())
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

        pub fn equalsExact(&self, other: DateTimeOffset) -> bool {
            self.utcDateTime() == other.utcDateTime() && self.0.offset() == other.0.offset()
        }

        pub fn offset(&self) -> TimeSpan {
            let seconds = self.0.offset().local_minus_utc();
            TimeSpan::from_seconds(seconds as f64)
        }

        pub fn ticks(&self) -> i64 {
            duration_to_ticks(self.0.naive_local() - Self::minValue().0.naive_utc())
        }

        pub fn utcTicks(&self) -> i64 {
            duration_to_ticks(self.0.naive_utc() - Self::minValue().0.naive_utc())
        }

        pub fn date(&self) -> DateTime {
            DateTime::new_ymd(self.0.year(), self.0.month() as i32, self.0.day() as i32)
        }

        pub fn dateTime(&self) -> DateTime {
            DateTime::new(self.0.naive_local(), DateTimeKind::Unspecified)
        }

        pub fn toLocalTime(&self) -> DateTimeOffset {
            let now = Local::now();
            let localTz = now.offset();
            DateTimeOffset(self.0.with_timezone(localTz).into())
        }

        pub fn toUniversalTime(&self) -> DateTimeOffset {
            DateTimeOffset(self.0.with_timezone(&Utc).into())
        }

        pub fn localDateTime(&self) -> DateTime {
            let ndt = Local.from_utc_datetime(&self.0.naive_utc()).naive_local();
            DateTime::new(ndt, DateTimeKind::Local)
        }

        pub fn utcDateTime(&self) -> DateTime {
            let ndt = self.0.naive_utc();
            DateTime::new(ndt, DateTimeKind::Utc)
        }

        pub fn toUnixTimeMilliseconds(&self) -> i64 {
            self.0.timestamp_millis()
        }

        pub fn toUnixTimeSeconds(&self) -> i64 {
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

        pub fn addMonths(&self, months: i32) -> DateTimeOffset {
            let cdt = if months < 0 {
                self.0
                    .checked_sub_months(Months::new(-months as u32))
                    .unwrap()
            } else {
                self.0
                    .checked_add_months(Months::new(months as u32))
                    .unwrap()
            };
            DateTimeOffset(cdt)
        }

        pub fn addYears(&self, years: i32) -> DateTimeOffset {
            self.addMonths(years * 12)
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
            let fmt = match format.as_str() {
                "" => "%m/%d/%Y %H:%M:%S %:z".to_string(),
                "g" => "%m/%d/%Y %H:%M".to_string(),
                "G" => "%m/%d/%Y %H:%M:%S".to_string(),
                "o" | "O" => "%Y-%m-%dT%H:%M:%S%.f%:z".to_string(),
                //TODO: support more formats, custom formats, etc.
                _ => format
                    .replace("yyyy", "%Y")
                    .replace("MM", "%m")
                    .replace("dd", "%d")
                    .replace("hh", "%H")
                    .replace("mm", "%M")
                    .replace("ss", "%S")
                    .replace("ffffff", "%6f")
                    .replace("fff", "%3f"),
            };
            let df = self.0.format(&fmt);
            fromString(df.to_string())
        }

        fn local_time_from_str(s: &str, fmt: &str) -> ParseResult<CDateTime<FixedOffset>> {
            let now = Local::now();
            let localTz = now.offset();
            let ndt = NaiveDateTime::parse_from_str(s, fmt)?;
            let loc = localTz.from_local_datetime(&ndt).unwrap();
            Ok(loc)
        }

        pub(crate) fn try_parse_str(s: &str) -> ParseResult<CDateTime<FixedOffset>> {
            s.parse::<CDateTime<FixedOffset>>()
                .or(CDateTime::parse_from_str(s, "%m/%d/%Y %H:%M:%S%.f %#z"))
                .or(Self::local_time_from_str(s, "%m/%d/%Y %H:%M:%S%.f"))
                .or(Self::local_time_from_str(s, "%m/%d/%Y %I:%M:%S %P"))
        }

        pub fn tryParse(s: string, res: &MutCell<DateTimeOffset>) -> bool {
            match Self::try_parse_str(s.trim()) {
                Ok(dt) => {
                    res.set(DateTimeOffset(dt.into()));
                    true
                }
                Err(e) => false,
            }
        }

        pub fn parse(s: string) -> DateTimeOffset {
            match Self::try_parse_str(s.trim()) {
                Ok(dt) => DateTimeOffset(dt.into()),
                Err(e) => panic!("The input string {} was not in a correct format.", s),
            }
        }
    }

    impl Add<TimeSpan> for DateTimeOffset {
        type Output = DateTimeOffset;

        fn add(self, rhs: TimeSpan) -> Self::Output {
            Self::add(&self, rhs)
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
