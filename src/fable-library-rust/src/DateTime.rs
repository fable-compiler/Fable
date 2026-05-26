#[cfg(feature = "datetime")]
pub mod DateTime_ {
    use crate::{
        DateOnly_::DateOnly,
        Format::DateTime::{
            format_dotnet_custom, format_offset_token, format_roundtrip_datetime,
            try_parse_datetime_str,
        },
        Native_::{Hashable, MutCell, String, ToString, compare, getHashCode},
        String_::{fromString, string},
        TimeOnly_::TimeOnly,
        TimeSpan_::{TimeSpan, nanoseconds_per_tick, ticks_per_second},
    };
    use chrono::{
        DateTime as CDateTime, Datelike, Duration, FixedOffset, Local, Months, NaiveDate,
        NaiveDateTime, NaiveTime, Offset, ParseResult, TimeZone, Timelike, Utc, Weekday,
    };
    use core::hash::{Hash, Hasher};
    use core::ops::{Add, Sub};

    #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
    pub enum DateTimeKind {
        Unspecified,
        Utc,
        Local,
    }

    #[derive(Clone, Copy)]
    pub struct DateTime {
        ndt: NaiveDateTime,
        kind: DateTimeKind,
    }

    impl core::fmt::Debug for DateTime {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.toString(string("")))
        }
    }

    impl core::fmt::Display for DateTime {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.toString(string("")))
        }
    }

    impl PartialEq for DateTime {
        fn eq(&self, other: &Self) -> bool {
            self.ticks() == other.ticks()
        }
    }

    impl PartialOrd for DateTime {
        fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
            self.ticks().partial_cmp(&other.ticks())
        }
    }

    impl Eq for DateTime {}

    impl Ord for DateTime {
        fn cmp(&self, other: &Self) -> core::cmp::Ordering {
            self.ticks().cmp(&other.ticks())
        }
    }

    impl Hash for DateTime {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.ticks().hash(state)
        }
    }

    impl Hashable for DateTime {
        #[inline]
        fn getHashCode(&self) -> i32 {
            getHashCode(&self.ticks())
        }
    }

    pub fn compareTo(x: DateTime, y: DateTime) -> i32 {
        compare(&x.ticks(), &y.ticks())
    }

    pub fn equals(x: DateTime, y: DateTime) -> bool {
        x.ticks() == y.ticks()
    }

    pub fn zero() -> DateTime {
        DateTime::minValue()
    }

    pub(crate) fn ticks_to_duration(ticks: i64) -> Duration {
        let seconds = ticks / ticks_per_second;
        let subsecond = ticks % ticks_per_second;
        let d1 = Duration::try_seconds(seconds).unwrap();
        let d2 = Duration::nanoseconds(subsecond * nanoseconds_per_tick);
        d1 + d2
    }

    pub(crate) fn duration_to_ticks(d: Duration) -> i64 {
        let seconds = d.num_seconds();
        let subsecond = d - Duration::try_seconds(seconds).unwrap();
        let ns = subsecond.num_nanoseconds().unwrap();
        seconds * ticks_per_second + ns / nanoseconds_per_tick
    }

    impl DateTime {
        pub fn new(ndt: NaiveDateTime, kind: DateTimeKind) -> DateTime {
            if ndt < Self::minValue().ndt || ndt > Self::maxValue().ndt {
                panic!("Invalid datetime range");
            }
            DateTime { ndt, kind }
        }

        pub fn new_kind(ndt: NaiveDateTime, kind: i32) -> DateTime {
            let dtKind = match kind {
                0 => DateTimeKind::Unspecified,
                1 => DateTimeKind::Utc,
                2 => DateTimeKind::Local,
                _ => panic!(
                    "Unsupported date kind. Only valid values are: 0 - Unspecified, 1 - Utc, 2 -> Local"
                ),
            };
            Self::new(ndt, dtKind)
        }

        pub fn new_empty() -> DateTime {
            Self::minValue()
        }

        pub fn new_ticks(ticks: i64) -> DateTime {
            Self::minValue().add(TimeSpan::fromTicks(ticks))
        }

        pub fn new_ticks_kind(ticks: i64, kind: i32) -> DateTime {
            let d = ticks_to_duration(ticks);
            let ndt = Self::minValue().ndt + d;
            Self::new_kind(ndt, kind)
        }

        pub fn new_date_time(d: DateOnly, t: TimeOnly) -> DateTime {
            let ndt = d.naive_date().and_time(t.naive_time());
            Self::new(ndt, DateTimeKind::Unspecified)
        }

        pub fn new_date_time_kind(d: DateOnly, t: TimeOnly, kind: i32) -> DateTime {
            let ndt = d.naive_date().and_time(t.naive_time());
            Self::new_kind(ndt, kind)
        }

        pub fn new_ymd(y: i32, m: i32, d: i32) -> DateTime {
            let nd = NaiveDate::from_ymd_opt(y, m as u32, d as u32).unwrap();
            let ndt = nd.and_hms_opt(0, 0, 0).unwrap();
            Self::new(ndt, DateTimeKind::Unspecified)
        }

        pub fn new_ymdhms(y: i32, m: i32, d: i32, h: i32, mins: i32, secs: i32) -> DateTime {
            let nd = NaiveDate::from_ymd_opt(y, m as u32, d as u32).unwrap();
            let ndt = nd.and_hms_opt(h as u32, mins as u32, secs as u32).unwrap();
            Self::new(ndt, DateTimeKind::Unspecified)
        }

        pub fn new_ymdhms_kind(
            y: i32,
            m: i32,
            d: i32,
            h: i32,
            mins: i32,
            secs: i32,
            kind: i32,
        ) -> DateTime {
            let dt = Self::new_ymdhms(y, m, d, h, mins, secs);
            Self::new_kind(dt.ndt, kind)
        }

        pub fn new_ymdhms_milli(
            y: i32,
            m: i32,
            d: i32,
            h: i32,
            mins: i32,
            secs: i32,
            millis: i32,
        ) -> DateTime {
            let nd = NaiveDate::from_ymd_opt(y, m as u32, d as u32).unwrap();
            let ndt = nd
                .and_hms_milli_opt(h as u32, mins as u32, secs as u32, millis as u32)
                .unwrap();
            Self::new(ndt, DateTimeKind::Unspecified)
        }

        pub fn new_ymdhms_milli_kind(
            y: i32,
            m: i32,
            d: i32,
            h: i32,
            mins: i32,
            secs: i32,
            millis: i32,
            kind: i32,
        ) -> DateTime {
            let dt = Self::new_ymdhms_milli(y, m, d, h, mins, secs, millis);
            Self::new_kind(dt.ndt, kind)
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
        ) -> DateTime {
            let nd = NaiveDate::from_ymd_opt(y, m as u32, d as u32).unwrap();
            let ndt = nd
                .and_hms_micro_opt(
                    h as u32,
                    mins as u32,
                    secs as u32,
                    (millis * 1000 + micros) as u32,
                )
                .unwrap();
            Self::new(ndt, DateTimeKind::Unspecified)
        }

        pub fn new_ymdhms_micro_kind(
            y: i32,
            m: i32,
            d: i32,
            h: i32,
            mins: i32,
            secs: i32,
            millis: i32,
            micros: i32,
            kind: i32,
        ) -> DateTime {
            let dt = Self::new_ymdhms_micro(y, m, d, h, mins, secs, millis, micros);
            Self::new_kind(dt.ndt, kind)
        }

        pub fn now() -> DateTime {
            DateTime {
                ndt: Local::now().naive_local(),
                kind: DateTimeKind::Local,
            }
        }

        pub fn utcNow() -> DateTime {
            DateTime {
                ndt: Utc::now().naive_utc(),
                kind: DateTimeKind::Utc,
            }
        }

        pub fn minValue() -> DateTime {
            let nd = NaiveDate::from_ymd_opt(1, 1, 1).unwrap();
            let ndt = nd.and_hms_opt(0, 0, 0).unwrap();
            DateTime {
                ndt,
                kind: DateTimeKind::Utc,
            }
        }

        pub fn maxValue() -> DateTime {
            let d = ticks_to_duration(1);
            let nd = NaiveDate::from_ymd_opt(10000, 1, 1).unwrap();
            let ndt = nd.and_hms_opt(0, 0, 0).unwrap() - d; // one tick before year 10000
            DateTime {
                ndt,
                kind: DateTimeKind::Utc,
            }
        }

        pub fn unixEpoch() -> DateTime {
            let ndt = Utc.timestamp_millis_opt(0).unwrap().naive_utc();
            Self::new(ndt, DateTimeKind::Utc)
        }

        pub fn daysInMonth(year: i32, month: i32) -> i32 {
            let (year2, month2) = if month == 12 {
                (year + 1, 1)
            } else {
                (year, month + 1)
            };
            let d1 = NaiveDate::from_ymd_opt(year, month as u32, 1).unwrap();
            let d2 = NaiveDate::from_ymd_opt(year2, month2 as u32, 1).unwrap();
            let days = (d2 - d1).num_days();
            days as i32
        }

        pub fn isLeapYear(year: i32) -> bool {
            Self::daysInMonth(year, 2) == 29
        }

        pub fn today() -> DateTime {
            let cdt = Utc::now();
            Self::new_ymdhms_kind(cdt.year(), cdt.month() as i32, cdt.day() as i32, 0, 0, 0, 1)
        }

        pub fn specifyKind(dt: DateTime, kind: i32) -> DateTime {
            Self::new_ticks_kind(dt.ticks(), kind)
        }

        pub fn add(&self, ts: TimeSpan) -> DateTime {
            let d = ticks_to_duration(ts.ticks());
            Self::new(self.ndt + d, self.kind)
        }

        pub fn subtract(&self, ts: TimeSpan) -> DateTime {
            let d = ticks_to_duration(ts.ticks());
            Self::new(self.ndt - d, self.kind)
        }

        pub fn subtract2(&self, other: DateTime) -> TimeSpan {
            let ticks = duration_to_ticks(self.ndt - other.ndt);
            TimeSpan::fromTicks(ticks)
        }

        pub fn kind(&self) -> i32 {
            match self.kind {
                DateTimeKind::Unspecified => 0,
                DateTimeKind::Utc => 1,
                DateTimeKind::Local => 2,
            }
        }

        pub(crate) fn kind_enum(&self) -> DateTimeKind {
            self.kind
        }

        pub fn ticks(&self) -> i64 {
            duration_to_ticks(self.ndt - Self::minValue().ndt)
        }

        pub fn date(&self) -> DateTime {
            Self::new_ymdhms_kind(
                self.year(),
                self.month() as i32,
                self.day() as i32,
                0,
                0,
                0,
                self.kind(),
            )
        }

        pub fn toLocalTime(&self) -> DateTime {
            let ndt = match self.kind {
                DateTimeKind::Utc => Local.from_utc_datetime(&self.ndt).naive_local(),
                DateTimeKind::Local => self.ndt,
                DateTimeKind::Unspecified => Local.from_utc_datetime(&self.ndt).naive_local(),
            };
            Self::new(ndt, DateTimeKind::Local)
        }

        pub fn toUniversalTime(&self) -> DateTime {
            let ndt = match self.kind {
                DateTimeKind::Utc => self.ndt,
                DateTimeKind::Local => Local.from_local_datetime(&self.ndt).unwrap().naive_utc(),
                DateTimeKind::Unspecified => {
                    Local.from_local_datetime(&self.ndt).unwrap().naive_utc()
                }
            };
            Self::new(ndt, DateTimeKind::Utc)
        }

        pub fn localDateTime(&self) -> DateTime {
            self.toLocalTime()
        }

        pub fn utcDateTime(&self) -> DateTime {
            self.toUniversalTime()
        }

        pub fn year(&self) -> i32 {
            self.ndt.year()
        }

        pub fn month(&self) -> i32 {
            self.ndt.month() as i32
        }

        pub fn day(&self) -> i32 {
            self.ndt.day() as i32
        }

        pub fn hour(&self) -> i32 {
            self.ndt.hour() as i32
        }

        pub fn minute(&self) -> i32 {
            self.ndt.minute() as i32
        }

        pub fn second(&self) -> i32 {
            self.ndt.second() as i32
        }

        pub fn millisecond(&self) -> i32 {
            self.ndt.and_utc().timestamp_subsec_millis() as i32
        }

        pub fn microsecond(&self) -> i32 {
            (self.ndt.and_utc().timestamp_subsec_micros() % 1000) as i32
        }

        pub fn nanosecond(&self) -> i32 {
            (self.ndt.and_utc().timestamp_subsec_nanos() % 1000) as i32
        }

        pub fn timeOfDay(&self) -> TimeSpan {
            let d = self.ndt.time() - NaiveTime::MIN;
            TimeSpan::fromTicks(duration_to_ticks(d))
        }

        pub fn dayNumber(&self) -> i32 {
            self.ndt.num_days_from_ce()
        }

        // todo implement as DayOfWeek enum https://docs.microsoft.com/en-us/dotnet/api/system.dayofweek?view=net-6.0
        pub fn dayOfWeek(&self) -> i32 {
            let weekday = self.ndt.weekday();
            match weekday {
                Weekday::Mon => 1,
                Weekday::Tue => 2,
                Weekday::Wed => 3,
                Weekday::Thu => 4,
                Weekday::Fri => 5,
                Weekday::Sat => 6,
                Weekday::Sun => 0,
            }
        }

        pub fn dayOfYear(&self) -> i32 {
            self.ndt.ordinal() as i32
        }

        pub fn addMonths(&self, months: i32) -> DateTime {
            let ndt = if months < 0 {
                self.ndt
                    .checked_sub_months(Months::new(-months as u32))
                    .unwrap()
            } else {
                self.ndt
                    .checked_add_months(Months::new(months as u32))
                    .unwrap()
            };
            Self::new(ndt, self.kind)
        }

        pub fn addYears(&self, years: i32) -> DateTime {
            self.addMonths(years * 12)
        }

        pub fn addDays(&self, days: f64) -> DateTime {
            self.add(TimeSpan::fromDays(days))
        }

        pub fn addHours(&self, hours: f64) -> DateTime {
            self.add(TimeSpan::fromHours(hours))
        }

        pub fn addMinutes(&self, minutes: f64) -> DateTime {
            self.add(TimeSpan::fromMinutes(minutes))
        }

        pub fn addSeconds(&self, seconds: f64) -> DateTime {
            self.add(TimeSpan::fromSeconds(seconds))
        }

        pub fn addMilliseconds(&self, millis: f64) -> DateTime {
            self.add(TimeSpan::fromMilliseconds(millis))
        }

        pub fn addMicroseconds(&self, micros: f64) -> DateTime {
            self.add(TimeSpan::fromMicroseconds(micros))
        }

        pub fn addTicks(&self, ticks: i64) -> DateTime {
            self.add(TimeSpan::fromTicks(ticks))
        }

        pub fn toString(&self, format: string) -> string {
            let cdt = self.to_cdt_fixed();
            let offset_minutes = cdt.offset().local_minus_utc() / 60;
            let suffix = match self.kind {
                DateTimeKind::Utc => "Z".to_string(),
                DateTimeKind::Local => format_offset_token(offset_minutes, 3),
                DateTimeKind::Unspecified => String::new(),
            };
            let fraction_ticks =
                self.ndt.and_utc().timestamp_subsec_nanos() as i64 / nanoseconds_per_tick;
            let text = match format.as_str() {
                "" => self.ndt.format("%m/%d/%Y %H:%M:%S").to_string(),
                "g" => self.ndt.format("%m/%d/%Y %H:%M").to_string(),
                "G" => self.ndt.format("%m/%d/%Y %H:%M:%S").to_string(),
                "o" | "O" => format_roundtrip_datetime(self.ndt, fraction_ticks, suffix.as_str()),
                _ => {
                    format_dotnet_custom(self.ndt, fraction_ticks, suffix.as_str(), format.as_str())
                }
            };
            fromString(text)
        }

        pub fn toLongDateString(&self) -> string {
            fromString(self.ndt.format("%A, %d %B %Y").to_string())
        }

        pub fn toShortDateString(&self) -> string {
            fromString(self.ndt.format("%m/%d/%Y").to_string())
        }

        pub fn toLongTimeString(&self) -> string {
            fromString(self.ndt.format("%H:%M:%S").to_string())
        }

        pub fn toShortTimeString(&self) -> string {
            fromString(self.ndt.format("%H:%M").to_string())
        }

        fn local_offset_seconds(ndt: NaiveDateTime) -> i32 {
            match Local.from_local_datetime(&ndt) {
                chrono::LocalResult::Single(dt) => dt.offset().local_minus_utc(),
                chrono::LocalResult::Ambiguous(earliest, latest) => earliest
                    .offset()
                    .local_minus_utc()
                    .max(latest.offset().local_minus_utc()),
                chrono::LocalResult::None => {
                    Local.from_utc_datetime(&ndt).offset().local_minus_utc()
                }
            }
        }

        pub fn isDaylightSavingTime(&self) -> bool {
            match self.kind {
                DateTimeKind::Utc => false,
                DateTimeKind::Local | DateTimeKind::Unspecified => {
                    let jan = NaiveDate::from_ymd_opt(self.year(), 1, 1)
                        .unwrap()
                        .and_hms_opt(12, 0, 0)
                        .unwrap();
                    let jul = NaiveDate::from_ymd_opt(self.year(), 7, 1)
                        .unwrap()
                        .and_hms_opt(12, 0, 0)
                        .unwrap();
                    let jan_offset = Self::local_offset_seconds(jan);
                    let jul_offset = Self::local_offset_seconds(jul);
                    jan_offset != jul_offset
                        && Self::local_offset_seconds(self.ndt) == jan_offset.max(jul_offset)
                }
            }
        }

        fn try_parse_str(s: &str) -> ParseResult<DateTime> {
            try_parse_datetime_str(s)
        }

        pub fn tryParse(s: string, res: &MutCell<DateTime>) -> bool {
            match Self::try_parse_str(s.trim()) {
                Ok(dt) => {
                    res.set(dt);
                    true
                }
                Err(e) => false,
            }
        }

        pub fn parse(s: string) -> DateTime {
            match Self::try_parse_str(s.trim()) {
                Ok(dt) => dt,
                Err(e) => panic!("The input string {} was not in a correct format.", s),
            }
        }

        pub(crate) fn to_cdt_fixed(&self) -> CDateTime<FixedOffset> {
            match self.kind {
                DateTimeKind::Utc => Utc.from_utc_datetime(&self.ndt).into(),
                DateTimeKind::Local | DateTimeKind::Unspecified => {
                    match Local.from_local_datetime(&self.ndt) {
                        chrono::LocalResult::Single(dt) => dt.into(),
                        chrono::LocalResult::Ambiguous(earliest, _) => earliest.into(),
                        chrono::LocalResult::None => Local.from_utc_datetime(&self.ndt).into(),
                    }
                }
            }
        }
    }

    impl Add<TimeSpan> for DateTime {
        type Output = DateTime;

        fn add(self, rhs: TimeSpan) -> Self::Output {
            Self::add(&self, rhs)
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
