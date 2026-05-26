pub mod TimeSpan_ {
    use crate::{
        Format::TimeSpan::{format_time_span, try_parse_time_span_str},
        Native_::{Hashable, MutCell, ToString, compare, getHashCode},
        String_::{fromString, string},
    };
    use core::ops::{Add, Div, Mul, Sub};

    #[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[repr(transparent)]
    pub struct TimeSpan {
        ticks: i64,
    }

    pub const nanoseconds_per_tick: i64 = 100;
    pub const ticks_per_microsecond: i64 = 10;
    pub const ticks_per_millisecond: i64 = 10_000;
    pub const ticks_per_second: i64 = 10_000_000;
    pub const ticks_per_minute: i64 = ticks_per_second * 60;
    pub const ticks_per_hour: i64 = ticks_per_minute * 60;
    pub const ticks_per_day: i64 = ticks_per_hour * 24;

    pub const zero: TimeSpan = TimeSpan { ticks: 0 };
    pub const min_value: TimeSpan = TimeSpan { ticks: i64::MIN };
    pub const max_value: TimeSpan = TimeSpan { ticks: i64::MAX };

    impl core::fmt::Debug for TimeSpan {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.toString(string("")))
        }
    }

    impl core::fmt::Display for TimeSpan {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.toString(string("")))
        }
    }

    impl Hashable for TimeSpan {
        #[inline]
        fn getHashCode(&self) -> i32 {
            getHashCode(&self.ticks())
        }
    }

    pub fn compareTo(x: TimeSpan, y: TimeSpan) -> i32 {
        compare(&x, &y)
    }

    pub fn equals(x: TimeSpan, y: TimeSpan) -> bool {
        x == y
    }

    impl TimeSpan {
        pub fn new_ticks(ticks: i64) -> TimeSpan {
            TimeSpan { ticks: ticks }
        }

        pub fn new_hms(h: i32, m: i32, s: i32) -> TimeSpan {
            Self::fromDays6(0, h, m as i64, s as i64, 0, 0)
        }

        pub fn new_dhms(d: i32, h: i32, m: i32, s: i32) -> TimeSpan {
            Self::fromDays6(d, h, m as i64, s as i64, 0, 0)
        }

        pub fn new_dhms_milli(d: i32, h: i32, m: i32, s: i32, ms: i32) -> TimeSpan {
            Self::fromDays6(d, h, m as i64, s as i64, ms as i64, 0)
        }

        pub fn new_dhms_micro(d: i32, h: i32, m: i32, s: i32, ms: i32, mi: i32) -> TimeSpan {
            Self::fromDays6(d, h, m as i64, s as i64, ms as i64, mi as i64)
        }

        pub fn fromTicks(ticks: i64) -> TimeSpan {
            Self::new_ticks(ticks)
        }

        pub fn fromDays(days: f64) -> TimeSpan {
            Self::fromHours(days * 24.)
        }

        pub fn fromDays1(d: i32) -> TimeSpan {
            Self::fromDays6(d, 0, 0, 0, 0, 0)
        }

        pub fn fromDays2(d: i32, h: i32) -> TimeSpan {
            Self::fromDays6(d, h, 0, 0, 0, 0)
        }

        pub fn fromDays3(d: i32, h: i32, m: i64) -> TimeSpan {
            Self::fromDays6(d, h, m, 0, 0, 0)
        }

        pub fn fromDays4(d: i32, h: i32, m: i64, s: i64) -> TimeSpan {
            Self::fromDays6(d, h, m, s, 0, 0)
        }

        pub fn fromDays5(d: i32, h: i32, m: i64, s: i64, ms: i64) -> TimeSpan {
            Self::fromDays6(d, h, m, s, ms, 0)
        }

        pub fn fromDays6(d: i32, h: i32, m: i64, s: i64, ms: i64, mi: i64) -> TimeSpan {
            let hours = (d * 24 + h) as i64;
            let hoursTicks = hours * ticks_per_hour;
            let minsTicks = m * ticks_per_minute;
            let secTicks = s * ticks_per_second;
            let msTicks = ms * ticks_per_millisecond;
            let miTicks = mi * ticks_per_microsecond;
            Self::new_ticks(hoursTicks + minsTicks + secTicks + msTicks + miTicks)
        }

        pub fn fromHours(hours: f64) -> TimeSpan {
            Self::fromMinutes(hours * 60.)
        }

        pub fn fromHours1(h: i32) -> TimeSpan {
            Self::fromDays6(0, h, 0, 0, 0, 0)
        }

        pub fn fromHours2(h: i32, m: i64) -> TimeSpan {
            Self::fromDays6(0, h, m, 0, 0, 0)
        }

        pub fn fromHours3(h: i32, m: i64, s: i64) -> TimeSpan {
            Self::fromDays6(0, h, m, s, 0, 0)
        }

        pub fn fromHours4(h: i32, m: i64, s: i64, ms: i64) -> TimeSpan {
            Self::fromDays6(0, h, m, s, ms, 0)
        }

        pub fn fromHours5(h: i32, m: i64, s: i64, ms: i64, mi: i64) -> TimeSpan {
            Self::fromDays6(0, h, m, s, ms, mi)
        }

        pub fn fromMinutes(minutes: f64) -> TimeSpan {
            Self::fromSeconds(minutes * 60.)
        }

        pub fn fromMinutes1(m: i64) -> TimeSpan {
            Self::fromDays6(0, 0, m, 0, 0, 0)
        }

        pub fn fromMinutes2(m: i64, s: i64) -> TimeSpan {
            Self::fromDays6(0, 0, m, s, 0, 0)
        }

        pub fn fromMinutes3(m: i64, s: i64, ms: i64) -> TimeSpan {
            Self::fromDays6(0, 0, m, s, ms, 0)
        }

        pub fn fromMinutes4(m: i64, s: i64, ms: i64, mi: i64) -> TimeSpan {
            Self::fromDays6(0, 0, m, s, ms, mi)
        }

        pub fn fromSeconds(seconds: f64) -> TimeSpan {
            Self::new_ticks((seconds * (ticks_per_second as f64)) as i64)
        }

        pub fn fromSeconds1(s: i64) -> TimeSpan {
            Self::fromDays6(0, 0, 0, s, 0, 0)
        }

        pub fn fromSeconds2(s: i64, ms: i64) -> TimeSpan {
            Self::fromDays6(0, 0, 0, s, ms, 0)
        }

        pub fn fromSeconds3(s: i64, ms: i64, mi: i64) -> TimeSpan {
            Self::fromDays6(0, 0, 0, s, ms, mi)
        }

        pub fn fromMilliseconds(millis: f64) -> TimeSpan {
            Self::new_ticks((millis * (ticks_per_millisecond as f64)) as i64)
        }

        pub fn fromMilliseconds1(ms: i64) -> TimeSpan {
            Self::fromDays6(0, 0, 0, 0, ms, 0)
        }

        pub fn fromMilliseconds2(ms: i64, mi: i64) -> TimeSpan {
            Self::fromDays6(0, 0, 0, 0, ms, mi)
        }

        pub fn fromMicroseconds(micros: f64) -> TimeSpan {
            Self::new_ticks((micros * (ticks_per_microsecond as f64)) as i64)
        }

        pub fn fromMicroseconds1(mi: i64) -> TimeSpan {
            Self::fromDays6(0, 0, 0, 0, 0, mi)
        }

        pub fn totalDays(&self) -> f64 {
            self.ticks as f64 / ticks_per_day as f64
        }

        pub fn totalHours(&self) -> f64 {
            self.ticks as f64 / ticks_per_hour as f64
        }

        pub fn totalMinutes(&self) -> f64 {
            self.ticks as f64 / ticks_per_minute as f64
        }

        pub fn totalSeconds(&self) -> f64 {
            self.ticks as f64 / ticks_per_second as f64
        }

        pub fn totalMilliseconds(&self) -> f64 {
            self.ticks as f64 / ticks_per_millisecond as f64
        }

        pub fn totalMicroseconds(&self) -> f64 {
            self.ticks as f64 / ticks_per_microsecond as f64
        }

        pub fn totalNanoseconds(&self) -> f64 {
            (self.ticks * nanoseconds_per_tick) as f64
        }

        pub fn ticks(&self) -> i64 {
            self.ticks
        }

        pub fn duration(&self) -> TimeSpan {
            Self::new_ticks(self.ticks.abs())
        }

        pub fn days(&self) -> i32 {
            self.totalDays() as i32
        }

        pub fn hours(&self) -> i32 {
            (self.totalHours() - self.totalDays().trunc() * 24.0) as i32
        }

        pub fn minutes(&self) -> i32 {
            (self.totalMinutes() - self.totalHours().trunc() * 60.0) as i32
        }

        pub fn seconds(&self) -> i32 {
            (self.totalSeconds() - self.totalMinutes().trunc() * 60.0) as i32
        }

        pub fn milliseconds(&self) -> i32 {
            (self.totalMilliseconds() - self.totalSeconds().trunc() * 1000.0) as i32
        }

        pub fn microseconds(&self) -> i32 {
            (self.totalMicroseconds() - self.totalMilliseconds().trunc() * 1000.0) as i32
        }

        pub fn nanoseconds(&self) -> i32 {
            (self.totalNanoseconds() - self.totalMicroseconds().trunc() * 1000.0) as i32
        }

        pub fn negate(&self) -> TimeSpan {
            Self::new_ticks(-self.ticks)
        }

        pub fn toString(&self, format: string) -> string {
            fromString(format_time_span(self, format.as_str()))
        }

        fn try_parse_str(s: &str) -> Result<TimeSpan, ()> {
            try_parse_time_span_str(s)
        }

        pub fn tryParse(s: string, res: &MutCell<TimeSpan>) -> bool {
            match Self::try_parse_str(s.trim()) {
                Ok(ts) => {
                    res.set(ts);
                    true
                }
                Err(e) => false,
            }
        }

        pub fn parse(s: string) -> TimeSpan {
            match Self::try_parse_str(s.trim()) {
                Ok(ts) => ts,
                Err(e) => panic!("String '{}' was not recognized as a valid TimeSpan.", s),
            }
        }
    }

    impl Add<TimeSpan> for TimeSpan {
        type Output = TimeSpan;

        fn add(self, rhs: TimeSpan) -> Self::Output {
            TimeSpan::new_ticks(self.ticks + rhs.ticks)
        }
    }

    impl Sub<TimeSpan> for TimeSpan {
        type Output = TimeSpan;

        fn sub(self, rhs: TimeSpan) -> Self::Output {
            TimeSpan::new_ticks(self.ticks - rhs.ticks)
        }
    }

    impl Mul<TimeSpan> for TimeSpan {
        type Output = TimeSpan;

        fn mul(self, rhs: TimeSpan) -> Self::Output {
            TimeSpan::new_ticks(self.ticks * rhs.ticks)
        }
    }

    impl Div<TimeSpan> for TimeSpan {
        type Output = f64;

        fn div(self, rhs: TimeSpan) -> Self::Output {
            (self.ticks as f64) / (rhs.ticks as f64)
        }
    }

    impl Mul<f64> for TimeSpan {
        type Output = TimeSpan;

        fn mul(self, rhs: f64) -> Self::Output {
            TimeSpan::new_ticks(((self.ticks as f64) * rhs) as i64)
        }
    }

    impl Div<f64> for TimeSpan {
        type Output = TimeSpan;

        fn div(self, rhs: f64) -> Self::Output {
            TimeSpan::new_ticks(((self.ticks as f64) / rhs) as i64)
        }
    }
}
