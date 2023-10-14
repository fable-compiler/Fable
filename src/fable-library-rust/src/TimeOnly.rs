#[cfg(feature = "datetime")]
pub mod TimeOnly_ {
    use crate::{
        DateTime_::{duration_to_ticks, ticks_to_duration, DateTime},
        Native_::{compare, MutCell, ToString},
        String_::{fromString, string},
        TimeSpan_::{nanoseconds_per_tick, ticks_per_day, TimeSpan},
    };
    use chrono::{DateTime as CDateTime, NaiveTime, ParseResult, Timelike};
    use core::ops::Sub;

    #[repr(transparent)]
    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct TimeOnly(NaiveTime);

    impl core::fmt::Display for TimeOnly {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.toString(string("")))
        }
    }

    pub fn compareTo(x: TimeOnly, y: TimeOnly) -> i32 {
        compare(&x, &y)
    }

    pub fn equals(x: TimeOnly, y: TimeOnly) -> bool {
        x == y
    }

    pub fn zero() -> TimeOnly {
        TimeOnly::minValue()
    }

    impl TimeOnly {
        pub(crate) fn naive_time(&self) -> NaiveTime {
            self.0
        }

        pub fn new_ticks(ticks: i64) -> TimeOnly {
            let d = ticks_to_duration(ticks);
            TimeOnly(NaiveTime::MIN + d)
        }

        pub fn new_hm(hours: i32, mins: i32) -> TimeOnly {
            if (hours < 0 || mins < 0) {
                panic!("The parameters describe an unrepresentable TimeOnly.");
            }
            let t = NaiveTime::from_hms_opt(hours as u32, mins as u32, 0).unwrap();
            TimeOnly(t)
        }

        pub fn new_hms(hours: i32, mins: i32, secs: i32) -> TimeOnly {
            if (hours < 0 || mins < 0 || secs < 0) {
                panic!("The parameters describe an unrepresentable TimeOnly.");
            }
            let t = NaiveTime::from_hms_opt(hours as u32, mins as u32, secs as u32).unwrap();
            TimeOnly(t)
        }

        pub fn new_hms_milli(hours: i32, mins: i32, secs: i32, millis: i32) -> TimeOnly {
            if (hours < 0 || mins < 0 || secs < 0 || millis < 0) {
                panic!("The parameters describe an unrepresentable TimeOnly.");
            }
            let t = NaiveTime::from_hms_milli_opt(
                hours as u32,
                mins as u32,
                secs as u32,
                millis as u32,
            )
            .unwrap();
            TimeOnly(t)
        }

        pub fn new_hms_micro(
            hours: i32,
            mins: i32,
            secs: i32,
            millis: i32,
            micros: i32,
        ) -> TimeOnly {
            if (hours < 0 || mins < 0 || secs < 0 || millis < 0 || micros < 0) {
                panic!("The parameters describe an unrepresentable TimeOnly.");
            }
            let t = NaiveTime::from_hms_micro_opt(
                hours as u32,
                mins as u32,
                secs as u32,
                (millis * 100 + micros) as u32,
            )
            .unwrap();
            TimeOnly(t)
        }

        pub fn minValue() -> TimeOnly {
            let t = NaiveTime::MIN;
            TimeOnly(t)
        }

        pub fn maxValue() -> TimeOnly {
            let t = NaiveTime::from_hms_nano_opt(23, 59, 59, 999_999_999).unwrap();
            TimeOnly(t)
        }

        pub fn isBetween(&self, start: TimeOnly, end: TimeOnly) -> bool {
            if start.0 <= end.0 {
                self.0 >= start.0 && self.0 < end.0
            } else {
                self.0 >= start.0 || self.0 < end.0
            }
        }

        pub fn fromDateTime(dt: DateTime) -> TimeOnly {
            let t = dt.to_cdt_fixed().time();
            TimeOnly(t)
        }

        pub fn fromTimeSpan(ts: TimeSpan) -> TimeOnly {
            if ts.ticks() < 0 || ts.ticks() >= ticks_per_day {
                panic!("The TimeSpan describes an unrepresentable TimeOnly.");
            }
            Self::minValue().add(ts)
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
            (self.0.nanosecond() / 1_000_000) as i32
        }

        pub fn microsecond(&self) -> i32 {
            (self.0.nanosecond() / 1000) as i32
        }

        pub fn nanosecond(&self) -> i32 {
            self.0.nanosecond() as i32
        }

        pub fn ticks(&self) -> i64 {
            duration_to_ticks(self.0 - NaiveTime::MIN)
        }

        pub fn toTimeSpan(&self) -> TimeSpan {
            TimeSpan::new_ticks(self.ticks())
        }

        pub fn add(&self, ts: TimeSpan) -> TimeOnly {
            let ticks = (self.ticks() + ts.ticks()) % ticks_per_day;
            let ticks = if ticks < 0 {
                ticks + ticks_per_day
            } else {
                ticks
            };
            Self::new_ticks(ticks)
        }

        pub fn add2(&self, ts: TimeSpan, res: &MutCell<i32>) -> TimeOnly {
            let ticks = self.ticks() + ts.ticks();
            let days = ticks / ticks_per_day;
            let ticks = ticks % ticks_per_day;
            let (days, ticks) = if ticks < 0 {
                (days - 1, ticks + ticks_per_day)
            } else {
                (days, ticks)
            };
            res.set(days as i32);
            Self::new_ticks(ticks)
        }

        pub fn op_Subtraction(t1: TimeOnly, t2: TimeOnly) -> TimeSpan {
            let ticks = t1.ticks() - t2.ticks();
            let ticks = if ticks < 0 {
                ticks + ticks_per_day
            } else {
                ticks
            };
            TimeSpan::from_ticks(ticks)
        }

        pub fn addHours(&self, hours: f64) -> TimeOnly {
            self.add(TimeSpan::from_hours(hours))
        }

        pub fn addMinutes(&self, minutes: f64) -> TimeOnly {
            self.add(TimeSpan::from_minutes(minutes))
        }

        pub fn toString(&self, format: string) -> string {
            let fmt = match format.as_str() {
                "" | "t" => "%H:%M".to_string(),
                "o" | "O" => "%H:%M:%S%.f".to_string(),
                "r" | "R" | "T" => "%H:%M:%S".to_string(),
                //TODO: support more formats, custom formats, etc.
                _ => format
                    .replace("hh", "%H")
                    .replace("mm", "%M")
                    .replace("ss", "%S")
                    .replace("ffffff", "%6f")
                    .replace("fff", "%3f"),
            };
            let df = self.0.format(&fmt);
            fromString(df.to_string())
        }

        fn try_parse_str(s: &str) -> ParseResult<NaiveTime> {
            s.parse::<NaiveTime>()
                .or(NaiveTime::parse_from_str(s, "%m/%d/%Y %H:%M:%S%.f"))
                .or(NaiveTime::parse_from_str(s, "%m/%d/%Y %I:%M:%S %P"))
        }

        pub fn tryParse(s: string, res: &MutCell<TimeOnly>) -> bool {
            match Self::try_parse_str(s.trim()) {
                Ok(nt) => {
                    res.set(TimeOnly(nt));
                    true
                }
                Err(e) => false,
            }
        }

        pub fn parse(s: string) -> TimeOnly {
            match Self::try_parse_str(s.trim()) {
                Ok(nt) => TimeOnly(nt),
                Err(e) => panic!("The input string {} was not in a correct format.", s),
            }
        }
    }

    impl Sub<TimeOnly> for TimeOnly {
        type Output = TimeSpan;

        fn sub(self, rhs: TimeOnly) -> Self::Output {
            Self::op_Subtraction(self, rhs)
        }
    }
}
