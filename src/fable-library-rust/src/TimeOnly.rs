#[cfg(feature = "datetime")]
pub mod TimeOnly_ {
    use crate::{
        DateTime_::{duration_to_ticks, ticks_to_duration, DateTime},
        Native_::{compare, MutCell},
        String_::{fromString, string},
        TimeSpan_::{nanoseconds_per_tick, TimeSpan},
    };
    use chrono::{DateTime as CDateTime, NaiveTime, Timelike};

    #[repr(transparent)]
    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct TimeOnly(NaiveTime);

    impl core::fmt::Display for TimeOnly {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.0.to_string())
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

        pub fn new1(ticks: i64) -> TimeOnly {
            let d = ticks_to_duration(ticks);
            TimeOnly(NaiveTime::MIN + d)
        }

        pub fn new2(hours: i32, mins: i32) -> TimeOnly {
            let t = NaiveTime::from_hms_opt(hours as u32, mins as u32, 0).unwrap();
            TimeOnly(t)
        }

        pub fn new3(hours: i32, mins: i32, secs: i32) -> TimeOnly {
            let t = NaiveTime::from_hms_opt(hours as u32, mins as u32, secs as u32).unwrap();
            TimeOnly(t)
        }

        pub fn new4(hours: i32, mins: i32, secs: i32, millis: i32) -> TimeOnly {
            let t = NaiveTime::from_hms_milli_opt(
                hours as u32,
                mins as u32,
                secs as u32,
                millis as u32,
            )
            .unwrap();
            TimeOnly(t)
        }

        pub fn new5(hours: i32, mins: i32, secs: i32, millis: i32, micros: i32) -> TimeOnly {
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

        pub fn fromDateTime(dt: DateTime) -> TimeOnly {
            let t = dt.to_cdt_with_offset().time();
            TimeOnly(t)
        }

        pub fn fromTimeSpan(ts: TimeSpan) -> TimeOnly {
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
            TimeSpan::from_ticks(self.ticks())
        }

        pub fn add(&self, ts: TimeSpan) -> TimeOnly {
            let d = ticks_to_duration(ts.ticks());
            TimeOnly(self.0 + d)
        }

        pub fn addHours(&self, hours: f64) -> TimeOnly {
            self.add(TimeSpan::from_hours(hours))
        }

        pub fn addMinutes(&self, minutes: f64) -> TimeOnly {
            self.add(TimeSpan::from_minutes(minutes))
        }

        pub fn toString(&self, format: string) -> string {
            let fmt = format
                .replace("hh", "%H")
                .replace("mm", "%M")
                .replace("ss", "%S")
                .replace("ffffff", "%6f")
                .replace("fff", "%3f");
            let df = self.0.format(&fmt);
            fromString(df.to_string())
        }

        pub fn tryParse(s: string, res: &MutCell<TimeOnly>) -> bool {
            match s.parse::<NaiveTime>() {
                Ok(nt) => {
                    res.set(TimeOnly(nt));
                    true
                }
                Err(e) => false,
            }
        }

        pub fn parse(s: string) -> TimeOnly {
            match s.parse::<NaiveTime>() {
                Ok(nt) => TimeOnly(nt),
                Err(e) => panic!("Input string was not in a correct format."),
            }
        }
    }
}
