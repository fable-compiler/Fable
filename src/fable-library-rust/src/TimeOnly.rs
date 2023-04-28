#[cfg(feature = "datetime")]
pub mod TimeOnly_ {
    use crate::{
        DateTime_::DateTime,
        String_::{fromString, string},
        TimeSpan_::{from_hours, from_minutes, from_ticks, nanoseconds_per_tick, TimeSpan},
    };
    use chrono::{DateTime as CDateTime, Duration, NaiveTime, Timelike};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct TimeOnly(NaiveTime);

    impl core::fmt::Display for TimeOnly {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.0.to_string())
        }
    }

    pub fn new_ticks(ticks: i64) -> TimeOnly {
        let ns = Duration::nanoseconds(ticks * nanoseconds_per_tick);
        TimeOnly(NaiveTime::MIN + ns)
    }

    pub fn new_hms_milli(h: i32, min: i32, s: i32, ms: i32) -> TimeOnly {
        let t = NaiveTime::from_hms_milli_opt(h as u32, min as u32, s as u32, ms as u32).unwrap();
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
        let t = dt.get_cdt_with_offset().time();
        TimeOnly(t)
    }

    pub fn fromTimeSpan(ts: TimeSpan) -> TimeOnly {
        minValue().add(ts)
    }

    impl TimeOnly {
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
            let ns = (self.0 - NaiveTime::MIN).num_nanoseconds().unwrap();
            ns / nanoseconds_per_tick
        }

        pub fn toTimeSpan(&self) -> TimeSpan {
            from_ticks(self.ticks())
        }

        pub fn add(&self, ts: TimeSpan) -> TimeOnly {
            let ns = Duration::nanoseconds(ts.ticks() * nanoseconds_per_tick);
            TimeOnly(self.0 + ns)
        }

        pub fn addHours(&self, hours: f64) -> TimeOnly {
            self.add(from_hours(hours))
        }

        pub fn addMinutes(&self, minutes: f64) -> TimeOnly {
            self.add(from_minutes(minutes))
        }
    }
}
