pub mod TimeSpan_ {
    use crate::Native_::{compare, MutCell, ToString, Vec};
    use crate::String_::{fromString, string};
    use core::ops::{Add, Div, Mul, Sub};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
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

    impl core::fmt::Display for TimeSpan {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.to_string(string("")))
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
            let hoursTicks = (h as i64) * ticks_per_hour;
            let minsTicks = (m as i64) * ticks_per_minute;
            let secTicks = (s as i64) * ticks_per_second;
            Self::new_ticks(hoursTicks + minsTicks + secTicks)
        }

        pub fn new_dhms(d: i32, h: i32, m: i32, s: i32) -> TimeSpan {
            let hours = d * 24 + h;
            Self::new_hms(hours, m, s)
        }

        pub fn new_dhms_milli(d: i32, h: i32, m: i32, s: i32, ms: i32) -> TimeSpan {
            let hours = (d * 24 + h) as i64;
            let hoursTicks = hours * ticks_per_hour;
            let minsTicks = (m as i64) * ticks_per_minute;
            let secTicks = (s as i64) * ticks_per_second;
            let msTicks = (ms as i64) * ticks_per_millisecond;
            Self::new_ticks(hoursTicks + minsTicks + secTicks + msTicks)
        }

        pub fn from_ticks(ticks: i64) -> TimeSpan {
            Self::new_ticks(ticks)
        }

        pub fn from_days(days: f64) -> TimeSpan {
            Self::from_hours(days * 24.)
        }

        pub fn from_hours(hours: f64) -> TimeSpan {
            Self::from_minutes(hours * 60.)
        }

        pub fn from_minutes(minutes: f64) -> TimeSpan {
            Self::from_seconds(minutes * 60.)
        }

        pub fn from_seconds(seconds: f64) -> TimeSpan {
            TimeSpan {
                ticks: (seconds * (ticks_per_second as f64)) as i64,
            }
        }

        pub fn from_milliseconds(millis: f64) -> TimeSpan {
            TimeSpan {
                ticks: (millis * (ticks_per_millisecond as f64)) as i64,
            }
        }

        pub fn from_microseconds(micros: f64) -> TimeSpan {
            TimeSpan {
                ticks: (micros * (ticks_per_microsecond as f64)) as i64,
            }
        }

        pub fn total_days(&self) -> f64 {
            self.ticks as f64 / ticks_per_day as f64
        }

        pub fn total_hours(&self) -> f64 {
            self.ticks as f64 / ticks_per_hour as f64
        }

        pub fn total_minutes(&self) -> f64 {
            self.ticks as f64 / ticks_per_minute as f64
        }

        pub fn total_seconds(&self) -> f64 {
            self.ticks as f64 / ticks_per_second as f64
        }

        pub fn total_milliseconds(&self) -> f64 {
            self.ticks as f64 / ticks_per_millisecond as f64
        }

        pub fn total_microseconds(&self) -> f64 {
            self.ticks as f64 / ticks_per_microsecond as f64
        }

        pub fn total_nanoseconds(&self) -> f64 {
            (self.ticks * nanoseconds_per_tick) as f64
        }

        pub fn ticks(&self) -> i64 {
            self.ticks
        }

        pub fn duration(&self) -> TimeSpan {
            Self::new_ticks(self.ticks.abs())
        }

        pub fn days(&self) -> i32 {
            self.total_days() as i32
        }

        pub fn hours(&self) -> i32 {
            (self.total_hours() - self.total_days() as i32 as f64 * 24.0) as i32
        }

        pub fn minutes(&self) -> i32 {
            (self.total_minutes() - self.total_hours() as i32 as f64 * 60.0) as i32
        }

        pub fn seconds(&self) -> i32 {
            (self.total_seconds() - self.total_minutes() as i32 as f64 * 60.0) as i32
        }

        pub fn milliseconds(&self) -> i32 {
            (self.total_milliseconds() - self.total_seconds() as i32 as f64 * 1000.0) as i32
        }

        pub fn microseconds(&self) -> i32 {
            (self.total_microseconds() - self.total_milliseconds() as i32 as f64 * 1000.0) as i32
        }

        pub fn nanoseconds(&self) -> i32 {
            (self.total_nanoseconds() - self.total_microseconds() as i32 as f64 * 1000.0) as i32
        }

        pub fn negate(&self) -> TimeSpan {
            Self::new_ticks(-self.ticks)
        }

        pub fn to_string(&self, format: string) -> string {
            let sign = if self.ticks < 0 { "-" } else { "" };
            let days = self.days().abs();
            let days = if days == 0 {
                "".to_string()
            } else {
                format_args!("{}.", days).to_string()
            };
            let hours = self.hours().abs();
            let mins = self.minutes().abs();
            let secs = self.seconds().abs();
            let frac = (self.ticks % ticks_per_second).abs();
            let frac = if frac == 0 {
                "".to_string()
            } else {
                format_args!(".{:07}", frac).to_string()
            };
            let s = match format.as_str() {
                "" | "c" => format_args!(
                    "{}{}{:02}:{:02}:{:02}{}",
                    sign, days, hours, mins, secs, frac
                )
                .to_string(),
                //TODO: support more formats, custom formats, etc.
                _ => format_args!(
                    "{}{}{:02}:{:02}:{:02}{}",
                    sign, days, hours, mins, secs, frac
                )
                .to_string(),
            };
            fromString(s)
        }

        fn try_parse_str(s: &str) -> Result<TimeSpan, ()> {
            let error = Err(());
            let s = s.trim();
            let isNeg = s.starts_with('-');
            let s = if isNeg { &s[1..] } else { s };
            let hms = s.split(':').collect::<Vec<&str>>();
            if s.contains('-') || hms.len() > 3 {
                error
            } else {
                let (d, h, m, s) = if hms.len() == 1 {
                    (hms[0], "0", "0", "0")
                } else {
                    let (d, h) = if let Some(dh) = hms[0].split_once('.') {
                        dh
                    } else {
                        ("0", hms[0])
                    };
                    let m = hms[1];
                    let s = if hms.len() > 2 { hms[2] } else { "0" };
                    (d, h, m, s)
                };
                let d = d.parse::<u32>();
                let h = h.parse::<u32>();
                let m = m.parse::<u32>();
                let s = s.parse::<f64>();
                match (d, h, m, s) {
                    (Ok(d), Ok(h), Ok(m), Ok(s))
                        if d < 10675200 && h < 24 && m < 60 && s < 60.0 =>
                    {
                        let ticks = d as i64 * ticks_per_day
                            + h as i64 * ticks_per_hour
                            + m as i64 * ticks_per_minute
                            + (s * ticks_per_second as f64) as i64;
                        let ticks = if isNeg { -ticks } else { ticks };
                        Ok(Self::from_ticks(ticks))
                    }
                    _ => error,
                }
            }
        }

        pub fn try_parse(s: string, res: &MutCell<TimeSpan>) -> bool {
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
            TimeSpan {
                ticks: self.ticks + rhs.ticks,
            }
        }
    }

    impl Sub<TimeSpan> for TimeSpan {
        type Output = TimeSpan;

        fn sub(self, rhs: TimeSpan) -> Self::Output {
            TimeSpan {
                ticks: self.ticks - rhs.ticks,
            }
        }
    }

    impl Mul<TimeSpan> for TimeSpan {
        type Output = TimeSpan;

        fn mul(self, rhs: TimeSpan) -> Self::Output {
            TimeSpan {
                ticks: self.ticks * rhs.ticks,
            }
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
            TimeSpan {
                ticks: ((self.ticks as f64) * rhs) as i64,
            }
        }
    }

    impl Div<f64> for TimeSpan {
        type Output = TimeSpan;

        fn div(self, rhs: f64) -> Self::Output {
            TimeSpan {
                ticks: ((self.ticks as f64) / rhs) as i64,
            }
        }
    }
}
