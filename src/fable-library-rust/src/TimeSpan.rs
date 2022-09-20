pub mod TimeSpan_ {

    use core::ops::{Add, Sub, Mul, Div};

    use crate::String_::string;

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct TimeSpan {
        ticks: i64,
    }

    pub const ticks_per_millisecond: i64 = 10_000;
    pub const ticks_per_second: i64 = 10_000_000;
    pub const ticks_per_minute: i64 = ticks_per_second * 60;
    pub const ticks_per_hour: i64 = ticks_per_minute * 60;
    pub const ticks_per_day: i64 = ticks_per_hour * 24;

    pub const zero: TimeSpan = TimeSpan { ticks: 0 };
    pub const min_value: TimeSpan = TimeSpan { ticks: i64::MIN };
    pub const max_value: TimeSpan = TimeSpan { ticks: i64::MAX };

    pub fn new_ticks(ticks: i64) -> TimeSpan {
        TimeSpan { ticks: ticks }
    }

    pub fn new_hms(h: i32, m: i32, s: i32) -> TimeSpan {
        let hoursTicks = (h as i64) * ticks_per_hour;
        let minsTicks = (m as i64) * ticks_per_minute;
        let secTicks = (s as i64) * ticks_per_second;
        new_ticks(hoursTicks + minsTicks + secTicks)
    }

    pub fn new_dhms(d: i32, h: i32, m: i32, s: i32) -> TimeSpan {
        let hours = d * 24 + h;
        new_hms(hours, m, s)
    }

    pub fn new_dhmsms(d: i32, h: i32, m: i32, s: i32, ms: i32) -> TimeSpan {
        let hours = (d * 24 + h) as i64;
        let hoursTicks = hours * ticks_per_hour;
        let minsTicks = (m as i64) * ticks_per_minute;
        let secTicks = (s as i64) * ticks_per_second;
        let msTicks = (ms as i64) * ticks_per_millisecond;
        new_ticks(hoursTicks + minsTicks + secTicks + msTicks)
    }

    pub fn from_ticks(ticks: i64) -> TimeSpan {
        new_ticks(ticks)
    }

    pub fn from_days(days: f64) -> TimeSpan {
        from_hours(days * 24.)
    }

    pub fn from_hours(h: f64) -> TimeSpan {
        from_minutes(h * 60.)
    }

    pub fn from_minutes(m: f64) -> TimeSpan {
        from_seconds(m * 60.)
    }

    pub fn from_seconds(s: f64) -> TimeSpan {
        TimeSpan {
            ticks: (s * (ticks_per_second as f64)) as i64,
        }
    }

    pub fn from_milliseconds(ms: f64) -> TimeSpan {
        TimeSpan {
            ticks: (ms * (ticks_per_millisecond as f64)) as i64,
        }
    }

    impl TimeSpan {
        pub fn total_seconds(&self) -> f64 {
            (self.ticks / ticks_per_second) as f64
        }

        pub fn total_milliseconds(&self) -> f64 {
            (self.ticks / ticks_per_millisecond) as f64
        }

        pub fn total_days(&self) -> f64 {
            let days = self.ticks as f64 / ticks_per_day as f64;
            days
        }

        pub fn total_hours(&self) -> f64 {
            let hours = self.ticks as f64 / ticks_per_hour as f64;
            hours
        }

        pub fn total_minutes(&self) -> f64 {
            let minutes = self.ticks as f64 / ticks_per_minute as f64;
            minutes
        }

        pub fn ticks(&self) -> i64 {
            self.ticks
        }

        pub fn days(&self) -> i32 {
            self.total_days().floor() as i32
        }

        pub fn hours(&self) -> i32 {
            let leftover_hours = self.total_hours().floor() - self.total_days().floor() * 24.0;
            leftover_hours.floor() as i32
        }

        pub fn minutes(&self) -> i32 {
            let leftover_minutes = self.total_minutes().floor() - self.total_hours().floor() * 60.0;
            leftover_minutes.floor() as i32
        }

        pub fn seconds(&self) -> i32 {
            let leftover_seconds = self.total_seconds().floor() - self.total_minutes().floor() * 60.0;
            leftover_seconds.floor() as i32
        }

        pub fn milliseconds(&self) -> i32 {
            let leftover_milliseconds = self.total_milliseconds().floor() - self.total_seconds().floor() * 1000.0;
            leftover_milliseconds.floor() as i32
        }
    }

    impl Add<TimeSpan> for TimeSpan {
        type Output = TimeSpan;

        fn add(self, rhs: TimeSpan) -> Self::Output {
            TimeSpan { ticks: self.ticks + rhs.ticks }
        }
    }

    impl Sub<TimeSpan> for TimeSpan {
        type Output = TimeSpan;

        fn sub(self, rhs: TimeSpan) -> Self::Output {
            TimeSpan { ticks: self.ticks - rhs.ticks }
        }
    }

    impl Mul<TimeSpan> for TimeSpan {
        type Output = TimeSpan;

        fn mul(self, rhs: TimeSpan) -> Self::Output {
            TimeSpan { ticks: self.ticks * rhs.ticks }
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
            TimeSpan { ticks: ((self.ticks as f64) * rhs) as i64 }
        }
    }

    impl Div<f64> for TimeSpan {
        type Output = TimeSpan;

        fn div(self, rhs: f64) -> Self::Output {
            TimeSpan { ticks: ((self.ticks as f64) / rhs) as i64  }
        }
    }


    // impl core::fmt::Display for TimeSpan {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }
}