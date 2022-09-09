pub mod TimeSpan_ {
    use crate::String_::string;

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct TimeSpan{
        ticks: i64
    }
    pub(crate) const num_ticks_per_second: i64 = 10_000_000;

    pub fn new_ticks(ticks:i64) -> TimeSpan {
        TimeSpan {ticks: ticks}
    }

    pub fn new_hms(h: i32, m: i32, s: i32) -> TimeSpan {
        let hoursTicks = (h as i64) * num_ticks_per_second * 60 * 60;
        let minsTicks = (m as i64) * num_ticks_per_second * 60;
        let secTicks = (s as i64) * num_ticks_per_second;
        new_ticks(hoursTicks + minsTicks + secTicks)
    }

    pub fn new_dhms(d: i32, h: i32, m: i32, s: i32) -> TimeSpan {
        let hours = d * 24 + h;
        new_hms(hours, m, s)
    }

    pub fn new_dhmsms(d: i32, h: i32, m: i32, s: i32, ms: i32) -> TimeSpan {
        let hours = (d * 24 + h) as i64;
        let hoursTicks = hours * num_ticks_per_second * 60 * 60;
        let minsTicks = (m as i64) * num_ticks_per_second * 60;
        let secTicks = (s as i64) * num_ticks_per_second;
        let msTicks = (ms as i64) * num_ticks_per_second / 1000;
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
        TimeSpan { ticks: (s * (num_ticks_per_second as f64)) as i64 }
    }

    pub fn from_milliseconds(ms: f64) -> TimeSpan {
        let ticks_per_ms = num_ticks_per_second / 1000;
        TimeSpan { ticks: (ms * (ticks_per_ms as f64)) as i64 }
    }




    impl TimeSpan {
        pub fn total_seconds(&self) -> f64 {
            (self.ticks / num_ticks_per_second) as f64
        }

        pub fn total_milliseconds(&self) -> f64 {
            let num_ticks_per_millisecond = num_ticks_per_second / 1000;
            (self.ticks / num_ticks_per_millisecond) as f64
        }
    }

    // impl core::fmt::Display for TimeSpan {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }
}