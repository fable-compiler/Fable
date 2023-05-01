#[cfg(feature = "datetime")]
pub mod DateOnly_ {
    use crate::{
        DateTime_::DateTime,
        Native_::{compare, MutCell},
        String_::{fromString, string},
    };
    use chrono::{DateTime as CDateTime, Datelike, NaiveDate};

    #[repr(transparent)]
    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct DateOnly(NaiveDate);

    impl core::fmt::Display for DateOnly {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.0.to_string())
        }
    }

    pub fn compareTo(x: DateOnly, y: DateOnly) -> i32 {
        compare(&x, &y)
    }

    pub fn equals(x: DateOnly, y: DateOnly) -> bool {
        x == y
    }

    pub fn zero() -> DateOnly {
        DateOnly::minValue()
    }

    impl DateOnly {
        pub(crate) fn naive_date(&self) -> NaiveDate {
            self.0
        }

        pub fn new_ymd(y: i32, m: i32, d: i32) -> DateOnly {
            let d = NaiveDate::from_ymd_opt(y, m as u32, d as u32).unwrap();
            DateOnly(d)
        }

        pub fn minValue() -> DateOnly {
            let d = NaiveDate::from_ymd_opt(1, 1, 1).unwrap();
            DateOnly(d)
        }

        pub fn maxValue() -> DateOnly {
            let d = NaiveDate::from_ymd_opt(9999, 12, 31).unwrap();
            DateOnly(d)
        }

        pub fn fromDateTime(dt: DateTime) -> DateOnly {
            let d = dt.get_cdt_with_offset().date_naive();
            DateOnly(d)
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

        pub fn dayNumber(&self) -> i32 {
            self.0.num_days_from_ce()
        }

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

        pub fn toString(&self, format: string) -> string {
            let fmt = format
                .replace("yyyy", "%Y")
                .replace("MM", "%m")
                .replace("dd", "%d");
            let df = self.0.format(&fmt);
            fromString(df.to_string())
        }

        pub fn tryParse(s: string, res: &MutCell<DateOnly>) -> bool {
            match CDateTime::parse_from_rfc3339(s.trim())
                .or(CDateTime::parse_from_rfc2822(s.trim()))
            {
                Ok(dt) => {
                    res.set(DateOnly(dt.naive_utc().date()));
                    true
                }
                Err(e) => false,
            }
        }

        pub fn parse(s: string) -> DateOnly {
            match CDateTime::parse_from_rfc3339(s.trim())
                .or(CDateTime::parse_from_rfc2822(s.trim()))
            {
                Ok(dt) => DateOnly(dt.naive_utc().date()),
                Err(e) => panic!("Input string was not in a correct format."),
            }
        }
    }
}
