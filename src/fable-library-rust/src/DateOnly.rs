#[cfg(feature = "datetime")]
pub mod DateOnly_ {
    use crate::{
        DateTime_::{ticks_to_duration, DateTime, DateTimeKind},
        Native_::{compare, MutCell, ToString},
        String_::{fromString, string},
        TimeOnly_::TimeOnly,
        TimeSpan_::ticks_per_day,
    };
    use chrono::{DateTime as CDateTime, Datelike, Months, NaiveDate, NaiveTime, ParseResult};

    #[repr(transparent)]
    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct DateOnly(NaiveDate);

    impl core::fmt::Display for DateOnly {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.toString(string("")))
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

        pub fn fromDayNumber(days: i32) -> DateOnly {
            let d = NaiveDate::from_num_days_from_ce_opt(days + 1).unwrap();
            DateOnly(d)
        }

        pub fn fromDateTime(dt: DateTime) -> DateOnly {
            let d = dt.to_cdt_fixed().date_naive();
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
            self.0.num_days_from_ce() - 1
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

        pub fn addDays(&self, days: i32) -> DateOnly {
            let d = ticks_to_duration(days as i64 * ticks_per_day);
            DateOnly(self.0 + d)
        }

        pub fn addMonths(&self, months: i32) -> DateOnly {
            let ndt = if months < 0 {
                self.0
                    .checked_sub_months(Months::new(-months as u32))
                    .unwrap()
            } else {
                self.0
                    .checked_add_months(Months::new(months as u32))
                    .unwrap()
            };
            DateOnly(ndt)
        }

        pub fn addYears(&self, years: i32) -> DateOnly {
            self.addMonths(years * 12)
        }

        pub fn toDateTime(&self, time: TimeOnly) -> DateTime {
            let ndt = self.0.and_time(time.naive_time());
            DateTime::new(ndt, DateTimeKind::Unspecified)
        }

        pub fn toDateTime2(&self, time: TimeOnly, kind: i32) -> DateTime {
            let ndt = self.0.and_time(time.naive_time());
            DateTime::new_kind(ndt, kind)
        }

        pub fn toString(&self, format: string) -> string {
            let fmt = match format.as_str() {
                "" | "d" => "%m/%d/%Y".to_string(),
                "o" | "O" => "%Y-%m-%d".to_string(),
                //TODO: support more formats, custom formats, etc.
                _ => format
                    .replace("yyyy", "%Y")
                    .replace("MM", "%m")
                    .replace("dd", "%d"),
            };
            let df = self.0.format(&fmt);
            fromString(df.to_string())
        }

        fn try_parse_str(s: &str) -> ParseResult<NaiveDate> {
            s.parse::<NaiveDate>()
                .or(NaiveDate::parse_from_str(s, "%m/%d/%Y"))
                .or(NaiveDate::parse_from_str(s, "%m/%d/%Y"))
        }

        pub fn tryParse(s: string, res: &MutCell<DateOnly>) -> bool {
            match Self::try_parse_str(s.trim()) {
                Ok(nd) => {
                    res.set(DateOnly(nd));
                    true
                }
                Err(e) => false,
            }
        }

        pub fn parse(s: string) -> DateOnly {
            match Self::try_parse_str(s.trim()) {
                Ok(nd) => DateOnly(nd),
                Err(e) => panic!("The input string {} was not in a correct format.", s),
            }
        }
    }
}
