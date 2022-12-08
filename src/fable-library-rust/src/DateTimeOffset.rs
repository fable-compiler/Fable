#[cfg(feature = "date")]
pub mod DateTimeOffset_ {
    use crate::{DateTime_::DateTime, String_::string};
    use chrono::{DateTime as CDT, FixedOffset, NaiveDateTime, TimeZone, Utc};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct DateTimeOffset(CDT<FixedOffset>);

    // pub fn min_value() -> DateTimeOffset {
    //     DateTimeOffset(CDT::<FixedOffset>::...)
    // }

    // pub fn max_value() -> v {
    //     DateTimeOffset(CDT::<FixedOffset>::...)
    // }

    // pub fn unix_epoch() -> DateTimeOffset {
    //     DateTimeOffset(CDT::<FixedOffset>::...)
    // }

    pub fn from_date(d: DateTime) -> DateTimeOffset {
        let cdto = d.to_cdt_with_offset();
        DateTimeOffset(cdto)
    }

    impl DateTimeOffset {
        pub(crate) fn get_cdt_with_offset(&self) -> CDT<FixedOffset> {
            self.0
        }
    }

    // impl core::fmt::Display for DateTimeOffset {
    //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    //         write!(f, "{}", self.0.to_string())
    //     }
    // }
}