#[cfg(feature = "guid")]
pub mod Guid_ {
    use crate::Native_::compare;
    use crate::String_::{string, toString};
    use uuid::{Uuid};

    #[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
    pub struct Guid(Uuid);

    pub const empty: Guid = Guid(Uuid::nil());

    impl core::fmt::Display for Guid {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", toString(&self.0))
        }
    }

    pub fn compareTo(x: Guid, y: Guid) -> i32 {
        compare(&x, &y)
    }

    pub fn equals(x: Guid, y: Guid) -> bool {
        x == y
    }

    pub fn new_guid() -> Guid {
        Guid(Uuid::new_v4())
    }

    pub fn parse(s: string) -> Guid {
        match Uuid::parse_str(s.as_ref()) {
            Ok(res) => Guid(res),
            Err(x) => panic!("{}", x)
        }
    }
}
