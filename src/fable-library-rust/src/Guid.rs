pub mod Guid_ {
    use crate::{Native_::Lrc, String_::string};
    use uuid::{Uuid};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct Guid(Uuid);

    impl core::fmt::Display for Guid {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.0.to_string())
        }
    }

    pub fn empty() -> Guid {
        Guid(Uuid::nil())
    }

    pub fn new_guid() -> Guid {
        Guid(Uuid::new_v4())
    }

    pub fn parse(s: Lrc<str>) -> Guid {
        let copiedInput = s.to_string();
        match Uuid::parse_str(&s) {
            Ok(res) => Guid(res),
            Err(x) => panic!("{}", x)
        }
    }
}
