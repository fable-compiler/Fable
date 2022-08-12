// [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed-point_combinators_in_lambda_calculus)

pub mod Guid_ {
    use crate::{Native_::Lrc, String_::string};
    use uuid::{Uuid};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct Guid(Uuid);

    impl Guid {
        pub fn to_string(&self) -> string {
            string(self.0.to_string().as_str())
        }
    }

    pub fn empty() -> Guid {
        Guid(Uuid::nil())
    }

    pub fn new_guid() -> Guid {
        Guid(Uuid::new_v4())
    }

    // .NET only needs V4
    pub fn parse(s: Lrc<str>) -> Guid {
        let copiedInput = s.to_string(); //
        match Uuid::parse_str(&s) {
            Ok(res) => Guid(res),
            Err(x) => panic!("{}", x)
        }
    }
}
