#[cfg(feature = "guid")]
pub mod Guid_ {
    use crate::NativeArray_::{new_array, Array};
    use crate::Native_::{compare, MutCell};
    use crate::String_::{string, toString};
    use uuid::Uuid;

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

    pub fn new_from_array(a: Array<u8>) -> Guid {
        Guid(Uuid::from_slice_le(a.as_slice()).unwrap())
    }

    pub fn tryParse(s: string, res: &MutCell<Guid>) -> bool {
        match Uuid::parse_str(s.trim()) {
            Ok(uuid) => {
                res.set(Guid(uuid));
                true
            }
            Err(e) => false,
        }
    }

    pub fn parse(s: string) -> Guid {
        match Uuid::parse_str(s.trim()) {
            Ok(uuid) => Guid(uuid),
            Err(e) => panic!("{}", e),
        }
    }

    pub fn toByteArray(x: Guid) -> Array<u8> {
        new_array(&x.0.to_bytes_le())
    }
}
