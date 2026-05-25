#[cfg(feature = "guid")]
pub mod Guid_ {
    use crate::DateTimeOffset_::DateTimeOffset;
    use crate::NativeArray_::{new_array, Array};
    use crate::Native_::{compare, format, getHashCode, Hashable, MutCell, ToString};
    use crate::String_::{fromString, string};
    use uuid::{NoContext, Timestamp, Uuid};

    #[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
    #[repr(transparent)]
    pub struct Guid(Uuid);

    pub const empty: Guid = Guid(Uuid::nil());

    impl core::fmt::Display for Guid {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.toString(string("")))
        }
    }

    impl Hashable for Guid {
        #[inline]
        fn getHashCode(&self) -> i32 {
            let bytes = self.toByteArray();
            getHashCode(&bytes)
        }
    }

    pub fn compareTo(x: Guid, y: Guid) -> i32 {
        compare(&x, &y)
    }

    pub fn equals(x: Guid, y: Guid) -> bool {
        x == y
    }

    impl Guid {
        pub fn new_guid() -> Guid {
            Guid(Uuid::new_v4())
        }

        pub fn new_from_array(a: Array<u8>) -> Guid {
            Guid(Uuid::from_slice_le(a.as_slice()).unwrap())
        }

        pub fn create_version7() -> Guid {
            let dto = DateTimeOffset::utcNow();
            Self::create_version7_with(dto)
        }

        pub fn create_version7_with(dto: DateTimeOffset) -> Guid {
            let seconds = dto.toUnixTimeSeconds() as u64;
            let subsec_nanos = dto.toUnixTimeSubsecNanos();
            let ts = Timestamp::from_unix(NoContext, seconds, subsec_nanos);
            Guid(Uuid::new_v7(ts))
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

        pub fn toByteArray(&self) -> Array<u8> {
            new_array(&self.0.to_bytes_le())
        }

        pub fn toString(&self, format: string) -> string {
            match format.as_str() {
                "" | "D" | "d" => fromString(self.0.hyphenated().to_string()),
                "N" | "n" => fromString(self.0.simple().to_string()),
                "B" | "b" => fromString(format!("{{{}}}", self.0.hyphenated())),
                "P" | "p" => fromString(format!("({})", self.0.hyphenated())),
                "X" | "x" => {
                    let bytes = self.0.as_bytes();
                    let data1 = u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
                    let data2 = u16::from_be_bytes([bytes[4], bytes[5]]);
                    let data3 = u16::from_be_bytes([bytes[6], bytes[7]]);
                    fromString(format!(
                        "{{0x{:08x},0x{:04x},0x{:04x},{{0x{:02x},0x{:02x},0x{:02x},0x{:02x},0x{:02x},0x{:02x},0x{:02x},0x{:02x}}}}}",
                    data1,
                    data2,
                    data3,
                    bytes[8],
                    bytes[9],
                    bytes[10],
                    bytes[11],
                    bytes[12],
                    bytes[13],
                    bytes[14],
                    bytes[15]
                ))
                }
                _ => panic!("Format specifier was invalid."),
            }
        }
    }
}
