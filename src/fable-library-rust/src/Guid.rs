// [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed-point_combinators_in_lambda_calculus)

pub mod Guid_ {
    use crate::{Native_::Lrc, String_::string};

    #[derive(Clone, Copy, PartialEq, PartialOrd, Debug)]
    pub struct Guid {
        bytes: [u8; 16]
    }

    impl Guid {
        pub fn to_string(&self) -> Lrc<str> {
            let data1 = ((self.bytes[0] as u32) << 24) |
            ((self.bytes[1] as u32) << 16) |
            ((self.bytes[2] as u32) <<  8) |
            ((self.bytes[3] as u32) <<  0);
        let data2 = ((self.bytes[4] as u16) <<  8) |
            ((self.bytes[5] as u16) <<  0);
        let data3 = ((self.bytes[6] as u16) <<  8) |
            ((self.bytes[7] as u16) <<  0);

        let s = format!( "{:08x}-\
           {:04x}-\
           {:04x}-\
           {:02x}{:02x}-\
           {:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            data1,
            data2,
            data3,
            self.bytes[8],
            self.bytes[9],
            self.bytes[10],
            self.bytes[11],
            self.bytes[12],
            self.bytes[13],
            self.bytes[14],
            self.bytes[15]);

            string(&s)
        }
    }

    pub fn empty() -> Guid {
        Guid { bytes: [0; 16] }
    }

    pub fn new_guid() -> Guid {
        //let rnd = rand::thread_rng().gen();
        //todo - use rand crate?
        panic!("Not implemented")
    }

    fn copy_memory(dst: &mut [u8], src: &[u8]) {
        for (slot, val) in dst.iter_mut().zip(src.iter()) {
            *slot = *val;
        }
    }

    fn from_bytes(b: &[u8]) -> Guid {
        let len = b.len();
        if len != 16 {
            panic!("Parse error - invalid length {}", len)
            //return Err(ParseError::InvalidLength(len));
        }

        let mut uuid = Guid { bytes: [0; 16] };
        copy_memory(&mut uuid.bytes, b);
        uuid
    }

    // Accumulated length of each hyphenated group in hex digits.
    const ACC_GROUP_LENS: [u8; 5] = [8, 12, 16, 20, 32];
    const SIMPLE_LENGTH: usize = 32;
    const HYPHENATED_LENGTH: usize = 36;

    // copied and modified from UUID crate https://docs.rs/uuid/0.5.1/uuid/index.html
    // .NET only needs V4
    pub fn parse(s: Lrc<str>) -> Guid {
        let copiedInput = s.to_string(); //alg
        let mut input = copiedInput.as_str();

        let len = input.len();
        if len == (HYPHENATED_LENGTH + 9) && input.starts_with("urn:uuid:") {
            input = &input[9..];
        } else if len != SIMPLE_LENGTH && len != HYPHENATED_LENGTH {
            panic!("Invalid length");
            //return Err(ParseError::InvalidLength(len));
        }

        // `digit` counts only hexadecimal digits, `i_char` counts all chars.
        let mut digit = 0;
        let mut group = 0;
        let mut acc = 0;
        let mut buffer = [0u8; 16];

        for (i_char, chr) in input.bytes().enumerate() {
            if digit as usize >= SIMPLE_LENGTH && group != 4 {
                if group == 0 {
                    panic!("Invalid length");
                }
                panic!("Invalid groups");
                //return Err(ParseError::InvalidGroups(group + 1));
            }

            if digit % 2 == 0 {
                // First digit of the byte.
                match chr {
                    // Calulate upper half.
                    b'0'..=b'9' => acc = chr - b'0',
                    b'a'..=b'f' => acc = chr - b'a' + 10,
                    b'A'..=b'F' => acc = chr - b'A' + 10,
                    // Found a group delimiter
                    b'-' => {
                        if ACC_GROUP_LENS[group] != digit {
                            // Calculate how many digits this group consists of in the input.
                            let found = if group > 0 {
                                digit - ACC_GROUP_LENS[group - 1]
                            } else {
                                digit
                            };
                            panic!("Invalid group length");
                            // return Err(ParseError::InvalidGroupLength(group,
                            //                                           found as usize,
                            //                                           GROUP_LENS[group]));
                        }
                        // Next group, decrement digit, it is incremented again at the bottom.
                        group += 1;
                        digit -= 1;
                    }
                    _ => panic!("Invalid character")
                        //return Err(ParseError::InvalidCharacter(input[i_char..].chars().next().unwrap(), i_char)),
                }
            } else {
                // Second digit of the byte, shift the upper half.
                acc *= 16;
                match chr {
                    b'0'..=b'9' => acc += chr - b'0',
                    b'a'..=b'f' => acc += chr - b'a' + 10,
                    b'A'..=b'F' => acc += chr - b'A' + 10,
                    b'-' => {
                        // The byte isn't complete yet.
                        let found = if group > 0 {
                            digit - ACC_GROUP_LENS[group - 1]
                        } else {
                            digit
                        };
                        panic!("Invalid group length")
                        // return Err(ParseError::InvalidGroupLength(group,
                        //                                           found as usize,
                        //                                           GROUP_LENS[group]));
                    }
                    _ => panic!("Invalid character")
                        // return Err(ParseError::InvalidCharacter(input[i_char..].chars().next().unwrap(), i_char)),
                }
                buffer[(digit / 2) as usize] = acc;
            }
            digit += 1;
        }

        // Now check the last group.
        if ACC_GROUP_LENS[4] != digit {
            panic!("Invalid group length")
            // return Err(ParseError::InvalidGroupLength(group,
            //                                           (digit - ACC_GROUP_LENS[3]) as usize,
            //                                           GROUP_LENS[4]));
        }

        from_bytes(&mut buffer)
    }
}
