pub mod Encoding_ {
    use crate::NativeArray_::{array_from, Array};
    use crate::Native_::{Lrc, LrcPtr, OnceInit, String, Vec};
    use crate::String_::{fromChars2, fromSlice, fromString, string, substring2_safe};

    pub trait Encoding: Send + Sync {
        fn getBytes(&self, s: string) -> Array<u8>;
        fn getBytes2(&self, s: string, index: i32, count: i32) -> Array<u8>;
        fn getBytesFromChars(&self, chars: Array<char>) -> Array<u8>;
        fn getBytesFromChars2(&self, chars: Array<char>, index: i32, count: i32) -> Array<u8>;
        fn getByteCount(&self, s: string) -> i32;
        fn getByteCount2(&self, s: string, index: i32, count: i32) -> i32;
        fn getByteCountFromChars(&self, chars: Array<char>) -> i32;
        fn getByteCountFromChars2(&self, chars: Array<char>, index: i32, count: i32) -> i32;
        fn getChars(&self, bytes: Array<u8>) -> Array<char>;
        fn getChars2(&self, bytes: Array<u8>, index: i32, count: i32) -> Array<char>;
        fn getCharCount(&self, bytes: Array<u8>) -> i32;
        fn getCharCount2(&self, bytes: Array<u8>, index: i32, count: i32) -> i32;
        fn getMaxByteCount(&self, charCount: i32) -> i32;
        fn getMaxCharCount(&self, byteCount: i32) -> i32;
        fn getString(&self, bytes: Array<u8>) -> string;
        fn getString2(&self, bytes: Array<u8>, index: i32, count: i32) -> string;
    }

    #[inline]
    fn as_u16_slice(bytes: &[u8]) -> &[u16] {
        unsafe { core::mem::transmute(&bytes[0..(bytes.len() >> 1)]) }
    }

    #[inline]
    fn get_slice(bytes: &[u8], index: i32, count: i32) -> &[u8] {
        &bytes[index as usize..(index + count) as usize]
    }

    pub struct UTF16LE {}

    pub fn get_Unicode() -> LrcPtr<dyn Encoding> {
        static utf16le: OnceInit<LrcPtr<dyn Encoding>> = OnceInit::new();
        utf16le
            .get_or_init(move || LrcPtr::from(Lrc::from(UTF16LE {}) as Lrc<dyn Encoding>))
            .clone()
    }

    impl UTF16LE {
        fn get_string(&self, bytes: &[u8]) -> String {
            let chars = as_u16_slice(&bytes);
            match String::from_utf16(chars) {
                Ok(mut s) => {
                    if bytes.len() & 1 == 1 {
                        let lastByte = bytes[bytes.len() - 1];
                        s.push(lastByte as char);
                    }
                    s
                }
                Err(e) => panic!("{}", e),
            }
        }
    }

    impl Encoding for UTF16LE {
        fn getBytes(&self, s: string) -> Array<u8> {
            let bytes: Vec<u8> = s
                .encode_utf16()
                .flat_map(|c| [(c & 0xFF) as u8, (c >> 8) as u8])
                .collect();
            array_from(bytes)
        }

        fn getBytes2(&self, s: string, index: i32, count: i32) -> Array<u8> {
            self.getBytes(substring2_safe(s, index, count))
        }

        fn getBytesFromChars(&self, chars: Array<char>) -> Array<u8> {
            self.getBytes(fromString(chars.iter().collect()))
        }

        fn getBytesFromChars2(&self, chars: Array<char>, index: i32, count: i32) -> Array<u8> {
            self.getBytes(fromChars2(chars, index, count))
        }

        fn getByteCount(&self, s: string) -> i32 {
            (s.encode_utf16().count() * 2) as i32
        }

        fn getByteCount2(&self, s: string, index: i32, count: i32) -> i32 {
            self.getByteCount(substring2_safe(s, index, count))
        }

        fn getByteCountFromChars(&self, chars: Array<char>) -> i32 {
            self.getByteCount(fromString(chars.iter().collect()))
        }

        fn getByteCountFromChars2(&self, chars: Array<char>, index: i32, count: i32) -> i32 {
            self.getByteCount(fromChars2(chars, index, count))
        }

        fn getChars(&self, bytes: Array<u8>) -> Array<char> {
            let s = self.get_string(&bytes);
            array_from(s.chars().collect())
        }

        fn getChars2(&self, bytes: Array<u8>, index: i32, count: i32) -> Array<char> {
            let s = self.get_string(get_slice(&bytes, index, count));
            array_from(s.chars().collect())
        }

        fn getCharCount(&self, bytes: Array<u8>) -> i32 {
            let s = self.get_string(&bytes);
            s.chars().count() as i32
        }

        fn getCharCount2(&self, bytes: Array<u8>, index: i32, count: i32) -> i32 {
            let s = self.get_string(get_slice(&bytes, index, count));
            s.chars().count() as i32
        }

        fn getMaxByteCount(&self, charCount: i32) -> i32 {
            (charCount + 1) * 2
        }

        fn getMaxCharCount(&self, byteCount: i32) -> i32 {
            (byteCount + 1) / 2 + 1
        }

        fn getString(&self, bytes: Array<u8>) -> string {
            let s = self.get_string(&bytes);
            fromString(s)
        }

        fn getString2(&self, bytes: Array<u8>, index: i32, count: i32) -> string {
            let s = self.get_string(get_slice(&bytes, index, count));
            fromString(s)
        }
    }

    pub struct UTF8 {}

    pub fn get_UTF8() -> LrcPtr<dyn Encoding> {
        static utf8: OnceInit<LrcPtr<dyn Encoding>> = OnceInit::new();
        utf8.get_or_init(move || LrcPtr::from(Lrc::from(UTF8 {}) as Lrc<dyn Encoding>))
            .clone()
    }

    impl UTF8 {
        fn get_string(bytes: &[u8]) -> &str {
            match core::str::from_utf8(bytes) {
                Ok(s) => s,
                Err(e) => panic!("{}", e),
            }
        }
    }

    impl Encoding for UTF8 {
        fn getBytes(&self, s: string) -> Array<u8> {
            array_from(s.as_bytes().to_vec())
        }

        fn getBytes2(&self, s: string, index: i32, count: i32) -> Array<u8> {
            self.getBytes(substring2_safe(s, index, count))
        }

        fn getBytesFromChars(&self, chars: Array<char>) -> Array<u8> {
            self.getBytes(fromString(chars.iter().collect()))
        }

        fn getBytesFromChars2(&self, chars: Array<char>, index: i32, count: i32) -> Array<u8> {
            self.getBytes(fromChars2(chars, index, count))
        }

        fn getByteCount(&self, s: string) -> i32 {
            s.as_bytes().len() as i32
        }

        fn getByteCount2(&self, s: string, index: i32, count: i32) -> i32 {
            self.getByteCount(substring2_safe(s, index, count))
        }

        fn getByteCountFromChars(&self, chars: Array<char>) -> i32 {
            self.getByteCount(fromString(chars.iter().collect()))
        }

        fn getByteCountFromChars2(&self, chars: Array<char>, index: i32, count: i32) -> i32 {
            self.getByteCount(fromChars2(chars, index, count))
        }

        fn getChars(&self, bytes: Array<u8>) -> Array<char> {
            let s = UTF8::get_string(&bytes);
            array_from(s.chars().collect())
        }

        fn getChars2(&self, bytes: Array<u8>, index: i32, count: i32) -> Array<char> {
            let s = UTF8::get_string(get_slice(&bytes, index, count));
            array_from(s.chars().collect())
        }

        fn getCharCount(&self, bytes: Array<u8>) -> i32 {
            let s = UTF8::get_string(&bytes);
            s.chars().count() as i32
        }

        fn getCharCount2(&self, bytes: Array<u8>, index: i32, count: i32) -> i32 {
            let s = UTF8::get_string(get_slice(&bytes, index, count));
            s.chars().count() as i32
        }

        fn getMaxByteCount(&self, charCount: i32) -> i32 {
            (charCount + 1) * 3
        }

        fn getMaxCharCount(&self, byteCount: i32) -> i32 {
            byteCount + 1
        }

        fn getString(&self, bytes: Array<u8>) -> string {
            let s = UTF8::get_string(&bytes);
            fromSlice(s)
        }

        fn getString2(&self, bytes: Array<u8>, index: i32, count: i32) -> string {
            let s = UTF8::get_string(get_slice(&bytes, index, count));
            fromSlice(s)
        }
    }
}
