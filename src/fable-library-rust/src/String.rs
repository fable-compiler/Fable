pub mod String_ {

    // -----------------------------------------------------------
    // Strings
    // -----------------------------------------------------------

    use crate::Native_::{arrayFrom, Array, Lrc, String, ToString, Vec};

    use core::fmt::Debug;
    use core::hash::Hash;

    #[repr(transparent)]
    #[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
    pub struct LrcStr(Lrc<str>);

    pub type string = LrcStr;

    impl string {
        pub fn as_str(&self) -> &str {
            self.as_ref()
        }
    }

    pub fn string(s: &'static str) -> string {
        LrcStr(Lrc::from(s))
    }

    pub fn stringFrom(s: String) -> string {
        LrcStr(Lrc::from(&*s))
    }

    // TODO: maybe intern strings, maybe add length in chars.
    // Alternative string type: enum of static/small/alloc strings.

    // const INLINE_MAX = 22;

    // #[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
    // pub enum LrcStr {
    //     Static(&'static str),
    //     Inline({ len: u8, buf: [u8; INLINE_MAX] }),
    //     Shared(Lrc<String>),
    // }

    // pub type string = LrcStr;

    // impl string {
    //     pub fn as_str(&self) -> &str {
    //         match self {
    //             LrcStr::Static(str) => str,
    //             LrcStr::Shared(rc) => rc.as_ref(),
    //         }
    //     }
    // }

    // pub fn string(s: &'static str) -> string {
    //     LrcStr::Static(s)
    // }

    // pub fn stringFrom(s: String) -> string {
    //     LrcStr::Shared(Lrc::from(s))
    // }

    impl core::convert::AsRef<str> for string {
        fn as_ref(&self) -> &str {
            &self.0.as_ref()
        }
    }

    impl Default for string {
        fn default() -> string {
            string("")
        }
    }

    impl core::ops::Deref for string {
        type Target = str;
        fn deref(&self) -> &Self::Target {
            &self.0.as_ref()
        }
    }

    impl core::fmt::Display for string {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", self.as_str())
        }
    }

    impl From<&'static str> for string {
        fn from(s: &'static str) -> Self {
            string(s)
        }
    }

    impl From<String> for string {
        fn from(s: String) -> Self {
            stringFrom(s)
        }
    }

    pub fn toString<T: ToString>(o: T) -> string {
        stringFrom(o.to_string())
    }

    pub fn toInt8 (s: string) -> i8 { s.parse::<i8>().unwrap() }
    pub fn toUInt8 (s: string) -> u8 { s.parse::<u8>().unwrap() }
    pub fn toInt16 (s: string) -> i16 { s.parse::<i16>().unwrap() }
    pub fn toUInt16 (s: string) -> u16 { s.parse::<u16>().unwrap() }
    pub fn toInt32 (s: string) -> i32 { s.parse::<i32>().unwrap() }
    pub fn toUInt32 (s: string) -> u32 { s.parse::<u32>().unwrap() }
    pub fn toInt64 (s: string) -> i64 { s.parse::<i64>().unwrap() }
    pub fn toUInt64 (s: string) -> u64 { s.parse::<u64>().unwrap() }
    pub fn toFloat32 (s: string) -> f32 { s.parse::<f32>().unwrap() }
    pub fn toFloat64 (s: string) -> f64 { s.parse::<f64>().unwrap() }
    pub fn toNativeInt (s: string) -> isize { s.parse::<isize>().unwrap() }
    pub fn toUNativeInt (s: string) -> usize { s.parse::<usize>().unwrap() }

    pub fn toBoolean (s: string) -> bool { s.parse::<bool>().unwrap() }
    pub fn toChar (s: string) -> char { s.parse::<char>().unwrap() }

    pub fn fromCharCode(code: u32) -> char {
        unsafe { core::char::from_u32_unchecked(code) }
    }

    pub fn toLowerChar(c: char) -> char {
        c.to_lowercase().next().unwrap()
    }

    pub fn toUpperChar(c: char) -> char {
        c.to_uppercase().next().unwrap()
    }

    pub fn ofChar(c: char) -> string {
        let s = [c].iter().collect::<String>();
        stringFrom(s)
    }

    // O(n) because Rust strings are UTF-8
    pub fn length(s: string) -> i32 {
        s.chars().count() as i32
    }

    // O(n) because Rust strings are UTF-8
    pub fn getCharAt(s: string, i: i32) -> char {
        s.chars().nth(i as usize).unwrap()
    }

    pub fn fromChar(c: char, count: i32) -> string {
        let s = [c].iter().collect::<String>();
        stringFrom(s.repeat(count as usize))
    }

    pub fn fromChars(a: Array<char>) -> string {
        stringFrom(a.iter().collect::<String>())
    }

    pub fn fromChars2(a: Array<char>, i: i32, count: i32) -> string {
        stringFrom(a.iter().skip(i as usize).take(count as usize).collect::<String>())
    }

    pub fn containsChar(s: string, c: char) -> bool {
        s.contains(c)
    }

    pub fn contains(s: string, p: string) -> bool {
        s.contains(p.as_str())
    }

    pub fn startsWithChar(s: string, c: char) -> bool {
        s.starts_with(c)
    }

    pub fn startsWith(s: string, p: string) -> bool {
        s.starts_with(p.as_str())
    }

    pub fn endsWithChar(s: string, c: char) -> bool {
        s.ends_with(c)
    }

    pub fn endsWith(s: string, p: string) -> bool {
        s.ends_with(p.as_str())
    }

    pub fn isEmpty(s: string) -> bool {
        s.is_empty()
    }

    pub fn isWhitespace(s: string) -> bool {
        s.trim().is_empty()
    }

    pub fn trim(s: string) -> string {
        stringFrom(s.trim().to_string())
    }

    pub fn trimChar(s: string, c: char) -> string {
        stringFrom(s.trim_matches(c).to_string())
    }

    pub fn trimChars(s: string, a: Array<char>) -> string {
        stringFrom(s.trim_matches(a.as_slice()).to_string())
    }

    pub fn trimEnd(s: string) -> string {
        stringFrom(s.trim_end().to_string())
    }

    pub fn trimEndChar(s: string, c: char) -> string {
        stringFrom(s.trim_end_matches(c).to_string())
    }

    pub fn trimEndChars(s: string, a: Array<char>) -> string {
        stringFrom(s.trim_end_matches(a.as_slice()).to_string())
    }

    pub fn trimStart(s: string) -> string {
        stringFrom(s.trim_start().to_string())
    }

    pub fn trimStartChar(s: string, c: char) -> string {
        stringFrom(s.trim_start_matches(c).to_string())
    }

    pub fn trimStartChars(s: string, a: Array<char>) -> string {
        stringFrom(s.trim_start_matches(a.as_slice()).to_string())
    }

    pub fn toLower(s: string) -> string {
        stringFrom(s.to_lowercase())
    }

    pub fn toUpper(s: string) -> string {
        stringFrom(s.to_uppercase())
    }

    pub fn concat(a: Array<string>) -> string {
        let v: Vec<&str> = a.iter().map(|s| s.as_str()).collect();
        stringFrom(v.concat())
    }

    pub fn join(sep: string, a: Array<string>) -> string {
        let v: Vec<&str> = a.iter().map(|s| s.as_str()).collect();
        stringFrom(v.join(&sep))
    }

    pub fn replace(s: string, old: string, new: string) -> string {
        stringFrom(s.replace(old.as_str(), new.as_str()))
    }

    pub fn substring(s: string, i: i32) -> string {
        if (i < 0) { panic!("Argument out of range") }
        stringFrom(s.chars().skip(i as usize).collect::<String>())
    }

    pub fn substring2(s: string, i: i32, count: i32) -> string {
        if (i < 0) || (count < 0) { panic!("Argument out of range") }
        stringFrom(s.chars().skip(i as usize).take(count as usize).collect::<String>())
    }

    pub fn getSlice(s: string, lower: Option<i32>, upper: Option<i32>) -> string {
        match (lower, upper) {
            (None, None) => {
                s
            },
            (Some(start), None) => {
                let start = if start < 0 { 0 } else { start };
                substring(s, start)
            },
            (None, Some(stop)) => {
                let start = 0;
                let count = if stop < start { 0 } else { stop - start + 1 };
                substring2(s, 0, count)
            },
            (Some(start), Some(stop)) => {
                let start = if start < 0 { 0 } else { start };
                let count = if stop < start { 0 } else { stop - start + 1 };
                substring2(s, start, count)
            },
        }
    }

    pub fn append(s1: string, s2: string) -> string {
        stringFrom([s1.as_str(), s2.as_str()].concat())
    }

    pub fn insert(s: string, i: i32, v: string) -> string {
        let left = substring2(s.clone(), 0, i);
        let right = substring(s, i);
        stringFrom([left.as_str(), v.as_str(), right.as_str()].concat())
    }

    pub fn remove(s: string, i: i32) -> string {
        substring2(s, 0, i)
    }

    pub fn remove2(s: string, i: i32, count: i32) -> string {
        let left = substring2(s.clone(), 0, i);
        let right = substring(s, i + count);
        stringFrom([left.as_str(), right.as_str()].concat())
    }

    fn toIndex(offset: i32, opt: Option<(&str, &str)>) -> i32 {
        match opt {
            Some((s, _)) => offset + s.chars().count() as i32,
            None => -1,
        }
    }

    pub fn indexOf(s: string, p: string) -> i32 {
        toIndex(0, s.split_once(p.as_str()))
    }

    pub fn indexOf2(s: string, p: string, i: i32) -> i32 {
        toIndex(i, substring(s, i).split_once(p.as_str()))
    }

    pub fn indexOf3(s: string, p: string, i: i32, count: i32) -> i32 {
        toIndex(i, substring2(s, i, count).split_once(p.as_str()))
    }

    pub fn indexOfChar(s: string, c: char) -> i32 {
        toIndex(0, s.split_once(c))
    }

    pub fn indexOfChar2(s: string, c: char, i: i32) -> i32 {
        toIndex(i, substring(s, i).split_once(c))
    }

    pub fn indexOfChar3(s: string, c: char, i: i32, count: i32) -> i32 {
        toIndex(i, substring2(s, i, count).split_once(c))
    }

    pub fn indexOfAny(s: string, a: Array<char>) -> i32 {
        toIndex(0, s.split_once(a.as_slice()))
    }

    pub fn indexOfAny2(s: string, a: Array<char>, i: i32) -> i32 {
        toIndex(i, substring(s, i).split_once(a.as_slice()))
    }

    pub fn indexOfAny3(s: string, a: Array<char>, i: i32, count: i32) -> i32 {
        toIndex(i, substring2(s, i, count).split_once(a.as_slice()))
    }

    pub fn lastIndexOf(s: string, p: string) -> i32 {
        toIndex(0, s.rsplit_once(p.as_str()))
    }

    pub fn lastIndexOf2(s: string, p: string, i: i32) -> i32 {
        toIndex(0, substring2(s, 0, (i + 1)).rsplit_once(p.as_str()))
    }

    pub fn lastIndexOf3(s: string, p: string, i: i32, count: i32) -> i32 {
        toIndex((i - count), substring2(s, (i-count+1), count).rsplit_once(p.as_str()))
    }

    pub fn lastIndexOfChar(s: string, c: char) -> i32 {
        toIndex(0, s.rsplit_once(c))
    }

    pub fn lastIndexOfChar2(s: string, c: char, i: i32) -> i32 {
        toIndex(0, substring2(s, 0, i + 1).rsplit_once(c))
    }

    pub fn lastIndexOfChar3(s: string, c: char, i: i32, count: i32) -> i32 {
        toIndex(i - count, substring2(s, i-count+1, count).rsplit_once(c))
    }

    pub fn lastIndexOfAny(s: string, a: Array<char>) -> i32 {
        toIndex(0, s.rsplit_once(a.as_slice()))
    }

    pub fn lastIndexOfAny2(s: string, a: Array<char>, i: i32) -> i32 {
        toIndex(0, substring2(s, 0, i + 1).rsplit_once(a.as_slice()))
    }

    pub fn lastIndexOfAny3(s: string, a: Array<char>, i: i32, count: i32) -> i32 {
        toIndex(i - count, substring2(s, i-count+1, count).rsplit_once(a.as_slice()))
    }

    pub fn padLeft(s: string, count: i32, c: char) -> string {
        let n = s.chars().count();
        if count as usize > n {
            let p = [c].iter().collect::<String>();
            let pad = p.repeat(count as usize - n);
            stringFrom([pad.as_str(), s.as_str()].concat())
        } else {
            s.clone()
        }
    }

    pub fn padRight(s: string, count: i32, c: char) -> string {
        let n = s.chars().count();
        if count as usize > n {
            let p = [c].iter().collect::<String>();
            let pad = p.repeat(count as usize - n);
            stringFrom([s.as_str(), pad.as_str()].concat())
        } else {
            s.clone()
        }
    }

    fn withSplitOptions(a: Vec<&str>, count: i32, options: i32) -> Array<string> {
        let mut a: Vec<&str> = a;
        if (options & 1 != 0) {
            a = a.into_iter().filter(|s| !s.is_empty()).collect();
        }
        if (options & 2 != 0) {
            a = a.into_iter().map(|s| s.trim()).collect();
        }
        if (count >= 0) {
            a = a.into_iter().take(count as usize).collect()
        }
        let a = a.into_iter().map(|s| stringFrom(s.to_string())).collect();
        arrayFrom(a)
    }

    pub fn split(s: string, p: string, count: i32, options: i32) -> Array<string> {
        let a: Vec<&str> =
            if (count >= 0) && (options & 1 == 0) {
                if p.is_empty() {
                    s.splitn(count as usize, char::is_whitespace).collect()
                } else {
                    s.splitn(count as usize, p.as_str()).collect()
                }
            } else {
                if p.is_empty() {
                    s.split(char::is_whitespace).collect()
                } else {
                    s.split(p.as_str()).collect()
                }
            };
        withSplitOptions(a, count, options)
    }

    pub fn splitChars(s: string, p: Array<char>, count: i32, options: i32) -> Array<string> {
        let a: Vec<&str> =
            if (count >= 0) && (options & 1 == 0) {
                if p.is_empty() {
                    s.splitn(count as usize, char::is_whitespace).collect()
                } else {
                    s.splitn(count as usize, p.as_slice()).collect()
                }
            } else {
                if p.is_empty() {
                    s.split(char::is_whitespace).collect()
                } else {
                    s.split(p.as_slice()).collect()
                }
            };
        withSplitOptions(a, count, options)
    }

    pub fn toCharArray(s: string) -> Array<char> {
        arrayFrom(s.chars().collect())
    }

    pub fn toCharArray2(s: string, i: i32, count: i32) -> Array<char> {
        arrayFrom(s.chars().skip(i as usize).take(count as usize).collect())
    }

    // -----------------------------------------------------------
    // String module
    // -----------------------------------------------------------

    pub fn collect(mapping: Lrc<impl Fn(char) -> string>, s: string) -> string {
        let v: Vec<string> = s.chars().map(|c| mapping(c)).collect();
        let v: Vec<&str> = v.iter().map(|s| s.as_str()).collect();
        stringFrom(v.concat())
    }

    pub fn exists(predicate: Lrc<impl Fn(char) -> bool>, s: string) -> bool {
        s.chars().any(|c| predicate(c))
    }

    pub fn filter(predicate: Lrc<impl Fn(char) -> bool>, s: string) -> string {
        stringFrom(s.chars().filter(|c| predicate(*c)).collect::<String>())
    }

    pub fn forAll(predicate: Lrc<impl Fn(char) -> bool>, s: string) -> bool {
        s.chars().all(|c| predicate(c))
    }

    pub fn init(count: i32, initializer: Lrc<impl Fn(i32) -> string>) -> string {
        let v: Vec<string> = (0..count).map(|i| initializer(i)).collect();
        let v: Vec<&str> = v.iter().map(|s| s.as_str()).collect();
        stringFrom(v.concat())
    }

    pub fn iter(action: Lrc<impl Fn(char) -> ()>, s: string) -> () {
        s.chars().for_each(|c| action(c))
    }

    pub fn iteri(action: Lrc<impl Fn(i32, char) -> ()>, s: string) -> () {
        let mut i: i32 = -1;
        s.chars().for_each(|c| { i += 1; action(i, c) })
    }

    pub fn map(mapping: Lrc<impl Fn(char) -> char>, s: string) -> string {
        stringFrom(s.chars().map(|c| mapping(c)).collect::<String>())
    }

    pub fn mapi(mapping: Lrc<impl Fn(i32, char) -> char>, s: string) -> string {
        let mut i: i32 = -1;
        stringFrom(s.chars().map(|c| { i += 1; mapping(i, c) }).collect::<String>())
    }

    pub fn replicate(count: i32, s: string) -> string {
        stringFrom(s.repeat(count as usize))
    }
}