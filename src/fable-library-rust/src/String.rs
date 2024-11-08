#[cfg_attr(rustfmt, rustfmt::skip)]
pub mod String_ {

    // -----------------------------------------------------------
    // Strings
    // -----------------------------------------------------------

    use crate::Native_::{compare, Func1, Func2, Lrc, String, ToString, Vec};
    use crate::NativeArray_::{array_from, Array};

    use core::cmp::Ordering;
    use core::hash::{Hash, Hasher};

    // -----------------------------------------------------------
    // Simple heap-allocated string type
    // -----------------------------------------------------------

    mod HeapString {
        use crate::Native_::{Lrc, String};

        #[repr(transparent)]
        #[derive(Clone)]
        pub struct LrcStr(Lrc<str>);

        pub type string = LrcStr;

        impl string {
            pub fn as_str(&self) -> &str {
                self.0.as_ref()
            }
        }

        pub fn string(s: &'static str) -> string {
            LrcStr(Lrc::from(s))
        }

        pub fn fromSlice(s: &str) -> string {
            LrcStr(Lrc::from(s))
        }

        pub fn fromString(s: String) -> string {
            LrcStr(Lrc::from(s))
        }

        pub fn fromIter(iter: impl Iterator<Item = char> + Clone) -> string {
            let s = iter.collect::<String>();
            LrcStr(Lrc::from(s))
        }
    }

    // -----------------------------------------------------------
    // Enum string type: enum of static/small/alloc string.
    // -----------------------------------------------------------
    // TODO: maybe intern strings, maybe add length in chars.

    mod EnumString {
        use crate::Native_::{Lrc, String};

        const INLINE_MAX: usize = 22;

        #[derive(Clone)]
        pub enum LrcStr {
            Static(&'static str),
            Inline { len: u8, buf: [u8; INLINE_MAX] },
            Shared(Lrc<str>),
        }

        pub type string = LrcStr;

        impl string {
            pub fn as_str(&self) -> &str {
                match self {
                    LrcStr::Static(str) => str,
                    LrcStr::Shared(rc) => rc.as_ref(),
                    LrcStr::Inline { len, buf } => unsafe {
                        core::str::from_utf8_unchecked(&buf[0..*len as usize])
                    },
                }
            }
        }

        pub fn string(s: &'static str) -> string {
            LrcStr::Static(s)
        }

        pub fn fromSlice(s: &str) -> string {
            let len = s.len();
            if len <= INLINE_MAX {
                let mut buf = [0u8; INLINE_MAX];
                buf[0..len].copy_from_slice(s.as_bytes());
                LrcStr::Inline {
                    len: len as u8,
                    buf: buf,
                }
            } else {
                LrcStr::Shared(Lrc::from(s))
            }
        }

        pub fn fromString(s: String) -> string {
            fromSlice(s.as_str())
        }

        pub fn fromIter(iter: impl Iterator<Item = char> + Clone) -> string {
            let len: usize = iter
                .clone()
                .take(INLINE_MAX + 1)
                .map(|c| c.len_utf8())
                .sum();
            if len <= INLINE_MAX {
                let mut buf = [0u8; INLINE_MAX];
                let mut pos: usize = 0;
                for c in iter {
                    let s = c.encode_utf8(&mut buf[pos..]);
                    pos = pos + s.len();
                }
                LrcStr::Inline {
                    len: len as u8,
                    buf: buf,
                }
            } else {
                let s = iter.collect::<String>();
                LrcStr::Shared(Lrc::from(s))
            }
        }
    }

    #[cfg(feature = "enum_string")]
    pub use EnumString::*;
    #[cfg(not(feature = "enum_string"))]
    pub use HeapString::*;

    // -----------------------------------------------------------
    // macros
    // -----------------------------------------------------------

    #[macro_export]
    macro_rules! printf {
        ($($arg:tt)*) => {{ print!($($arg)*) }}
    }

    #[macro_export]
    macro_rules! printfn {
        ($($arg:tt)*) => {{ println!($($arg)*) }}
    }

    #[macro_export]
    macro_rules! eprintf {
        ($($arg:tt)*) => {{ eprint!($($arg)*) }}
    }

    #[macro_export]
    macro_rules! eprintfn {
        ($($arg:tt)*) => {{ eprintln!($($arg)*) }}
    }

    #[macro_export]
    macro_rules! failwithf {
        ($($arg:tt)*) => {{ panic!($($arg)*) }}
    }

    #[macro_export]
    macro_rules! sprintf {
        ($($arg:tt)*) => {{
            let res = format!($($arg)*);
            $crate::String_::fromString(res)
        }}
    }

    #[macro_export]
    macro_rules! kprintf {
        ($f:expr, $($arg:expr),+) => {{
            let res = format!($($arg),+);
            $f($crate::String_::fromString(res))
        }}
    }

    pub use crate::printf;
    pub use crate::printfn;
    pub use crate::eprintf;
    pub use crate::eprintfn;
    pub use crate::failwithf;
    pub use crate::sprintf;
    pub use crate::kprintf;

    // -----------------------------------------------------------
    // traits
    // -----------------------------------------------------------

    impl core::convert::AsRef<str> for string {
        fn as_ref(&self) -> &str {
            self.as_str()
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
            self.as_str()
        }
    }

    impl core::fmt::Debug for string {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "\"{}\"", self.as_str())
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
            fromString(s)
        }
    }

    impl From<&[char]> for string {
        fn from(a: &[char]) -> Self {
            fromIter(a.iter().copied())
        }
    }

    impl Hash for string {
        #[inline]
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.as_str().hash(state);
        }
    }

    impl PartialEq for string {
        #[inline]
        fn eq(&self, other: &Self) -> bool {
            self.as_str().eq(other.as_str())
        }
    }

    impl Eq for string {}

    impl PartialOrd for string {
        #[inline]
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            self.as_str().partial_cmp(other.as_str())
        }

        #[inline]
        fn lt(&self, other: &Self) -> bool {
            self.as_str() < other.as_str()
        }

        #[inline]
        fn le(&self, other: &Self) -> bool {
            self.as_str() <= other.as_str()
        }

        #[inline]
        fn gt(&self, other: &Self) -> bool {
            self.as_str() > other.as_str()
        }

        #[inline]
        fn ge(&self, other: &Self) -> bool {
            self.as_str() >= other.as_str()
        }
    }

    impl Ord for string {
        #[inline]
        fn cmp(&self, other: &Self) -> Ordering {
            self.as_str().cmp(other.as_str())
        }
    }

    // -----------------------------------------------------------
    // string implementation
    // -----------------------------------------------------------

    pub fn toString<T: ToString>(o: T) -> string {
        fromString(o.to_string())
    }

    pub fn ofChar(c: char) -> string {
        fromIter([c].into_iter())
    }

    pub fn ofBoolean(b: bool) -> string {
        if b { string("True") }
        else { string("False") }
    }

    // O(n) because Rust strings are UTF-8
    pub fn length(s: string) -> i32 {
        s.chars().count() as i32
    }

    // O(n) because Rust strings are UTF-8
    pub fn getCharAt(s: string, i: i32) -> char {
        s.chars().nth(i as usize).unwrap()
    }

    // O(n) because Rust strings are UTF-8
    pub fn get_char_pos(s: &string, i: i32) -> (usize, i32) {
        if i <= 0 { (0, 0) }
        else {
            let mut n = 0;
            let mut pos: usize = 0;
            for c in s.chars().take(i as usize) {
                n = n + 1;
                pos = pos + c.len_utf8();
            }
            (pos, n)
        }
    }

    pub fn fromChar(c: char, count: i32) -> string {
        fromIter(core::iter::repeat(c).take(count as usize))
    }

    pub fn fromChars(a: Array<char>) -> string {
        fromIter(a.iter().copied())
    }

    pub fn fromChars2(a: Array<char>, i: i32, count: i32) -> string {
        fromIter(a.iter().copied().skip(i as usize).take(count as usize))
    }

    // pub mod StringComparison {
    //     pub const CurrentCulture: i32 = 0;
    //     pub const CurrentCultureIgnoreCase: i32 = 1;
    //     pub const InvariantCulture: i32 = 2;
    //     pub const InvariantCultureIgnoreCase: i32 = 3;
    //     pub const Ordinal: i32 = 4;
    //     pub const OrdinalIgnoreCase: i32 = 5;
    // }

    fn isIgnoreCase(comparison: i32) -> bool {
        comparison == 1 || comparison == 3 || comparison == 5
    }

    pub fn containsChar(s: string, c: char) -> bool {
        s.contains(c)
    }

    pub fn containsChar2(s: string, c: char, comparison: i32) -> bool {
        if isIgnoreCase(comparison) {
            let mut buf = [0u8; 4];
            let c_str = c.encode_utf8(&mut buf);
            s.to_uppercase().contains(&c_str.to_uppercase())
        } else {
            s.contains(c)
        }
    }

    pub fn contains(s: string, p: string) -> bool {
        s.contains(p.as_str())
    }

    pub fn contains2(s: string, p: string, comparison: i32) -> bool {
        if isIgnoreCase(comparison) {
            s.to_uppercase().contains(&p.to_uppercase())
        } else {
            s.contains(p.as_str())
        }
    }

    pub fn equalsOrdinal(s1: string, s2: string) -> bool {
        s1.eq(&s2)
    }

    pub fn equals2(s1: string, s2: string, comparison: i32) -> bool {
        if isIgnoreCase(comparison) {
            s1.to_uppercase().eq(&s2.to_uppercase())
        } else {
            s1.eq(&s2)
        }
    }

    pub fn compareOrdinal(s1: string, s2: string) -> i32 {
        compare(&s1, &s2)
    }

    pub fn compareOrdinal2(s1: string, i1: i32, s2: string, i2: i32, count: i32) -> i32 {
        let s1 = substring2(s1, i1, count);
        let s2 = substring2(s2, i2, count);
        compareOrdinal(s1, s2)
    }

    pub fn compareWith(s1: string, s2: string, comparison: i32) -> i32 {
        if isIgnoreCase(comparison) {
            compare(&s1.to_uppercase(), &s2.to_uppercase())
        } else {
            compare(&s1, &s2)
        }
    }

    pub fn compareWith2(s1: string, i1: i32, s2: string, i2: i32, count: i32, comparison: i32) -> i32 {
        let s1 = substring2(s1, i1, count);
        let s2 = substring2(s2, i2, count);
        compareWith(s1, s2, comparison)
    }

    pub fn compareCase(s1: string, s2: string, ignoreCase: bool) -> i32 {
        let comparison = if ignoreCase { 5 } else { 4 };
        compareWith(s1, s2, comparison)
    }

    pub fn compareCase2(s1: string, i1: i32, s2: string, i2: i32, count: i32, ignoreCase: bool) -> i32 {
        let s1 = substring2(s1, i1, count);
        let s2 = substring2(s2, i2, count);
        compareCase(s1, s2, ignoreCase)
    }

    pub fn startsWithChar(s: string, c: char) -> bool {
        s.starts_with(c)
    }

    pub fn startsWith(s: string, p: string) -> bool {
        s.starts_with(p.as_str())
    }

    pub fn startsWith2(s: string, p: string, comparison: i32) -> bool {
        if isIgnoreCase(comparison) {
            s.to_uppercase().starts_with(&p.to_uppercase())
        } else {
            s.starts_with(p.as_str())
        }
    }

    pub fn startsWith3(s: string, p: string, ignoreCase: bool) -> bool {
        let comparison = if ignoreCase { 5 } else { 4 };
        startsWith2(s, p, comparison)
    }

    pub fn endsWithChar(s: string, c: char) -> bool {
        s.ends_with(c)
    }

    pub fn endsWith(s: string, p: string) -> bool {
        s.ends_with(p.as_str())
    }

    pub fn endsWith2(s: string, p: string, comparison: i32) -> bool {
        if isIgnoreCase(comparison) {
            s.to_uppercase().ends_with(&p.to_uppercase())
        } else {
            s.ends_with(p.as_str())
        }
    }

    pub fn endsWith3(s: string, p: string, ignoreCase: bool) -> bool {
        let comparison = if ignoreCase { 5 } else { 4 };
        endsWith2(s, p, comparison)
    }

    pub fn isEmpty(s: string) -> bool {
        s.is_empty()
    }

    pub fn isWhitespace(s: string) -> bool {
        s.trim().is_empty()
    }

    pub fn trim(s: string) -> string {
        fromSlice(s.trim())
    }

    pub fn trimChar(s: string, c: char) -> string {
        fromSlice(s.trim_matches(c))
    }

    pub fn trimChars(s: string, a: Array<char>) -> string {
        fromSlice(s.trim_matches(a.as_slice()))
    }

    pub fn trimEnd(s: string) -> string {
        fromSlice(s.trim_end())
    }

    pub fn trimEndChar(s: string, c: char) -> string {
        fromSlice(s.trim_end_matches(c))
    }

    pub fn trimEndChars(s: string, a: Array<char>) -> string {
        fromSlice(s.trim_end_matches(a.as_slice()))
    }

    pub fn trimStart(s: string) -> string {
        fromSlice(s.trim_start())
    }

    pub fn trimStartChar(s: string, c: char) -> string {
        fromSlice(s.trim_start_matches(c))
    }

    pub fn trimStartChars(s: string, a: Array<char>) -> string {
        fromSlice(s.trim_start_matches(a.as_slice()))
    }

    pub fn toLower(s: string) -> string {
        fromString(s.to_lowercase())
    }

    pub fn toUpper(s: string) -> string {
        fromString(s.to_uppercase())
    }

    pub fn concat(a: Array<string>) -> string {
        fromIter(a.iter().flat_map(|s| s.chars()))
    }

    pub fn join(sep: string, a: Array<string>) -> string {
        let v: Vec<&str> = a.iter().map(|s| s.as_str()).collect();
        fromString(v.join(&sep))
    }

    pub fn replace(s: string, oldValue: string, newValue: string) -> string {
        fromString(s.replace(oldValue.as_str(), newValue.as_str()))
    }

    pub fn replaceChar(s: string, oldChar: char, newChar: char) -> string {
        let mut buf = [0u8; 4];
        let newStr = newChar.encode_utf8(&mut buf);
        fromString(s.replace(oldChar, newStr))
    }

    pub fn substring_safe(s: string, i: i32) -> string {
        if i <= 0 { s }
        else {
            let (pos, n) = get_char_pos(&s, i);
            fromSlice(&s[pos..])
        }
    }

    pub fn substring2_safe(s: string, i: i32, count: i32) -> string {
        let (pos, n1) = get_char_pos(&s, i);
        let (end, n2) = get_char_pos(&s, i + count);
        fromSlice(&s[pos..end])
    }

    pub fn substring(s: string, i: i32) -> string {
        if (i < 0) {
            panic!("Argument out of range")
        }
        let (pos, n) = get_char_pos(&s, i);
        if (n != i) || (pos > s.len()) {
            panic!("Argument out of range")
        }
        fromSlice(&s[pos..])
    }

    pub fn substring2(s: string, i: i32, count: i32) -> string {
        if (i < 0) || (count < 0) {
            panic!("Argument out of range")
        }
        let (pos, n1) = get_char_pos(&s, i);
        let (end, n2) = get_char_pos(&s, i + count);
        if (n1 != i) || (n2 != i + count) || (pos > s.len()) || (end > s.len()) {
            panic!("Argument out of range")
        }
        fromSlice(&s[pos..end])
    }

    pub fn getSlice(s: string, lower: Option<i32>, upper: Option<i32>) -> string {
        match (lower, upper) {
            (None, None) => {
                s
            },
            (Some(start), None) => {
                let start = if start < 0 { 0 } else { start };
                substring_safe(s, start)
            },
            (None, Some(stop)) => {
                let start = 0;
                let count = if stop < start { 0 } else { stop - start + 1 };
                substring2_safe(s, 0, count)
            },
            (Some(start), Some(stop)) => {
                let start = if start < 0 { 0 } else { start };
                let count = if stop < start { 0 } else { stop - start + 1 };
                substring2_safe(s, start, count)
            },
        }
    }

    pub fn append(s1: string, s2: string) -> string {
        fromIter([s1, s2].iter().flat_map(|s| s.chars()))
    }

    pub fn insert(s: string, i: i32, v: string) -> string {
        let left = substring2(s.clone(), 0, i);
        let right = substring(s, i);
        fromIter([left, v, right].iter().flat_map(|s| s.chars()))
    }

    pub fn remove(s: string, i: i32) -> string {
        substring2(s, 0, i)
    }

    pub fn remove2(s: string, i: i32, count: i32) -> string {
        let left = substring2(s.clone(), 0, i);
        let right = substring(s, i + count);
        fromIter([left, right].iter().flat_map(|s| s.chars()))
    }

    fn to_index(offset: i32, opt: Option<(&str, &str)>) -> i32 {
        match opt {
            Some((s, _)) => offset + s.chars().count() as i32,
            None => -1,
        }
    }

    pub fn indexOf(s: string, p: string) -> i32 {
        to_index(0, s.split_once(p.as_str()))
    }

    pub fn indexOf2(s: string, p: string, i: i32) -> i32 {
        to_index(i, substring(s, i).split_once(p.as_str()))
    }

    pub fn indexOf3(s: string, p: string, i: i32, count: i32) -> i32 {
        to_index(i, substring2(s, i, count).split_once(p.as_str()))
    }

    pub fn indexOfChar(s: string, c: char) -> i32 {
        to_index(0, s.split_once(c))
    }

    pub fn indexOfChar2(s: string, c: char, i: i32) -> i32 {
        to_index(i, substring(s, i).split_once(c))
    }

    pub fn indexOfChar3(s: string, c: char, i: i32, count: i32) -> i32 {
        to_index(i, substring2(s, i, count).split_once(c))
    }

    pub fn indexOfAny(s: string, a: Array<char>) -> i32 {
        to_index(0, s.split_once(a.as_slice()))
    }

    pub fn indexOfAny2(s: string, a: Array<char>, i: i32) -> i32 {
        to_index(i, substring(s, i).split_once(a.as_slice()))
    }

    pub fn indexOfAny3(s: string, a: Array<char>, i: i32, count: i32) -> i32 {
        to_index(i, substring2(s, i, count).split_once(a.as_slice()))
    }

    pub fn lastIndexOf(s: string, p: string) -> i32 {
        to_index(0, s.rsplit_once(p.as_str()))
    }

    pub fn lastIndexOf2(s: string, p: string, i: i32) -> i32 {
        to_index(0, substring2(s, 0, (i + 1)).rsplit_once(p.as_str()))
    }

    pub fn lastIndexOf3(s: string, p: string, i: i32, count: i32) -> i32 {
        to_index((i - count), substring2(s, (i-count+1), count).rsplit_once(p.as_str()))
    }

    pub fn lastIndexOfChar(s: string, c: char) -> i32 {
        to_index(0, s.rsplit_once(c))
    }

    pub fn lastIndexOfChar2(s: string, c: char, i: i32) -> i32 {
        to_index(0, substring2(s, 0, i + 1).rsplit_once(c))
    }

    pub fn lastIndexOfChar3(s: string, c: char, i: i32, count: i32) -> i32 {
        to_index(i - count, substring2(s, i-count+1, count).rsplit_once(c))
    }

    pub fn lastIndexOfAny(s: string, a: Array<char>) -> i32 {
        to_index(0, s.rsplit_once(a.as_slice()))
    }

    pub fn lastIndexOfAny2(s: string, a: Array<char>, i: i32) -> i32 {
        to_index(0, substring2(s, 0, i + 1).rsplit_once(a.as_slice()))
    }

    pub fn lastIndexOfAny3(s: string, a: Array<char>, i: i32, count: i32) -> i32 {
        to_index(i - count, substring2(s, i-count+1, count).rsplit_once(a.as_slice()))
    }

    pub fn padLeft(s: string, count: i32, c: char) -> string {
        let n = s.chars().count();
        if count as usize > n {
            let pad = core::iter::repeat(c).take(count as usize - n);
            fromIter(pad.chain(s.chars()))
        } else {
            s.clone()
        }
    }

    pub fn padRight(s: string, count: i32, c: char) -> string {
        let n = s.chars().count();
        if count as usize > n {
            let pad = core::iter::repeat(c).take(count as usize - n);
            fromIter(s.chars().chain(pad))
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
        let a = a.into_iter().map(|s| fromSlice(s)).collect();
        array_from(a)
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

    pub fn splitStrings(s: string, ps: Array<string>, options: i32) -> Array<string> {
        let mut a: Vec<&str> = [s.as_str()].into_iter().collect();
        for p in ps.iter() {
            a = a.iter().flat_map(|&s| s.split(p.as_str())).collect();
        }
        withSplitOptions(a, -1, options)
    }

    pub fn toCharArray(s: string) -> Array<char> {
        array_from(s.chars().collect())
    }

    pub fn toCharArray2(s: string, i: i32, count: i32) -> Array<char> {
        array_from(s.chars().skip(i as usize).take(count as usize).collect())
    }

    // -----------------------------------------------------------
    // String module
    // -----------------------------------------------------------

    pub fn collect(mapping: Func1<char, string>, s: string) -> string {
        let v: Vec<string> = s.chars().map(|c| mapping(c)).collect();
        fromIter(v.iter().flat_map(|s| s.chars()))
    }

    pub fn exists(predicate: Func1<char, bool>, s: string) -> bool {
        s.chars().any(|c| predicate(c))
    }

    pub fn filter(predicate: Func1<char, bool>, s: string) -> string {
        fromIter(s.chars().filter(|c| predicate(*c)))
    }

    pub fn forAll(predicate: Func1<char, bool>, s: string) -> bool {
        s.chars().all(|c| predicate(c))
    }

    pub fn initialize(count: i32, initializer: Func1<i32, string>) -> string {
        let v: Vec<string> = (0..count).map(|i| initializer(i)).collect();
        fromIter(v.iter().flat_map(|s| s.chars()))
    }

    pub fn iterate(action: Func1<char, ()>, s: string) -> () {
        s.chars().for_each(|c| action(c))
    }

    pub fn iterateIndexed(action: Func2<i32, char, ()>, s: string) -> () {
        let mut i: i32 = -1;
        s.chars().for_each(|c| { i += 1; action(i, c) })
    }

    pub fn map(mapping: Func1<char, char>, s: string) -> string {
        fromIter(s.chars().map(|c| mapping(c)))
    }

    pub fn mapIndexed(mapping: Func2<i32, char, char>, s: string) -> string {
        fromIter(s.chars().enumerate().map(|(i, c)| mapping(i as i32, c)))
    }

    pub fn replicate(count: i32, s: string) -> string {
        // fromString(s.repeat(count as usize))
        fromIter(core::iter::repeat(&s).take(count as usize).flat_map(|s| s.chars()))
    }
}
