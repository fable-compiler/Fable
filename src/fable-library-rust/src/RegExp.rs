#[cfg(feature = "regexp")]
pub mod RegExp_ {
    use core::borrow::{Borrow, BorrowMut};

    use crate::{
        Interfaces_::System::Collections::Generic::{IEnumerable_1, IEnumerator_1},
        NativeArray_::{array_from, new_array, new_empty, Array},
        Native_::{Func1, LrcPtr, ToString, Vec},
        Seq_::ofArray,
        String_::{fromSlice, fromString, get_char_pos, string, substring, substring2},
    };

    #[derive(Clone, Debug)]
    pub struct Regex {
        regex_: regex::Regex,
        options: i32,
    }

    #[derive(Clone, Debug)]
    pub struct Capture {
        text: string,
        index: i32,
        length: i32,
    }

    #[derive(Clone, Debug)]
    pub struct Group {
        name: string,
        captures: CaptureCollection,
    }

    #[derive(Clone, Debug)]
    pub struct Match {
        groups: GroupCollection,
    }

    #[derive(Clone, Debug)]
    pub struct CaptureCollection(Array<Capture>);

    #[derive(Clone, Debug)]
    pub struct GroupCollection(Array<Group>);

    #[derive(Clone, Debug)]
    pub struct MatchCollection(Array<Match>);

    pub type MatchEvaluator = Func1<Match, string>;

    impl core::ops::Deref for Regex {
        type Target = regex::Regex;
        fn deref(&self) -> &Self::Target {
            &self.regex_
        }
    }

    impl Capture {
        pub fn index(&self) -> i32 {
            self.index
        }
        pub fn length(&self) -> i32 {
            self.length
        }
        pub fn value(&self) -> string {
            substring2(self.text.clone(), self.index, self.length)
        }
    }

    impl Group {
        pub fn new(text: string, n: Option<&str>, m: Option<regex::Match>) -> Group {
            match m {
                Some(m) => {
                    let name = fromString(n.unwrap_or("").to_string());
                    let capture = Capture {
                        text,
                        index: m.start() as i32,
                        length: m.len() as i32,
                    };
                    Group {
                        name,
                        captures: CaptureCollection(new_array(&[capture])),
                    }
                }
                None => Group {
                    name: string(""),
                    captures: CaptureCollection(new_empty()),
                },
            }
        }

        pub fn empty() -> Group {
            Group::new(string(""), None, None)
        }

        pub fn name(&self) -> string {
            self.name.clone()
        }

        pub fn captures(&self) -> LrcPtr<CaptureCollection> {
            LrcPtr::new(self.captures.clone())
        }

        pub fn index(&self) -> i32 {
            if self.captures.count() > 0 {
                self.captures.item_n(0).index()
            } else {
                0
            }
        }

        pub fn length(&self) -> i32 {
            if self.captures.count() > 0 {
                self.captures.item_n(0).length()
            } else {
                0
            }
        }

        pub fn value(&self) -> string {
            if self.captures.count() > 0 {
                self.captures.item_n(0).value()
            } else {
                string("")
            }
        }

        pub fn success(&self) -> bool {
            self.captures.count() > 0
        }
    }

    impl Match {
        pub fn new(
            text: &string,
            captureNames: &regex::CaptureNames,
            captures: &regex::Captures,
        ) -> Match {
            let groups = captures
                .iter()
                .zip(captureNames.clone())
                .map(|(m, n)| Group::new(text.clone(), n, m))
                .collect::<Vec<Group>>();
            Match {
                groups: GroupCollection(array_from(groups)),
            }
        }

        pub fn empty() -> Match {
            let empty = Group::new(string(""), None, None);
            Match {
                groups: GroupCollection(new_array(&[empty])),
            }
        }

        pub fn groups(&self) -> LrcPtr<GroupCollection> {
            LrcPtr::new(self.groups.clone())
        }

        pub fn name(&self) -> string {
            self.groups.item_n(0).name()
        }

        pub fn captures(&self) -> LrcPtr<CaptureCollection> {
            self.groups.item_n(0).captures()
        }

        pub fn index(&self) -> i32 {
            self.groups.item_n(0).index()
        }

        pub fn length(&self) -> i32 {
            self.groups.item_n(0).length()
        }

        pub fn value(&self) -> string {
            self.groups.item_n(0).value()
        }

        pub fn success(&self) -> bool {
            self.groups.item_n(0).success()
        }
    }

    impl Regex {
        pub fn new__s(pat: string) -> LrcPtr<Regex> {
            let regex_ = regex::Regex::new(pat.as_str()).unwrap();
            LrcPtr::new(Regex { regex_, options: 0 })
        }

        pub fn new__sn(pat: string, options: i32) -> LrcPtr<Regex> {
            // Supported RegexOptions:
            // * IgnoreCase:               0x0001
            // * Multiline:                0x0002
            // * Compiled:                 0x0008 (ignored)
            // * Singleline:               0x0010
            // * IgnorePatternWhitespace:  0x0020
            // * ECMAScript:               0x0100 (ignored)
            if ((options & !(1 | 2 | 8 | 16 | 32 | 256)) != 0) {
                panic!("Unsupported Regex Option");
            }
            let regex_ = regex::RegexBuilder::new(pat.as_str())
                .case_insensitive((options & 1) != 0)
                .multi_line((options & 2) != 0)
                .dot_matches_new_line((options & 16) != 0)
                .ignore_whitespace((options & 32) != 0)
                //.unicode(false)
                .build()
                .unwrap();
            LrcPtr::new(Regex { regex_, options })
        }

        pub fn escape__s(str: string) -> string {
            fromString(regex::escape(str.as_str()))
        }

        pub fn unescape__s(str: string) -> string {
            let pat = string(r"\\([\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|])");
            let reg = Regex::new__s(pat);
            fromString(reg.replace_all(str.as_str(), "$1").to_string())
        }

        pub fn getGroupNames(&self) -> Array<string> {
            let names: Vec<string> = self
                .capture_names()
                .filter_map(|o| o.map(fromSlice))
                .collect();
            array_from(names)
        }

        pub fn getGroupNumbers(&self) -> Array<i32> {
            let numbers: Vec<i32> = self
                .capture_names()
                .enumerate()
                .filter_map(|(i, o)| o.map(|s| i as i32))
                .collect();
            array_from(numbers)
        }

        pub fn groupNameFromNumber(&self, i: i32) -> string {
            match self.capture_names().nth(i as usize).flatten() {
                Some(s) => fromSlice(s),
                None => string(""),
            }
        }

        pub fn groupNumberFromName(&self, name: string) -> i32 {
            match self.capture_names().enumerate().find(|(i, o)| match o {
                Some(s) => *s == name.as_str(),
                None => false,
            }) {
                Some((i, o)) => i as i32,
                None => -1,
            }
        }

        pub fn isMatch_s(&self, input: string) -> bool {
            self.is_match(input.as_str())
        }

        pub fn isMatch_sn(&self, input: string, start: i32) -> bool {
            let (pos, n) = get_char_pos(&input, start);
            self.is_match_at(input.as_str(), pos)
        }

        pub fn isMatch__ss(input: string, pat: string) -> bool {
            let reg = Regex::new__s(pat);
            reg.isMatch_s(input)
        }

        pub fn isMatch__ssn(input: string, pat: string, options: i32) -> bool {
            let reg = Regex::new__sn(pat, options);
            reg.isMatch_s(input)
        }

        pub fn count_s(&self, input: string) -> i32 {
            self.find_iter(input.as_str()).count() as i32
        }

        pub fn count__ss(input: string, pat: string) -> i32 {
            let reg = Regex::new__s(pat);
            reg.count_s(input)
        }

        pub fn count__ssn(input: string, pat: string, options: i32) -> i32 {
            let reg = Regex::new__sn(pat, options);
            reg.count_s(input)
        }

        pub fn match_s(&self, input: string) -> Match {
            match self.captures(input.as_str()) {
                Some(captures) => Match::new(&input, &self.capture_names(), &captures),
                None => Match::empty(),
            }
        }

        pub fn match_sn(&self, input: string, start: i32) -> Match {
            let (pos, n) = get_char_pos(&input, start);
            match self.captures_at(input.as_str(), pos) {
                Some(captures) => Match::new(&input, &self.capture_names(), &captures),
                None => Match::empty(),
            }
        }

        pub fn match_snn(&self, input: string, start: i32, count: i32) -> Match {
            let input = substring2(input, 0, start + count);
            let (pos, n) = get_char_pos(&input, start);
            match self.captures_at(input.as_str(), pos) {
                Some(captures) => Match::new(&input, &self.capture_names(), &captures),
                None => Match::empty(),
            }
        }

        pub fn match__ss(input: string, pat: string) -> Match {
            let reg = Regex::new__s(pat);
            reg.match_s(input)
        }

        pub fn match__ssn(input: string, pat: string, options: i32) -> Match {
            let reg = Regex::new__sn(pat, options);
            reg.match_s(input)
        }

        pub fn matches_s(&self, input: string) -> LrcPtr<MatchCollection> {
            let names = self.capture_names();
            let matches = self
                .captures_iter(input.as_str())
                .map(|captures| Match::new(&input, &names, &captures))
                .collect::<Vec<Match>>();
            LrcPtr::new(MatchCollection(array_from(matches)))
        }

        pub fn matches_sn(&self, input: string, start: i32) -> LrcPtr<MatchCollection> {
            let input = substring(input, start);
            self.matches_s(input)
        }

        pub fn matches__ss(input: string, pat: string) -> LrcPtr<MatchCollection> {
            let reg = Regex::new__s(pat);
            reg.matches_s(input)
        }

        pub fn matches__ssn(input: string, pat: string, options: i32) -> LrcPtr<MatchCollection> {
            let reg = Regex::new__sn(pat, options);
            reg.matches_s(input)
        }

        pub fn options(&self) -> i32 {
            self.options
        }

        pub fn replace_ss(&self, input: string, rep: string) -> string {
            let str = self.replace_all(input.as_str(), rep.as_str());
            fromString(str.to_string())
        }

        pub fn replace_ssn(&self, input: string, rep: string, count: i32) -> string {
            let str = self.replacen(input.as_str(), count as usize, rep.as_str());
            fromString(str.to_string())
        }

        pub fn replace_ssnn(&self, input: string, rep: string, count: i32, start: i32) -> string {
            let (pos, n) = get_char_pos(&input, start);
            let (prefix, input) = input.split_at(pos);
            let str = self.replacen(input, count as usize, rep.as_str());
            let s = [prefix, str.borrow()].join("");
            fromString(s)
        }

        pub fn replace_sf(&self, input: string, eval: MatchEvaluator) -> string {
            let names = self.capture_names();
            let str = self.replace_all(input.as_str(), |captures: &regex::Captures| {
                eval(Match::new(&input, &names, captures))
            });
            fromString(str.to_string())
        }

        pub fn replace_sfn(&self, input: string, eval: MatchEvaluator, count: i32) -> string {
            let names = self.capture_names();
            let str = self.replacen(
                input.as_str(),
                count as usize,
                |captures: &regex::Captures| eval(Match::new(&input, &names, captures)),
            );
            fromString(str.to_string())
        }

        pub fn replace_sfnn(
            &self,
            input: string,
            eval: MatchEvaluator,
            count: i32,
            start: i32,
        ) -> string {
            let (pos, n) = get_char_pos(&input, start);
            let (prefix, input) = input.split_at(pos);
            let input = fromSlice(input);
            let names = self.capture_names();
            let str = self.replacen(
                input.as_str(),
                count as usize,
                |captures: &regex::Captures| eval(Match::new(&input, &names, captures)),
            );
            let s = [prefix, str.borrow()].join("");
            fromString(s)
        }

        pub fn replace__sss(input: string, pat: string, rep: string) -> string {
            let reg = Regex::new__s(pat);
            reg.replace_ss(input, rep)
        }

        pub fn replace__sssn(input: string, pat: string, rep: string, options: i32) -> string {
            let reg = Regex::new__sn(pat, options);
            reg.replace_ss(input, rep)
        }

        pub fn replace__ssf(input: string, pat: string, eval: MatchEvaluator) -> string {
            let reg = Regex::new__s(pat);
            reg.replace_sf(input, eval)
        }

        pub fn replace__ssfn(
            input: string,
            pat: string,
            eval: MatchEvaluator,
            options: i32,
        ) -> string {
            let reg = Regex::new__sn(pat, options);
            reg.replace_sf(input, eval)
        }

        pub fn split_s(&self, input: string) -> Array<string> {
            let parts: Vec<string> = self.split(input.as_str()).map(fromSlice).collect();
            array_from(parts)
        }

        pub fn split_sn(&self, input: string, count: i32) -> Array<string> {
            let parts: Vec<string> = self
                .splitn(input.as_str(), count as usize)
                .map(fromSlice)
                .collect();
            array_from(parts)
        }

        pub fn split_snn(&self, input: string, count: i32, start: i32) -> Array<string> {
            let input = substring(input, start);
            self.split_sn(input, count)
        }

        pub fn split__ss(input: string, pat: string) -> Array<string> {
            let reg = Regex::new__s(pat);
            reg.split_s(input)
        }

        pub fn split__ssn(input: string, pat: string, options: i32) -> Array<string> {
            let reg = Regex::new__sn(pat, options);
            reg.split_s(input)
        }
    }

    impl CaptureCollection {
        pub fn count(&self) -> i32 {
            self.0.len() as i32
        }

        pub fn item_n(&self, index: i32) -> Capture {
            self.0[index].clone()
        }
    }

    impl IEnumerable_1<Capture> for CaptureCollection {
        fn GetEnumerator(&self) -> LrcPtr<dyn IEnumerator_1<Capture>> {
            ofArray(self.0.clone()).GetEnumerator()
        }
    }

    impl GroupCollection {
        pub fn count(&self) -> i32 {
            self.0.len() as i32
        }

        pub fn item_n(&self, index: i32) -> Group {
            if index >= 0 || index < self.count() {
                self.0[index].clone()
            } else {
                Group::empty()
            }
        }

        pub fn item_s(&self, name: string) -> Group {
            let count = self.count();
            let mut g: Option<Group> = None;
            // linear search should be fast enough
            for i in 0..count {
                if self.0[i].name() == name {
                    g = Some(self.0[i].clone());
                }
            }
            g.unwrap_or(Group::empty())
        }

        pub fn keys(&self) -> Array<string> {
            let count = self.count();
            let mut names = Vec::new();
            for i in 0..count {
                if self.0[i].name().len() > 0 {
                    names.push(self.0[i].name());
                }
            }
            array_from(names)
        }

        pub fn values(&self) -> LrcPtr<GroupCollection> {
            LrcPtr::new(self.clone())
        }
    }

    impl IEnumerable_1<Group> for GroupCollection {
        fn GetEnumerator(&self) -> LrcPtr<dyn IEnumerator_1<Group>> {
            ofArray(self.0.clone()).GetEnumerator()
        }
    }

    impl MatchCollection {
        pub fn count(&self) -> i32 {
            self.0.len() as i32
        }

        pub fn item_n(&self, index: i32) -> Match {
            self.0[index].clone()
        }
    }

    impl IEnumerable_1<Match> for MatchCollection {
        fn GetEnumerator(&self) -> LrcPtr<dyn IEnumerator_1<Match>> {
            ofArray(self.0.clone()).GetEnumerator()
        }
    }
}
