#![allow(non_snake_case)]

// re-export at crate root level
use std::rc::Rc;
use crate::Mutable::*;

type string = Rc<str>;
type Array<T> = Rc<MutCell<Vec<T>>>;

fn array<T: Clone>(v: Vec<T>) -> Array<T> {
    Rc::from(MutCell::from(v))
}

pub mod String_ {
    use super::*;

    // -----------------------------------------------------------
    // Strings
    // -----------------------------------------------------------

    pub fn string(s: &str) -> string {
        Rc::from(s)
    }

    pub fn fromCharCode(code: u32) -> char {
        unsafe { core::char::from_u32_unchecked(code) }
    }

    pub fn toLowerChar(c: &char) -> char {
        c.to_lowercase().next().unwrap()
    }

    pub fn toUpperChar(c: &char) -> char {
        c.to_uppercase().next().unwrap()
    }

    pub fn ofChar(c: &char) -> string {
        let s = [*c].iter().collect::<String>();
        string(&s)
    }

    // O(n) because Rust strings are UTF-8
    pub fn length(s: &string) -> i32 {
        s.chars().count() as i32
    }

    // O(n) because Rust strings are UTF-8
    pub fn getCharAt(s: &string, i: &i32) -> char {
        s.chars().nth(*i as usize).unwrap()
    }

    pub fn fromChar(c: &char, count: &i32) -> string {
        let s = [*c].iter().collect::<String>();
        string(&s.repeat(*count as usize))
    }

    pub fn fromChars(a: &Array<char>) -> string {
        string(&a.iter().collect::<String>())
    }

    pub fn fromChars2(a: &Array<char>, i: &i32, count: &i32) -> string {
        string(&a.iter().skip(*i as usize).take(*count as usize).collect::<String>())
    }

    pub fn containsChar(s: &string, c: &char) -> bool {
        s.contains(*c)
    }

    pub fn contains(s: &string, p: &string) -> bool {
        s.contains(p.as_ref())
    }

    pub fn startsWithChar(s: &string, c: &char) -> bool {
        s.starts_with(*c)
    }

    pub fn startsWith(s: &string, p: &string) -> bool {
        s.starts_with(p.as_ref())
    }

    pub fn endsWithChar(s: &string, c: &char) -> bool {
        s.ends_with(*c)
    }

    pub fn endsWith(s: &string, p: &string) -> bool {
        s.ends_with(p.as_ref())
    }

    pub fn isEmpty(s: &string) -> bool {
        s.is_empty()
    }

    pub fn isWhitespace(s: &string) -> bool {
        s.trim().is_empty()
    }

    pub fn trim(s: &string) -> string {
        string(s.trim())
    }

    pub fn trimChar(s: &string, c: &char) -> string {
        string(s.trim_matches(*c))
    }

    pub fn trimChars(s: &string, a: &Array<char> ) -> string {
        string(s.trim_matches(a.as_slice()))
    }

    pub fn trimEnd(s: &string) -> string {
        string(s.trim_end())
    }

    pub fn trimEndChar(s: &string, c: &char) -> string {
        string(s.trim_end_matches(*c))
    }

    pub fn trimEndChars(s: &string, a: &Array<char> ) -> string {
        string(s.trim_end_matches(a.as_slice()))
    }

    pub fn trimStart(s: &string) -> string {
        string(s.trim_start())
    }

    pub fn trimStartChar(s: &string, c: &char) -> string {
        string(s.trim_start_matches(*c))
    }

    pub fn trimStartChars(s: &string, a: &Array<char> ) -> string {
        string(s.trim_start_matches(a.as_slice()))
    }

    pub fn toLower(s: &string) -> string {
        string(&s.to_lowercase())
    }

    pub fn toUpper(s: &string) -> string {
        string(&s.to_uppercase())
    }

    pub fn concat(a: &Array<string>) -> string {
        string(&a.concat())
    }

    pub fn join(sep: &string, a: &Array<string>) -> string {
        string(&a.join(sep))
    }

    pub fn replace(s: &string, old: &string, new: &string) -> string {
        string(&s.replace(old.as_ref(), new.as_ref()))
    }

    pub fn substring(s: &string, i: &i32) -> string {
        if (*i < 0) { panic!("Argument out of range") }
        string(&s.chars().skip(*i as usize).collect::<String>())
    }

    pub fn substring2(s: &string, i: &i32, count: &i32) -> string {
        if (*i < 0) || (*count < 0) { panic!("Argument out of range") }
        string(&s.chars().skip(*i as usize).take(*count as usize).collect::<String>())
    }

    pub fn insert(s: &string, i: &i32, v: &string) -> string {
        let left = substring2(s, &0, i);
        let right = substring(s, i);
        string(&[left.as_ref(), v.as_ref(), right.as_ref()].concat())
    }

    pub fn remove(s: &string, i: &i32) -> string {
        substring2(s, &0, i)
    }

    pub fn remove2(s: &string, i: &i32, count: &i32) -> string {
        let left = substring2(s, &0, i);
        let right = substring(s, &(i + count));
        string(&[left.as_ref(), right.as_ref()].concat())
    }


    fn toIndex(offset: &i32, opt: Option<(&str, &str)>) -> i32 {
        match opt {
            Some((s, _)) => offset + s.chars().count() as i32,
            None => -1,
        }
    }

    pub fn indexOf(s: &string, p: &string) -> i32 {
        toIndex(&0, s.split_once(p.as_ref()))
    }

    pub fn indexOf2(s: &string, p: &string, i: &i32) -> i32 {
        toIndex(&i, substring(s, i).split_once(p.as_ref()))
    }

    pub fn indexOf3(s: &string, p: &string, i: &i32, count: &i32) -> i32 {
        toIndex(&i, substring2(s, i, count).split_once(p.as_ref()))
    }

    pub fn indexOfChar(s: &string, c: &char) -> i32 {
        toIndex(&0, s.split_once(*c))
    }

    pub fn indexOfChar2(s: &string, c: &char, i: &i32) -> i32 {
        toIndex(i, substring(s, i).split_once(*c))
    }

    pub fn indexOfChar3(s: &string, c: &char, i: &i32, count: &i32) -> i32 {
        toIndex(i, substring2(s, i, count).split_once(*c))
    }

    pub fn indexOfAny(s: &string, a: &Array<char>) -> i32 {
        toIndex(&0, s.split_once(a.as_slice()))
    }

    pub fn indexOfAny2(s: &string, a: &Array<char>, i: &i32) -> i32 {
        toIndex(i, substring(s, i).split_once(a.as_slice()))
    }

    pub fn indexOfAny3(s: &string, a: &Array<char>, i: &i32, count: &i32) -> i32 {
        toIndex(i, substring2(s, i, count).split_once(a.as_slice()))
    }

    pub fn lastIndexOf(s: &string, p: &string) -> i32 {
        toIndex(&0, s.rsplit_once(p.as_ref()))
    }

    pub fn lastIndexOf2(s: &string, p: &string, i: &i32) -> i32 {
        toIndex(&0, substring2(s, &0, &(*i + 1)).rsplit_once(p.as_ref()))
    }

    pub fn lastIndexOf3(s: &string, p: &string, i: &i32, count: &i32) -> i32 {
        toIndex(&(i - count), substring2(s, &(i-count+1), count).rsplit_once(p.as_ref()))
    }

    pub fn lastIndexOfChar(s: &string, c: &char) -> i32 {
        toIndex(&0, s.rsplit_once(*c))
    }

    pub fn lastIndexOfChar2(s: &string, c: &char, i: &i32) -> i32 {
        toIndex(&0, substring2(s, &0, &(*i + 1)).rsplit_once(*c))
    }

    pub fn lastIndexOfChar3(s: &string, c: &char, i: &i32, count: &i32) -> i32 {
        toIndex(&(i - count), substring2(s, &(i-count+1), count).rsplit_once(*c))
    }

    pub fn lastIndexOfAny(s: &string, a: &Array<char>) -> i32 {
        toIndex(&0, s.rsplit_once(a.as_slice()))
    }

    pub fn lastIndexOfAny2(s: &string, a: &Array<char>, i: &i32) -> i32 {
        toIndex(&0, substring2(s, &0, &(*i + 1)).rsplit_once(a.as_slice()))
    }

    pub fn lastIndexOfAny3(s: &string, a: &Array<char>, i: &i32, count: &i32) -> i32 {
        toIndex(&(i - count), substring2(s, &(i-count+1), count).rsplit_once(a.as_slice()))
    }

    pub fn padLeft(s: &string, count: &i32, c: &char) -> string {
        let n = s.chars().count();
        if *count as usize > n {
            let p = [*c].iter().collect::<String>();
            let pad = p.repeat(*count as usize - n);
            string(&[&pad, s.as_ref()].concat())
        } else {
            s.clone()
        }
    }

    pub fn padRight(s: &string, count: &i32, c: &char) -> string {
        let n = s.chars().count();
        if *count as usize > n {
            let p = [*c].iter().collect::<String>();
            let pad = p.repeat(*count as usize - n);
            string(&[s.as_ref(), &pad].concat())
        } else {
            s.clone()
        }
    }

    fn withSplitOptions(a: Vec<&str>, count: &i32, options: &i32) -> Array<string> {
        let mut a: Vec<&str> = a;
        if (*options & 1 != 0) {
            a = a.into_iter().filter(|s| !s.is_empty()).collect();
        }
        if (*options & 2 != 0) {
            a = a.into_iter().map(|s| s.trim()).collect();
        }
        if (*count >= 0) {
            a = a.into_iter().take(*count as usize).collect()
        }
        let a = a.into_iter().map(|s| string(s)).collect();
        array(a)
    }

    pub fn split(s: &string, p: &string, count: &i32, options: &i32) -> Array<string> {
        let a: Vec<&str> =
            if (*count >= 0) && (*options & 1 == 0) {
                if p.is_empty() {
                    s.splitn(*count as usize, char::is_whitespace).collect()
                } else {
                    s.splitn(*count as usize, p.as_ref()).collect()
                }
            } else {
                if p.is_empty() {
                    s.split(char::is_whitespace).collect()
                } else {
                    s.split(p.as_ref()).collect()
                }
            };
        withSplitOptions(a, count, options)
    }

    pub fn splitChars(s: &string, p: &Array<char>, count: &i32, options: &i32) -> Array<string> {
        let a: Vec<&str> =
            if (*count >= 0) && (*options & 1 == 0) {
                if p.is_empty() {
                    s.splitn(*count as usize, char::is_whitespace).collect()
                } else {
                    s.splitn(*count as usize, p.as_slice()).collect()
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

    pub fn toCharArray(s: &string) -> Array<char> {
        array(s.chars().collect())
    }

    pub fn toCharArray2(s: &string, i: &i32, count: &i32) -> Array<char> {
        array(s.chars().skip(*i as usize).take(*count as usize).collect())
    }

    // -----------------------------------------------------------
    // String module
    // -----------------------------------------------------------

    pub fn collect(mapping: &Rc<impl Fn(&char) -> string>, s: &string) -> string {
        let v: Vec<string> = s.chars().map(|c| mapping(&c)).collect();
        string(&v.concat())
    }

    pub fn exists(predicate: &Rc<impl Fn(&char) -> bool>, s: &string) -> bool {
        s.chars().any(|c| predicate(&c))
    }

    pub fn filter(predicate: &Rc<impl Fn(&char) -> bool>, s: &string) -> string {
        string(&s.chars().filter(|c| predicate(&c)).collect::<String>())
    }

    pub fn forAll(predicate: &Rc<impl Fn(&char) -> bool>, s: &string) -> bool {
        s.chars().all(|c| predicate(&c))
    }

    pub fn init(count: &i32, initializer: &Rc<impl Fn(&i32) -> string>) -> string {
        let v: Vec<string> = (0..*count).map(|i| initializer(&i)).collect();
        string(&v.concat())
    }

    pub fn iter(action: &Rc<impl Fn(&char) -> ()>, s: &string) -> () {
        s.chars().for_each(|c| action(&c))
    }

    pub fn iteri(action: &Rc<impl Fn(&i32, &char) -> ()>, s: &string) -> () {
        let mut i: i32 = -1;
        s.chars().for_each(|c| { i += 1; action(&i, &c) })
    }

    pub fn map(mapping: &Rc<impl Fn(&char) -> char>, s: &string) -> string {
        string(&s.chars().map(|c| mapping(&c)).collect::<String>())
    }

    pub fn mapi(mapping: &Rc<impl Fn(&i32, &char) -> char>, s: &string) -> string {
        let mut i: i32 = -1;
        string(&s.chars().map(|c| { i += 1; mapping(&i, &c) }).collect::<String>())
    }

    pub fn replicate(count: &i32, s: &string) -> string {
        string(&s.repeat(*count as usize))
    }

}
