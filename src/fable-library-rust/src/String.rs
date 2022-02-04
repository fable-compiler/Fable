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

    pub fn toChar(code: &u32) -> char {
        unsafe { core::char::from_u32_unchecked(*code) }
    }

    // O(n) because Rust strings are UTF-8
    pub fn getCharAt(s: &string, index: &i32) -> char {
        s.chars().nth(*index as usize).unwrap()
    }

    pub fn fromChar(c: &char, count: &i32) -> string {
        string(&[*c].repeat(*count as usize).iter().collect::<String>())
    }

    pub fn fromChars(a: &Array<char>) -> string {
        string(&a.iter().collect::<String>())
    }

    pub fn fromChars2(a: &Array<char>, start: &i32, length: &i32) -> string {
        string(&a.iter().skip(*start as usize).take(*length as usize).collect::<String>())
    }

    pub fn containsChar(s: &string, c: &char) -> bool {
        s.contains(*c)
    }

    pub fn containsStr(s: &string, v: &string) -> bool {
        s.contains(v.as_ref())
    }

    pub fn toLowerCase(s: &string) -> string {
        string(&s.to_lowercase())
    }

    pub fn toUpperCase(s: &string) -> string {
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

    pub fn substring(s: &string, start: &i32) -> string {
        string(&s[(*start as usize)..])
    }

    pub fn substring2(s: &string, start: &i32, length: &i32) -> string {
        string(&s[(*start as usize)..((start + length) as usize)])
    }

    pub fn toCharArray(s: &string) -> Array<char> {
        array(s.chars().collect())
    }

    pub fn toCharArray2(s: &string, start: &i32, length: &i32) -> Array<char> {
        array(s.chars().skip(*start as usize).take(*length as usize).collect())
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
        s.char_indices().for_each(|(i, c)| action(&(i as i32), &c))
    }

    pub fn map(mapping: &Rc<impl Fn(&char) -> char>, s: &string) -> string {
        string(&s.chars().map(|c| mapping(&c)).collect::<String>())
    }

    pub fn mapi(mapping: &Rc<impl Fn(&i32, &char) -> char>, s: &string) -> string {
        string(&s.char_indices().map(|(i, c)| mapping(&(i as i32), &c)).collect::<String>())
    }

    pub fn replicate(count: &i32, s: &string) -> string {
        string(&s.repeat(*count as usize))
    }

}
