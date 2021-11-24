#![allow(non_snake_case)]

// import at crate root level
pub(crate) mod Mutable;
pub(crate) mod Lazy;

// re-export at crate root level
pub use std::rc::Rc;
pub use Mutable::*;
pub use Lazy::*;

pub mod Native {
    use super::*;

    pub fn defaultOf<T>() -> T {
        unsafe { std::mem::zeroed() }
    }

    pub fn asString(s: &str) -> Rc<str> {
        Rc::from(s)
    }

    pub fn ofString(s: &String) -> Rc<str> {
        Rc::from(s.to_owned())
    }

    pub fn ofArray<T: Clone>(a: &[T]) -> Rc<[MutCell<T>]> {
        let v: Vec<MutCell<T>> = a.iter().map(|x| MutCell::from(x.clone())).collect();
        Rc::from(v)
    }

    pub fn newArray<T: Clone>(count: &i32, value: &T) -> Rc<[MutCell<T>]> {
        let v = vec![value.clone(); *count as usize];
        ofArray(&v)
    }

    pub fn copyArray<T: Clone>(a: &Rc<[MutCell<T>]>) -> Rc<[MutCell<T>]> {
        Rc::from(a.to_vec())
    }
}