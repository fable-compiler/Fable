#![allow(non_snake_case)]

// import at crate root level
pub(crate) mod Lazy;
pub(crate) mod Mutable;

// re-export at crate root level
pub use Lazy::*;
pub use Mutable::*;

use std::rc::Rc;

pub mod Native {
    use super::*;
    use core::cmp::Ordering;

    pub fn defaultOf<T>() -> T {
        unsafe { std::mem::zeroed() } // will panic on Rc/Arc/Box
    }

    pub fn getZero<T: Default>() -> T {
        Default::default()
    }

    pub fn compare<T>(comparer: &Rc<impl Fn(&T, &T) -> i32>) -> impl Fn(&T, &T) -> Ordering {
        let comp = comparer.clone();
        move |x, y| match comp(x, y) {
            i if i < 0 => Ordering::Less,
            i if i > 0 => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }

    pub fn string(s: &str) -> Rc<str> {
        Rc::from(s)
    }

    pub fn toChar(code: &u32) -> char {
        unsafe { core::char::from_u32_unchecked(*code) }
    }

    pub fn refCell<T: Clone>(x: &T) -> Rc<MutCell<T>> {
        Rc::from(MutCell::from(x.clone()))
    }

    pub fn arrayEmpty<T: Clone>() -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(Vec::new()))
    }

    pub fn arrayWithCapacity<T: Clone>(capacity: &i32) -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(Vec::with_capacity(*capacity as usize)))
    }

    pub fn arrayFrom<T: Clone>(a: &[T]) -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(a.to_vec()))
    }

    pub fn arrayCreate<T: Clone>(count: &i32, value: &T) -> Rc<MutCell<Vec<T>>> {
        let v = vec![value.clone(); *count as usize];
        Rc::from(MutCell::from(v))
    }

    pub fn arrayCopy<T: Clone>(a: &Rc<MutCell<Vec<T>>>) -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(a.get_mut().to_vec()))
    }
}
