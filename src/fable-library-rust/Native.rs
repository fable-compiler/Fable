#![allow(non_snake_case)]

pub mod Native {
    use std::rc::Rc;
    use core::cell::Cell;

    pub fn defaultOf<T>() -> T {
        unsafe { std::mem::zeroed() }
    }

    pub fn cellGet<T: Clone>(c: &Cell<T>) -> T {
        unsafe { (*c.as_ptr()).clone() }
    }

    pub fn asString(s: &str) -> Rc<str> {
        Rc::from(s)
    }

    pub fn ofString(s: &String) -> Rc<str> {
        Rc::from(s.to_owned())
    }

    pub fn ofArray<T: Clone>(a: &[T]) -> Rc<[Cell<T>]> {
        let v: Vec<Cell<T>> = a.iter().map(|x| Cell::from(x.clone())).collect();
        Rc::from(v)
    }

    pub fn newArray<T: Clone>(count: &i32, value: &T) -> Rc<[Cell<T>]> {
        let v = vec![value.clone(); *count as usize];
        ofArray(&v)
    }
}