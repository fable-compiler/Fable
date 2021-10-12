#![allow(non_snake_case)]

use std::rc::Rc;
use core::cell::Cell;
use core::cell::RefCell;

#[inline]
pub fn rcString(s: &str) -> Rc<str> { Rc::from(s) }

pub fn rcValArray<T: Copy>(a: &[T]) -> Rc<[Cell<T>]> {
    let v: Vec<Cell<T>> = a.iter().map(|x| Cell::from(*x)).collect();
    Rc::from(v)
}

pub fn rcRefArray<T: Clone>(a: &[T]) -> Rc<[RefCell<T>]> {
    let v: Vec<RefCell<T>> = a.iter().map(|x| RefCell::from(x.clone())).collect();
    Rc::from(v)
}
