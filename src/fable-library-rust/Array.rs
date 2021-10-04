#![allow(dead_code)]
#![allow(non_snake_case)]
pub mod Array {
    use std::rc::Rc;

    pub fn create<T: Clone>(len: &i32, elem: &T) -> Rc<[T]> {
        Rc::from(vec![elem.clone(); *len as usize])
    }

    pub fn copy<T: Clone>(array: &Rc<[T]>) -> Rc<[T]> {
        Rc::from(array.to_vec())
    }

    pub fn reverse<T: Clone>(array: &Rc<[T]>) -> Rc<[T]> {
        let vec = array.to_vec();
        vec.reverse();
        Rc::from(vec)
    }
}
