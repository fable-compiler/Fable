use crate::Native_::List_1;

pub mod List {
    use crate::{Native_::{List_1, Lrc, MutCell}, List_::{cons, singleton, iterate}};
    use super::super::List_;

    //Cannot impl From and Into traits here as List is not a struct but an alias to types not in this crate - thus following same pattern

    pub fn from_vec<T: Clone>(vec: &Vec<T>) -> List_1<T> {
        let mut lst: List_1<T> = None;
        for (i, item) in vec.iter().rev().enumerate() {
            lst = cons(item.clone(), lst);
        }
        lst
    }

    pub fn into_vec<T: Clone>(lst: List_1<T>) -> Vec<T> {
        let vec = Lrc::from(MutCell::from(Vec::new()));
        iterate(Lrc::from({
            let vec = vec.clone();
            move |item: T| {
                let rawVec = vec.get_mut();
                rawVec.push(item.clone());
            }

        }), lst);
        vec.get()
    }
}

pub mod Array {
    use crate::{Native_::{Array, Lrc, MutCell}, Array_};

    pub fn from_vec<T: Clone>(vec: &Vec<T>) -> Array<T> {
        let vecNew: Vec<T> = vec.iter().map(|item|item.clone()).collect();
        Lrc::from(MutCell::from(vecNew))
    }

    pub fn from_vec_val<T: Clone>(vec: Vec<T>) -> Array<T> {
        Lrc::from(MutCell::from(vec))
    }

    pub fn into_vec<T: Clone>(lst: Array<T>) -> Vec<T> {
        // todo check if Rc has exactly 1 reference, if so skip this copying and move out
        let vecNew: Vec<T> = lst.get().iter().map(|item|item.clone()).collect();
        vecNew
    }
}

pub mod Seq {}

pub mod Set {}

pub mod Map {}