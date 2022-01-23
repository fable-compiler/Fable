#![allow(non_snake_case)]

// import at crate root level
pub(crate) mod Lazy;
pub(crate) mod Mutable;

// re-export at crate root level
pub use crate::Choice::*;
pub use std::collections::{HashMap, HashSet};
pub use std::rc::Rc;
pub use Lazy::*;
pub use Mutable::*;

pub type List_1<T> = Option<Rc<crate::List::Node_1<T>>>;
pub type Set_1<T> = Option<Rc<crate::Set::SetTree_1<T>>>;
pub type Map_2<K, V> = Option<Rc<crate::Map::MapTree_2<K, V>>>;

// pub type Array_1<T> = Rc<MutCell<Vec<T>>>;
// pub type HashSet_1<T> = Rc<MutCell<HashSet<T>>>;
// pub type HashMap_2<K, V> = Rc<MutCell<HashMap<K, V>>>;

pub mod Native {
    use super::*;
    use core::cmp::Ordering;
    use core::fmt::Debug;
    use core::hash::Hash;

    pub fn ignore<T>(arg: &T) -> () {}

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

    // -----------------------------------------------------------
    // Arrays
    // -----------------------------------------------------------

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
        Rc::from(MutCell::from(a.to_vec()))
    }

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    pub fn hashSetEmpty<T: Clone>() -> Rc<MutCell<HashSet<T>>> {
        Rc::from(MutCell::from(HashSet::new()))
    }

    pub fn hashSetWithCapacity<T: Clone>(capacity: &i32) -> Rc<MutCell<HashSet<T>>> {
        Rc::from(MutCell::from(HashSet::with_capacity(*capacity as usize)))
    }

    pub fn hashSetFrom<T: Eq + Hash + Clone>(a: &Rc<MutCell<Vec<T>>>) -> Rc<MutCell<HashSet<T>>> {
        Rc::from(MutCell::from(HashSet::from_iter(a.iter().cloned())))
    }

    pub fn hashSetEntries<T: Clone>(set: &Rc<MutCell<HashSet<T>>>) -> Rc<MutCell<Vec<T>>> {
        Rc::from(MutCell::from(Vec::from_iter(set.iter().cloned())))
    }

    // -----------------------------------------------------------
    // HashMaps
    // -----------------------------------------------------------

    pub fn hashMapEmpty<K: Clone, V: Clone>() -> Rc<MutCell<HashMap<K, V>>> {
        Rc::from(MutCell::from(HashMap::new()))
    }

    pub fn hashMapWithCapacity<K: Clone, V: Clone>(capacity: &i32) -> Rc<MutCell<HashMap<K, V>>> {
        Rc::from(MutCell::from(HashMap::with_capacity(*capacity as usize)))
    }

    pub fn hashMapFrom<K: Eq + Hash + Clone, V: Clone>(
        a: &Rc<MutCell<Vec<(K, V)>>>,
    ) -> Rc<MutCell<HashMap<K, V>>> {
        Rc::from(MutCell::from(HashMap::from_iter(a.iter().cloned())))
    }

    pub fn hashMapTryAdd<K: Eq + Hash + Clone, V: Clone>(
        dict: &Rc<MutCell<HashMap<K, V>>>,
        k: &K,
        v: &V,
    ) -> bool {
        // dict.get_mut().try_insert(k.clone(), v.clone()).is_ok() // nightly only
        if dict.get_mut().contains_key(k) {
            false
        } else {
            dict.get_mut().insert(k.clone(), v.clone()).is_none()
        }
    }

    pub fn hashMapAdd<K: Eq + Hash + Clone, V: Clone>(
        dict: &Rc<MutCell<HashMap<K, V>>>,
        k: &K,
        v: &V,
    ) {
        match dict.get_mut().insert(k.clone(), v.clone()) {
            Some(v) => {
                panic!("An item with the same key has already been added.")
            }
            None => (),
        }
    }

    pub fn hashMapGet<K: Eq + Hash + Clone, V: Clone>(
        dict: &Rc<MutCell<HashMap<K, V>>>,
        k: &K,
    ) -> V {
        match dict.get_mut().get(k) {
            Some(v) => v.clone(),
            None => {
                panic!("The given key was not present in the dictionary.")
            }
        }
    }

    pub fn hashMapSet<K: Eq + Hash + Clone, V: Clone>(
        dict: &Rc<MutCell<HashMap<K, V>>>,
        k: &K,
        v: &V,
    ) {
        dict.get_mut().insert(k.clone(), v.clone()); // ignore return value
    }

    pub fn tryGetValue<K: Eq + Hash + Clone, V: Clone>(
        dict: &Rc<MutCell<HashMap<K, V>>>,
        k: &K,
        res: &Rc<MutCell<V>>,
    ) -> bool {
        match dict.get_mut().get(k) {
            Some(v) => {
                res.set(v.clone());
                true
            }
            None => false,
        }
    }

    pub fn hashMapKeys<K: Clone, V: Clone>(
        dict: &Rc<MutCell<HashMap<K, V>>>,
    ) -> Rc<MutCell<Vec<K>>> {
        Rc::from(MutCell::from(Vec::from_iter(dict.keys().cloned())))
    }

    pub fn hashMapValues<K: Clone, V: Clone>(
        dict: &Rc<MutCell<HashMap<K, V>>>,
    ) -> Rc<MutCell<Vec<V>>> {
        Rc::from(MutCell::from(Vec::from_iter(dict.values().cloned())))
    }

    pub fn hashMapEntries<K: Clone, V: Clone>(
        dict: &Rc<MutCell<HashMap<K, V>>>,
    ) -> Rc<MutCell<Vec<(K, V)>>> {
        Rc::from(MutCell::from(Vec::from_iter(
            dict.iter().map(|(k, v)| (k.clone(), v.clone())),
        )))
    }
}
