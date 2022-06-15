#![allow(non_snake_case)]

// import at root level
mod Mutable;
mod Lazy;

pub mod Native {

    // re-export at module level
    pub use crate::Choice::*;
    pub use std::collections::{HashMap, HashSet};
    pub use std::rc::Rc;
    pub use super::Mutable::*;
    pub use super::Lazy::*;

    pub type List_1<T> = Option<Rc<crate::List_::Node_1<T>>>;
    pub type Set_1<T> = Option<Rc<crate::Set_::SetTree_1<T>>>;
    pub type Map_2<K, V> = Option<Rc<crate::Map_::MapTree_2<K, V>>>;

    // TODO: use these types in generated code
    pub type string = Rc<str>;
    pub type seq<T> = Rc<dyn crate::Interfaces::IEnumerable_1<T>>;
    pub type RefCell<T> = Rc<MutCell<T>>;
    pub type Array<T> = Rc<MutCell<Vec<T>>>;
    pub type HashSet_1<T> = Rc<MutCell<HashSet<T>>>;
    pub type HashMap_2<K, V> = Rc<MutCell<HashMap<K, V>>>;

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

    pub fn comparer<T: Clone>(comp: Rc<impl Fn(T, T) -> i32>) -> impl Fn(&T, &T) -> Ordering {
        move |x, y| match comp(x.clone(), y.clone()) {
            i if i < 0 => Ordering::Less,
            i if i > 0 => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }

    // -----------------------------------------------------------
    // References
    // -----------------------------------------------------------

    #[inline]
    pub fn mkRef<T>(x: T) -> Rc<T> {
        Rc::from(x)
    }

    #[inline]
    pub fn mkMut<T: Clone>(x: T) -> MutCell<T> {
        MutCell::from(x)
    }

    #[inline]
    pub fn mkRefMut<T: Clone>(x: T) -> Rc<MutCell<T>> {
        mkRef(mkMut(x))
    }

    #[inline]
    pub fn refCell<T: Clone>(x: T) -> RefCell<T> {
        mkRefMut(x)
    }

    #[inline]
    pub fn array<T: Clone>(v: Vec<T>) -> Array<T> {
        mkRefMut(v)
    }

    // -----------------------------------------------------------
    // Arrays
    // -----------------------------------------------------------

    pub fn arrayEmpty<T: Clone>() -> Array<T> {
        array(Vec::new())
    }

    pub fn arrayWithCapacity<T: Clone>(capacity: i32) -> Array<T> {
        array(Vec::with_capacity(capacity as usize))
    }

    pub fn arrayFrom<T: Clone>(a: &[T]) -> Array<T> {
        array(a.to_vec())
    }

    pub fn arrayCreate<T: Clone>(count: &i32, value: &T) -> Array<T> {
        array(vec![value.clone(); *count as usize])
    }

    pub fn arrayCopy<T: Clone>(a: Array<T>) -> Array<T> {
        array(a.to_vec())
    }

    // -----------------------------------------------------------
    // Sequences
    // -----------------------------------------------------------

    pub fn seq_as_iter<T: Clone + 'static>(seq: &seq<T>) -> impl Iterator<Item = T> {
        let en = seq.GetEnumerator();
        let next = move || {
            if en.MoveNext() {
                Some(en.Current().clone())
            } else {
                None
            }
        };
        std::iter::from_fn(next)
    }

    pub fn iter_as_seq<T, I>(iter: I) -> seq<T>
    where
        T: Clone + 'static,
        I: Clone + Iterator<Item = T> + 'static,
    {
        let iter = mkMut(iter);
        let f = mkRef(move || iter.get_mut().next());
        let en = crate::Seq::Enumerable::fromFunction(f);
        crate::Seq::mkSeq(mkRef(move || en.clone()))
    }

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    pub fn hashSetEmpty<T: Clone>() -> HashSet_1<T> {
        mkRefMut(HashSet::new())
    }

    pub fn hashSetWithCapacity<T: Clone>(capacity: i32) -> HashSet_1<T> {
        mkRefMut(HashSet::with_capacity(capacity as usize))
    }

    pub fn hashSetFrom<T: Eq + Hash + Clone>(a: Array<T>) -> HashSet_1<T> {
        mkRefMut(HashSet::from_iter(a.iter().cloned()))
    }

    pub fn hashSetEntries<T: Clone>(set: HashSet_1<T>) -> Array<T> {
        array(Vec::from_iter(set.iter().cloned()))
    }

    // -----------------------------------------------------------
    // HashMaps
    // -----------------------------------------------------------

    pub fn hashMapEmpty<K: Clone, V: Clone>() -> HashMap_2<K, V> {
        mkRefMut(HashMap::new())
    }

    pub fn hashMapWithCapacity<K: Clone, V: Clone>(capacity: i32) -> HashMap_2<K, V> {
        mkRefMut(HashMap::with_capacity(capacity as usize))
    }

    pub fn hashMapFrom<K: Eq + Hash + Clone, V: Clone>(a: Array<(K, V)>) -> HashMap_2<K, V> {
        mkRefMut(HashMap::from_iter(a.iter().cloned()))
    }

    pub fn hashMapTryAdd<K: Eq + Hash + Clone, V: Clone>(
        dict: HashMap_2<K, V>,
        k: K,
        v: V,
    ) -> bool {
        // dict.get_mut().try_insert(k.clone(), v.clone()).is_ok() // nightly only
        if dict.get_mut().contains_key(&k) {
            false
        } else {
            dict.get_mut().insert(k.clone(), v.clone()).is_none()
        }
    }

    pub fn hashMapAdd<K: Eq + Hash + Clone, V: Clone>(dict: HashMap_2<K, V>, k: K, v: V) {
        match dict.get_mut().insert(k.clone(), v.clone()) {
            Some(v) => {
                panic!("An item with the same key has already been added.")
            }
            None => (),
        }
    }

    pub fn hashMapGet<K: Eq + Hash + Clone, V: Clone>(dict: HashMap_2<K, V>, k: K) -> V {
        match dict.get_mut().get(&k) {
            Some(v) => v.clone(),
            None => {
                panic!("The given key was not present in the dictionary.")
            }
        }
    }

    pub fn hashMapSet<K: Eq + Hash + Clone, V: Clone>(dict: HashMap_2<K, V>, k: K, v: V) {
        dict.get_mut().insert(k.clone(), v.clone()); // ignore return value
    }

    pub fn tryGetValue<K: Eq + Hash + Clone, V: Clone>(
        dict: HashMap_2<K, V>,
        k: K,
        res: &RefCell<V>,
    ) -> bool {
        match dict.get_mut().get(&k) {
            Some(v) => {
                res.set(v.clone());
                true
            }
            None => false,
        }
    }

    pub fn hashMapKeys<K: Clone, V: Clone>(dict: HashMap_2<K, V>) -> Array<K> {
        array(Vec::from_iter(dict.keys().cloned()))
    }

    pub fn hashMapValues<K: Clone, V: Clone>(dict: HashMap_2<K, V>) -> Array<V> {
        array(Vec::from_iter(dict.values().cloned()))
    }

    pub fn hashMapEntries<K: Clone, V: Clone>(dict: HashMap_2<K, V>) -> Array<(K, V)> {
        array(Vec::from_iter(
            dict.iter().map(|(k, v)| (k.clone(), v.clone())),
        ))
    }
}
