#![allow(non_snake_case)]

// import at root level
mod Mutable;
mod Lazy;

pub mod Native_ {

    // re-export at module level
    pub use std::boxed::Box as Box_;
    pub use std::rc::Rc;
    pub use std::sync::Arc;
    pub use std::thread_local;
    pub use startup::on_startup;
    pub use super::Mutable::*;
    pub use super::Lazy::*;
    pub use crate::Choice_::*;

    #[cfg(not(feature = "futures"))]
    pub type Lrc<T> = Rc<T>;
    #[cfg(feature = "futures")]
    pub type Lrc<T> = Arc<T>;

    pub type MutArray<T> = MutCell<Vec<T>>;
    pub type MutHashSet<T> = MutCell<std::collections::HashSet<T>>;
    pub type MutHashMap<K, V> = MutCell<std::collections::HashMap<K, V>>;

    pub type Array<T> = Lrc<MutArray<T>>;
    pub type HashSet<T> = Lrc<MutHashSet<T>>;
    pub type HashMap<K, V> = Lrc<MutHashMap<K, V>>;

    // TODO: use these types in generated code
    pub type seq<T> = Lrc<dyn crate::Interfaces_::IEnumerable_1<T>>;
    pub type Seq<T> = crate::Seq_::Enumerable::Seq<T>;
    pub type RefCell<T> = Lrc<MutCell<T>>;
    pub type Nullable<T> = Option<Lrc<T>>;

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

    pub fn comparer<T: Clone>(comp: Lrc<impl Fn(T, T) -> i32>) -> impl Fn(&T, &T) -> Ordering {
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
    pub fn mkRef<T>(x: T) -> Lrc<T> {
        Lrc::from(x)
    }

    #[inline]
    pub fn mkMut<T>(x: T) -> MutCell<T> {
        MutCell::from(x)
    }

    #[inline]
    pub fn mkRefMut<T>(x: T) -> Lrc<MutCell<T>> {
        mkRef(mkMut(x))
    }

    #[inline]
    pub fn refCell<T>(x: T) -> RefCell<T> {
        mkRefMut(x)
    }

    // -----------------------------------------------------------
    // Arrays
    // -----------------------------------------------------------

    pub fn array<T: Clone>(v: Vec<T>) -> Array<T> {
        mkRefMut(v)
    }

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

    pub fn seq_to_iter<T: Clone + 'static>(seq: &seq<T>) -> impl Iterator<Item = T> {
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

    pub fn iter_to_seq<T, I>(iter: I) -> seq<T>
    where
        T: Clone + 'static,
        I: Iterator<Item = T> + 'static,
    {
        let iter = mkMut(iter);
        let f = mkRef(move || iter.get_mut().next());
        let en = crate::Seq_::Enumerable::fromFunction(f);
        crate::Seq_::mkSeq(mkRef(move || en.clone()))
    }

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    pub fn hashSetEmpty<T: Clone>() -> HashSet<T> {
        mkRefMut(std::collections::HashSet::new())
    }

    pub fn hashSetWithCapacity<T: Clone>(capacity: i32) -> HashSet<T> {
        mkRefMut(std::collections::HashSet::with_capacity(capacity as usize))
    }

    pub fn hashSetFrom<T: Eq + Hash + Clone>(a: Array<T>) -> HashSet<T> {
        mkRefMut(std::collections::HashSet::from_iter(a.iter().cloned()))
    }

    pub fn hashSetEntries<T: Clone>(set: HashSet<T>) -> Array<T> {
        array(Vec::from_iter(set.iter().cloned()))
    }

    // -----------------------------------------------------------
    // HashMaps
    // -----------------------------------------------------------

    pub fn hashMapEmpty<K: Clone, V: Clone>() -> HashMap<K, V> {
        mkRefMut(std::collections::HashMap::new())
    }

    pub fn hashMapWithCapacity<K: Clone, V: Clone>(capacity: i32) -> HashMap<K, V> {
        mkRefMut(std::collections::HashMap::with_capacity(capacity as usize))
    }

    pub fn hashMapFrom<K: Eq + Hash + Clone, V: Clone>(a: Array<Lrc<(K, V)>>) -> HashMap<K, V> {
        let it = a.iter().map(|pair| pair.as_ref().clone());
        mkRefMut(std::collections::HashMap::from_iter(it))
    }

    pub fn hashMapTryAdd<K: Eq + Hash + Clone, V: Clone>(
        dict: HashMap<K, V>,
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

    pub fn hashMapAdd<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K, v: V) {
        match dict.get_mut().insert(k.clone(), v.clone()) {
            Some(v) => {
                panic!("An item with the same key has already been added.")
            }
            None => (),
        }
    }

    pub fn hashMapGet<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K) -> V {
        match dict.get_mut().get(&k) {
            Some(v) => v.clone(),
            None => {
                panic!("The given key was not present in the dictionary.")
            }
        }
    }

    pub fn hashMapSet<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K, v: V) {
        dict.get_mut().insert(k.clone(), v.clone()); // ignore return value
    }

    pub fn tryGetValue<K: Eq + Hash + Clone, V: Clone>(
        dict: HashMap<K, V>,
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

    pub fn hashMapKeys<K: Clone, V: Clone>(dict: HashMap<K, V>) -> Array<K> {
        array(Vec::from_iter(dict.keys().cloned()))
    }

    pub fn hashMapValues<K: Clone, V: Clone>(dict: HashMap<K, V>) -> Array<V> {
        array(Vec::from_iter(dict.values().cloned()))
    }

    pub fn hashMapEntries<K: Clone, V: Clone>(dict: HashMap<K, V>) -> Array<(K, V)> {
        array(Vec::from_iter(
            dict.iter().map(|(k, v)| (k.clone(), v.clone())),
        ))
    }
}
