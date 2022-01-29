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

pub type List_1<T> = Option<Rc<crate::List_::Node_1<T>>>;
pub type Set_1<T> = Option<Rc<crate::Set_::SetTree_1<T>>>;
pub type Map_2<K, V> = Option<Rc<crate::Map_::MapTree_2<K, V>>>;

// TODO: make public eventually
type string = Rc<str>;
type RefCell<T> = Rc<MutCell<T>>;
type Array<T> = Rc<MutCell<Vec<T>>>;
type HashSet_1<T> = Rc<MutCell<HashSet<T>>>;
type HashMap_2<K, V> = Rc<MutCell<HashMap<K, V>>>;

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
    // Strings
    // -----------------------------------------------------------

    pub fn toChar(code: &u32) -> char {
        unsafe { core::char::from_u32_unchecked(*code) }
    }

    pub fn getCharAt(s: &string, index: &i32) -> char {
        // O(n) because of UTF-8
        s[(*index as usize)..].chars().next().unwrap()
    }

    pub fn string(s: &str) -> string {
        Rc::from(s)
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
        let slice: &str = &s[(*start as usize)..];
        string(slice)
    }

    pub fn substring2(s: &string, start: &i32, length: &i32) -> string {
        let slice: &str = &s[(*start as usize)..((start + length) as usize)];
        string(slice)
    }

    pub fn toCharArray(s: &string) -> Array<char> {
        array(s.chars().collect())
    }

    pub fn toCharArray2(s: &string, start: &i32, length: &i32) -> Array<char> {
        let slice: &str = &s[(*start as usize)..((start + length) as usize)];
        array(slice.chars().collect())
    }

    // -----------------------------------------------------------
    // Arrays
    // -----------------------------------------------------------

    pub fn arrayEmpty<T: Clone>() -> Array<T> {
        array(Vec::new())
    }

    pub fn arrayWithCapacity<T: Clone>(capacity: &i32) -> Array<T> {
        array(Vec::with_capacity(*capacity as usize))
    }

    pub fn arrayFrom<T: Clone>(a: &[T]) -> Array<T> {
        array(a.to_vec())
    }

    pub fn arrayCreate<T: Clone>(count: &i32, value: &T) -> Array<T> {
        array(vec![value.clone(); *count as usize])
    }

    pub fn arrayCopy<T: Clone>(a: &Array<T>) -> Array<T> {
        array(a.to_vec())
    }

    // -----------------------------------------------------------
    // HashSets
    // -----------------------------------------------------------

    pub fn hashSetEmpty<T: Clone>() -> HashSet_1<T> {
        mkRefMut(HashSet::new())
    }

    pub fn hashSetWithCapacity<T: Clone>(capacity: &i32) -> HashSet_1<T> {
        mkRefMut(HashSet::with_capacity(*capacity as usize))
    }

    pub fn hashSetFrom<T: Eq + Hash + Clone>(a: &Array<T>) -> HashSet_1<T> {
        mkRefMut(HashSet::from_iter(a.iter().cloned()))
    }

    pub fn hashSetEntries<T: Clone>(set: &HashSet_1<T>) -> Array<T> {
        array(Vec::from_iter(set.iter().cloned()))
    }

    // -----------------------------------------------------------
    // HashMaps
    // -----------------------------------------------------------

    pub fn hashMapEmpty<K: Clone, V: Clone>() -> HashMap_2<K, V> {
        mkRefMut(HashMap::new())
    }

    pub fn hashMapWithCapacity<K: Clone, V: Clone>(capacity: &i32) -> HashMap_2<K, V> {
        mkRefMut(HashMap::with_capacity(*capacity as usize))
    }

    pub fn hashMapFrom<K: Eq + Hash + Clone, V: Clone>(a: &Array<(K, V)>) -> HashMap_2<K, V> {
        mkRefMut(HashMap::from_iter(a.iter().cloned()))
    }

    pub fn hashMapTryAdd<K: Eq + Hash + Clone, V: Clone>(
        dict: &HashMap_2<K, V>,
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

    pub fn hashMapAdd<K: Eq + Hash + Clone, V: Clone>(dict: &HashMap_2<K, V>, k: &K, v: &V) {
        match dict.get_mut().insert(k.clone(), v.clone()) {
            Some(v) => {
                panic!("An item with the same key has already been added.")
            }
            None => (),
        }
    }

    pub fn hashMapGet<K: Eq + Hash + Clone, V: Clone>(dict: &HashMap_2<K, V>, k: &K) -> V {
        match dict.get_mut().get(k) {
            Some(v) => v.clone(),
            None => {
                panic!("The given key was not present in the dictionary.")
            }
        }
    }

    pub fn hashMapSet<K: Eq + Hash + Clone, V: Clone>(dict: &HashMap_2<K, V>, k: &K, v: &V) {
        dict.get_mut().insert(k.clone(), v.clone()); // ignore return value
    }

    pub fn tryGetValue<K: Eq + Hash + Clone, V: Clone>(
        dict: &HashMap_2<K, V>,
        k: &K,
        res: &RefCell<V>,
    ) -> bool {
        match dict.get_mut().get(k) {
            Some(v) => {
                res.set(v.clone());
                true
            }
            None => false,
        }
    }

    pub fn hashMapKeys<K: Clone, V: Clone>(dict: &HashMap_2<K, V>) -> Array<K> {
        array(Vec::from_iter(dict.keys().cloned()))
    }

    pub fn hashMapValues<K: Clone, V: Clone>(dict: &HashMap_2<K, V>) -> Array<V> {
        array(Vec::from_iter(dict.values().cloned()))
    }

    pub fn hashMapEntries<K: Clone, V: Clone>(dict: &HashMap_2<K, V>) -> Array<(K, V)> {
        array(Vec::from_iter(
            dict.iter().map(|(k, v)| (k.clone(), v.clone())),
        ))
    }
}
