pub mod HashMap_ {

    // -----------------------------------------------------------
    // HashMaps
    // -----------------------------------------------------------

    #[cfg(feature = "no_std")]
    use hashbrown as collections;
    #[cfg(not(feature = "no_std"))]
    use std::collections;

    use crate::Native_::{mkRefMut, Lrc, LrcPtr, MutCell, Vec};
    use crate::NativeArray_::{array_from, Array};
    type MutHashMap<K, V> = MutCell<collections::HashMap<K, V>>;

    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::Hash;

    #[repr(transparent)]
    #[derive(Clone, Debug, Default)] //, PartialEq, PartialOrd, Eq, Hash, Ord)]
    pub struct HashMap<K: Clone, V: Clone>(Lrc<MutHashMap<K, V>>);

    impl<K: Clone, V: Clone> core::ops::Deref for HashMap<K, V> {
        type Target = Lrc<MutHashMap<K, V>>;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<K: Clone + Debug, V: Clone + Debug> Display for HashMap<K, V> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.0) //TODO:
        }
    }

    pub fn new_empty<K: Clone, V: Clone>() -> HashMap<K, V> {
        HashMap(mkRefMut(collections::HashMap::new()))
    }

    pub fn new_with_capacity<K: Clone, V: Clone>(capacity: i32) -> HashMap<K, V> {
        HashMap(mkRefMut(collections::HashMap::with_capacity(
            capacity as usize,
        )))
    }

    pub fn new_from_array<K: Eq + Hash + Clone, V: Clone>(a: Array<LrcPtr<(K, V)>>) -> HashMap<K, V> {
        let it = a.iter().map(|pair| pair.as_ref().clone());
        HashMap(mkRefMut(collections::HashMap::from_iter(it)))
    }

    pub fn isReadOnly<K: Clone, V: Clone>(dict: HashMap<K, V>) -> bool {
        false
    }

    pub fn count<K: Clone, V: Clone>(dict: HashMap<K, V>) -> i32 {
        dict.len() as i32
    }

    pub fn containsKey<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K) -> bool {
        dict.contains_key(&k)
    }

    pub fn containsValue<K: Eq + Hash + Clone, V: Clone + PartialEq>(dict: HashMap<K, V>, v: V) -> bool {
        dict.values().any(|x| x.eq(&v))
    }

    pub fn tryAdd<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K, v: V) -> bool {
        // dict.get_mut().try_insert(k, v).is_ok() // nightly only
        if dict.contains_key(&k) {
            false
        } else {
            dict.get_mut().insert(k, v).is_none()
        }
    }

    pub fn add<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K, v: V) {
        match dict.get_mut().insert(k, v) {
            Some(v) => {
                panic!("An item with the same key has already been added.")
            }
            None => (),
        }
    }

    pub fn remove<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K) -> bool {
        dict.get_mut().remove(&k).is_some()
    }

    pub fn clear<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>) {
        dict.get_mut().clear();
    }

    pub fn get<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K) -> V {
        match dict.get_mut().get(&k) {
            Some(v) => v.clone(),
            None => {
                panic!("The given key was not present in the dictionary.")
            }
        }
    }

    pub fn set<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K, v: V) {
        dict.get_mut().insert(k, v); // ignore return value
    }

    pub fn tryGetValue<K: Eq + Hash + Clone, V: Clone>(
        dict: HashMap<K, V>,
        k: K,
        res: &MutCell<V>,
    ) -> bool {
        match dict.get_mut().get(&k) {
            Some(v) => {
                res.set(v.clone());
                true
            }
            None => false,
        }
    }

    pub fn keys<K: Clone, V: Clone>(dict: HashMap<K, V>) -> Array<K> {
        array_from(Vec::from_iter(dict.keys().cloned()))
    }

    pub fn values<K: Clone, V: Clone>(dict: HashMap<K, V>) -> Array<V> {
        array_from(Vec::from_iter(dict.values().cloned()))
    }

    pub fn entries<K: Clone, V: Clone>(dict: HashMap<K, V>) -> Array<(K, V)> {
        array_from(Vec::from_iter(
            dict.iter().map(|(k, v)| (k.clone(), v.clone())),
        ))
    }
}
