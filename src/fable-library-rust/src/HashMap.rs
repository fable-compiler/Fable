pub mod HashMap_ {

    // -----------------------------------------------------------
    // HashMaps
    // -----------------------------------------------------------

    #[cfg(not(feature = "no_std"))]
    use std::collections;
    #[cfg(feature = "no_std")]
    use hashbrown as collections;

    use crate::Native_::{arrayFrom, mkRefMut, Array, Lrc, MutCell, Vec};
    type MutHashMap<K, V> = MutCell<collections::HashMap<K, V>>;

    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::Hash;

    #[repr(transparent)]
    #[derive(Clone, Debug, Default)] //, PartialEq, PartialOrd, Eq, Hash, Ord)]
    pub struct HashMap<K: Clone, V: Clone>(Lrc<MutHashMap<K, V>>);

    impl<K: Clone, V: Clone> core::ops::Deref for HashMap<K, V> {
        type Target = MutHashMap<K, V>;
        fn deref(&self) -> &Self::Target {
            &self.0.as_ref()
        }
    }

    impl<K: Clone + Debug, V: Clone + Debug> Display for HashMap<K, V> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.0) //TODO:
        }
    }

    pub fn empty<K: Clone, V: Clone>() -> HashMap<K, V> {
        HashMap(mkRefMut(collections::HashMap::new()))
    }

    pub fn withCapacity<K: Clone, V: Clone>(capacity: i32) -> HashMap<K, V> {
        HashMap(mkRefMut(collections::HashMap::with_capacity(capacity as usize)))
    }

    pub fn fromArray<K: Eq + Hash + Clone, V: Clone>(a: Array<Lrc<(K, V)>>) -> HashMap<K, V> {
        let it = a.iter().map(|pair| pair.as_ref().clone());
        HashMap(mkRefMut(collections::HashMap::from_iter(it)))
    }

    pub fn tryAdd<K: Eq + Hash + Clone, V: Clone>(
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

    pub fn add<K: Eq + Hash + Clone, V: Clone>(dict: HashMap<K, V>, k: K, v: V) {
        match dict.get_mut().insert(k.clone(), v.clone()) {
            Some(v) => {
                panic!("An item with the same key has already been added.")
            }
            None => (),
        }
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
        dict.get_mut().insert(k.clone(), v.clone()); // ignore return value
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
        arrayFrom(Vec::from_iter(dict.keys().cloned()))
    }

    pub fn values<K: Clone, V: Clone>(dict: HashMap<K, V>) -> Array<V> {
        arrayFrom(Vec::from_iter(dict.values().cloned()))
    }

    pub fn entries<K: Clone, V: Clone>(dict: HashMap<K, V>) -> Array<(K, V)> {
        arrayFrom(Vec::from_iter(
            dict.iter().map(|(k, v)| (k.clone(), v.clone())),
        ))
    }

}
