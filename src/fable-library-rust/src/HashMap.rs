pub mod HashMap_ {

    // -----------------------------------------------------------
    // HashMaps
    // -----------------------------------------------------------

    #[cfg(feature = "no_std")]
    use hashbrown as collections;
    #[cfg(not(feature = "no_std"))]
    use std::collections;

    use crate::NativeArray_::{array_from, Array};
    use crate::Native_::{default_eq_comparer, mkRefMut, seq_to_iter};
    use crate::Native_::{HashKey, LrcPtr, MutCell, Seq, Vec};
    use crate::System::Collections::Generic::IEqualityComparer_1;

    use core::fmt::{Debug, Display, Formatter, Result};
    use core::hash::{Hash, Hasher};

    type MutHashMap<K, V> = MutCell<collections::HashMap<HashKey<K>, V>>;

    #[derive(Clone)] //, Debug, Default, PartialEq, PartialOrd, Eq, Hash, Ord)]
    pub struct HashMap<K: Clone, V: Clone> {
        hash_map: LrcPtr<MutHashMap<K, V>>,
        comparer: LrcPtr<dyn IEqualityComparer_1<K>>,
    }

    // impl<K, V: Clone> Default for HashMap<K, V>
    // where
    //     K: Clone + Hash + PartialEq + 'static,
    // {
    //     fn default() -> HashMap<K, V> {
    //         new_empty()
    //     }
    // }

    impl<K: Clone, V: Clone> core::ops::Deref for HashMap<K, V> {
        type Target = LrcPtr<MutHashMap<K, V>>;
        fn deref(&self) -> &Self::Target {
            &self.hash_map
        }
    }

    impl<K: Clone + Debug, V: Clone + Debug> Debug for HashMap<K, V> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.hash_map) //TODO:
        }
    }

    impl<K: Clone + Debug, V: Clone + Debug> Display for HashMap<K, V> {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "{:?}", self.hash_map) //TODO:
        }
    }

    fn from_iter<K: Clone + 'static, V: Clone, I: Iterator<Item = (K, V)>>(
        iter: I,
        comparer: LrcPtr<dyn IEqualityComparer_1<K>>,
    ) -> HashMap<K, V> {
        let it = iter.map(|(k, v)| {
            let key = HashKey::new(k, comparer.clone());
            (key, v)
        });
        HashMap {
            hash_map: mkRefMut(collections::HashMap::from_iter(it)),
            comparer: comparer.clone(),
        }
    }

    fn to_iter<K: Clone, V: Clone>(map: &HashMap<K, V>) -> impl Iterator<Item = (K, V)> + '_ {
        map.iter().map(|(k, v)| (k.key.clone(), v.clone()))
    }

    pub fn new_empty<K, V: Clone>() -> HashMap<K, V>
    where
        K: Clone + Hash + PartialEq + 'static,
    {
        HashMap {
            hash_map: mkRefMut(collections::HashMap::new()),
            comparer: default_eq_comparer::<K>(),
        }
    }

    pub fn new_with_capacity<K, V: Clone>(capacity: i32) -> HashMap<K, V>
    where
        K: Clone + Hash + PartialEq + 'static,
    {
        HashMap {
            hash_map: mkRefMut(collections::HashMap::with_capacity(capacity as usize)),
            comparer: default_eq_comparer::<K>(),
        }
    }

    pub fn new_with_comparer<K: Clone, V: Clone>(
        comparer: LrcPtr<dyn IEqualityComparer_1<K>>,
    ) -> HashMap<K, V> {
        HashMap {
            hash_map: mkRefMut(collections::HashMap::new()),
            comparer,
        }
    }

    pub fn new_with_capacity_comparer<K: Clone, V: Clone>(
        capacity: i32,
        comparer: LrcPtr<dyn IEqualityComparer_1<K>>,
    ) -> HashMap<K, V> {
        HashMap {
            hash_map: mkRefMut(collections::HashMap::with_capacity(capacity as usize)),
            comparer,
        }
    }

    pub fn new_from_enumerable<K, V: Clone + 'static>(seq: Seq<(K, V)>) -> HashMap<K, V>
    where
        K: Clone + Hash + PartialEq + 'static,
    {
        from_iter(seq_to_iter(&seq), default_eq_comparer::<K>())
    }

    pub fn new_from_enumerable_comparer<K: Clone + 'static, V: Clone + 'static>(
        seq: Seq<(K, V)>,
        comparer: LrcPtr<dyn IEqualityComparer_1<K>>,
    ) -> HashMap<K, V> {
        from_iter(seq_to_iter(&seq), comparer)
    }

    pub fn new_from_dictionary<K: Clone + 'static, V: Clone>(map: HashMap<K, V>) -> HashMap<K, V> {
        from_iter(to_iter(&map), map.comparer.clone())
    }

    pub fn new_from_dictionary_comparer<K: Clone + 'static, V: Clone>(
        map: HashMap<K, V>,
        comparer: LrcPtr<dyn IEqualityComparer_1<K>>,
    ) -> HashMap<K, V> {
        from_iter(to_iter(&map), comparer)
    }

    pub fn new_from_tuple_array<K, V: Clone>(a: Array<LrcPtr<(K, V)>>) -> HashMap<K, V>
    where
        K: Clone + Hash + PartialEq + 'static,
    {
        let it = a.iter().map(|tup| tup.as_ref().clone());
        from_iter(it, default_eq_comparer::<K>())
    }

    pub fn isReadOnly<K: Clone, V: Clone>(map: HashMap<K, V>) -> bool {
        false
    }

    pub fn count<K: Clone, V: Clone>(map: HashMap<K, V>) -> i32 {
        map.len() as i32
    }

    pub fn containsKey<K: Clone + 'static, V: Clone>(map: HashMap<K, V>, k: K) -> bool {
        let key = HashKey::new(k, map.comparer.clone());
        map.contains_key(&key)
    }

    pub fn containsValue<K: Clone, V: Clone + PartialEq>(map: HashMap<K, V>, v: V) -> bool {
        map.values().any(|x| x.eq(&v))
    }

    pub fn tryAdd<K: Clone + 'static, V: Clone>(map: HashMap<K, V>, k: K, v: V) -> bool {
        let key = HashKey::new(k, map.comparer.clone());
        // map.get_mut().try_insert(key, v).is_ok() // nightly only
        if map.contains_key(&key) {
            false
        } else {
            map.get_mut().insert(key, v).is_none()
        }
    }

    pub fn add<K: Clone + 'static, V: Clone>(map: HashMap<K, V>, k: K, v: V) {
        let key = HashKey::new(k, map.comparer.clone());
        match map.get_mut().insert(key, v) {
            Some(v) => {
                panic!("An item with the same key has already been added.")
            }
            None => (),
        }
    }

    pub fn remove<K: Clone + 'static, V: Clone>(map: HashMap<K, V>, k: K) -> bool {
        let key = HashKey::new(k, map.comparer.clone());
        map.get_mut().remove(&key).is_some()
    }

    pub fn clear<K: Clone, V: Clone>(map: HashMap<K, V>) {
        map.get_mut().clear();
    }

    pub fn get<K: Clone + 'static, V: Clone>(map: HashMap<K, V>, k: K) -> V {
        let key = HashKey::new(k, map.comparer.clone());
        match map.get_mut().get(&key) {
            Some(v) => v.clone(),
            None => {
                panic!("The given key was not present in the dictionary.")
            }
        }
    }

    pub fn set<K: Clone + 'static, V: Clone>(map: HashMap<K, V>, k: K, v: V) {
        let key = HashKey::new(k, map.comparer.clone());
        map.get_mut().insert(key, v); // ignore return value
    }

    pub fn tryGetValue<K: Clone + 'static, V: Clone>(
        map: HashMap<K, V>,
        k: K,
        res: &MutCell<V>,
    ) -> bool {
        let key = HashKey::new(k, map.comparer.clone());
        match map.get_mut().get(&key) {
            Some(v) => {
                res.set(v.clone());
                true
            }
            None => false,
        }
    }

    pub fn keys<K: Clone, V: Clone>(map: HashMap<K, V>) -> Array<K> {
        array_from(Vec::from_iter(map.keys().map(|k| k.key.clone())))
    }

    pub fn values<K: Clone, V: Clone>(map: HashMap<K, V>) -> Array<V> {
        array_from(Vec::from_iter(map.values().cloned()))
    }

    pub fn entries<K: Clone, V: Clone>(map: HashMap<K, V>) -> Array<(K, V)> {
        array_from(Vec::from_iter(to_iter(&map)))
    }
}
