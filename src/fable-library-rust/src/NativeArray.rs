pub mod NativeArray_ {
    use crate::Native_::{alloc, mkRefMut, Lrc, LrcPtr, MutCell, Vec};

    // -----------------------------------------------------------
    // Arrays
    // -----------------------------------------------------------

    type MutArray<T> = MutCell<Vec<T>>;

    #[repr(transparent)]
    #[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord)]
    pub struct Array<T: Clone>(Lrc<MutArray<T>>);

    impl<T: Clone> core::ops::Deref for Array<T> {
        type Target = Lrc<MutArray<T>>;
        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<T: Clone + core::fmt::Debug> core::fmt::Display for Array<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{:?}", self.0) //TODO: improve
        }
    }

    impl<T: Clone> From<Vec<T>> for Array<T> {
        fn from(v: Vec<T>) -> Self {
            array_from(v)
        }
    }

    impl<T: Clone> From<&Vec<T>> for Array<T> {
        fn from(v: &Vec<T>) -> Self {
            let v2: Vec<T> = v.iter().map(|item| item.clone()).collect();
            array_from(v2)
        }
    }

    impl<T: Clone> Into<Vec<T>> for Array<T> {
        fn into(self) -> Vec<T> {
            self.get().iter().map(|item| item.clone()).collect()
        }
    }

    pub fn array_from<T: Clone>(v: Vec<T>) -> Array<T> {
        Array(mkRefMut(v))
    }

    pub fn new_array<T: Clone>(a: &[T]) -> Array<T> {
        array_from(a.to_vec())
    }

    pub fn new_empty<T: Clone>() -> Array<T> {
        array_from(Vec::new())
    }

    pub fn new_with_capacity<T: Clone>(capacity: i32) -> Array<T> {
        array_from(Vec::with_capacity(capacity as usize))
    }

    pub fn new_init<T: Clone>(value: &T, count: i32) -> Array<T> {
        array_from(alloc::vec![value.clone(); count as usize])
    }

    pub fn new_copy<T: Clone>(a: Array<T>) -> Array<T> {
        array_from(a.to_vec())
    }

    pub fn isReadOnly<T: Clone>(a: Array<T>) -> bool {
        false
    }

    pub fn count<T: Clone>(a: Array<T>) -> i32 {
        a.len() as i32
    }

    pub fn contains<T: PartialEq + Clone>(a: Array<T>, v: T) -> bool {
        a.contains(&v)
    }

    pub fn add<T: Clone>(a: Array<T>, v: T) {
        a.get_mut().push(v);
    }

    pub fn remove<T: PartialEq + Clone>(a: Array<T>, pos: i32) -> bool {
        a.get_mut().remove(pos as usize); true
    }

    pub fn clear<T: Clone>(a: Array<T>) {
        a.get_mut().clear();
    }
}
