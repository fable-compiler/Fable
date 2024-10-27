pub mod NativeArray_ {
    use crate::Global_::SR::indexOutOfBounds;
    use crate::Native_::{alloc, make_compare, mkRefMut, partial_compare, seq_to_iter};
    use crate::Native_::{Func1, Func2, LrcPtr, MutCell, Seq, Vec};
    use crate::System::Collections::Generic::IComparer_1;

    // -----------------------------------------------------------
    // Arrays
    // -----------------------------------------------------------

    type MutArray<T> = MutCell<Vec<T>>;

    #[derive(Clone, Debug, Default, Eq, Hash, PartialEq, PartialOrd, Ord)]
    #[repr(transparent)]
    pub struct Array<T: Clone>(LrcPtr<MutArray<T>>);

    impl<T: Clone> core::ops::Deref for Array<T> {
        type Target = LrcPtr<MutArray<T>>;
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
            self.get().iter().cloned().collect()
        }
    }

    pub fn array_from<T: Clone>(v: Vec<T>) -> Array<T> {
        Array(mkRefMut(v))
    }

    pub fn new_array<T: Clone>(a: &[T]) -> Array<T> {
        array_from(a.to_vec())
    }

    pub fn new_init<T: Clone>(value: &T, count: i32) -> Array<T> {
        array_from(alloc::vec![value.clone(); count as usize])
    }

    pub fn new_empty<T: Clone>() -> Array<T> {
        array_from(Vec::new())
    }

    pub fn new_with_capacity<T: Clone>(capacity: i32) -> Array<T> {
        array_from(Vec::with_capacity(capacity as usize))
    }

    pub fn new_from_enumerable<T: Clone + 'static>(seq: Seq<T>) -> Array<T> {
        array_from(Vec::from_iter(seq_to_iter(&seq)))
    }

    pub fn get_Capacity<T: Clone>(a: Array<T>) -> i32 {
        a.capacity() as i32
    }

    pub fn get_Count<T: Clone>(a: Array<T>) -> i32 {
        a.len() as i32
    }

    pub fn isReadOnly<T: Clone>(a: Array<T>) -> bool {
        false
    }

    pub fn add<T: Clone>(a: Array<T>, v: T) {
        a.get_mut().push(v);
    }

    pub fn addRange<T: Clone + 'static>(a: Array<T>, seq: Seq<T>) {
        let range = a.len()..a.len();
        a.get_mut().splice(range, seq_to_iter(&seq));
    }

    pub fn copyTo<T: Clone>(src: Array<T>, dest: Array<T>) {
        copyTo2(src, dest, 0)
    }

    pub fn copyTo2<T: Clone>(src: Array<T>, dest: Array<T>, destIndex: i32) {
        let count = src.len() as i32;
        copyTo4(src, 0, dest, destIndex, count)
    }

    pub fn copyTo4<T: Clone>(
        src: Array<T>,
        srcIndex: i32,
        dest: Array<T>,
        destIndex: i32,
        count: i32,
    ) {
        let check1 = srcIndex + count > src.len() as i32;
        let check2 = destIndex + count > dest.len() as i32;
        if srcIndex < 0 || destIndex < 0 || count < 0 || check1 || check2 {
            panic!("{}", indexOutOfBounds());
        }
        let rangeFrom = srcIndex as usize..(srcIndex + count) as usize;
        let rangeTo = destIndex as usize..(destIndex + count) as usize;
        dest.get_mut()[rangeTo].clone_from_slice(&src.as_slice()[rangeFrom]);
    }

    pub fn contains<T: Clone + PartialEq>(a: Array<T>, v: T) -> bool {
        a.contains(&v)
    }

    pub fn clear<T: Clone>(a: Array<T>) {
        a.get_mut().clear();
    }

    pub fn convertAll<T: Clone + 'static, U: Clone + 'static>(
        a: Array<T>,
        converter: Func1<T, U>,
    ) -> Array<U> {
        let it = a.iter().map(|x| converter(x.clone()));
        array_from(it.collect())
    }

    pub fn binarySearch<T: Clone + PartialOrd>(a: Array<T>, v: T) -> i32 {
        let res = a.binary_search_by(|x| partial_compare(x, &v));
        match res {
            Ok(i) => i as i32,
            Err(i) => !i as i32,
        }
    }

    pub fn binarySearch2<T: Clone + 'static>(
        a: Array<T>,
        v: T,
        comparer: LrcPtr<dyn IComparer_1<T>>,
    ) -> i32 {
        let cmp = make_compare(Func2::new(move |x: T, y: T| comparer.Compare(x, y)));
        let res = a.binary_search_by(|x| cmp(x, &v));
        match res {
            Ok(i) => i as i32,
            Err(i) => !i as i32,
        }
    }

    pub fn binarySearch4<T: Clone + 'static>(
        a: Array<T>,
        index: i32,
        count: i32,
        v: T,
        comparer: LrcPtr<dyn IComparer_1<T>>,
    ) -> i32 {
        let len = a.len() as i32;
        if index < 0 || index >= len || count < 0 || index + count > len {
            panic!("{}", indexOutOfBounds());
        }
        let range = index as usize..(index + count) as usize;
        let cmp = make_compare(Func2::new(move |x: T, y: T| comparer.Compare(x, y)));
        let res = a.as_slice()[range].binary_search_by(|x| cmp(x, &v));
        match res {
            Ok(i) => (index as usize + i) as i32,
            Err(i) => !(index as usize + i) as i32,
        }
    }

    pub fn exists<T: Clone + 'static>(a: Array<T>, predicate: Func1<T, bool>) -> bool {
        a.iter().find(|&x| predicate(x.clone())).is_some()
    }

    pub fn getRange<T: Clone>(a: Array<T>, index: i32, count: i32) -> Array<T> {
        let len = a.len() as i32;
        if index < 0 || index >= len || count < 0 || index + count > len {
            panic!("{}", indexOutOfBounds());
        }
        let range = index as usize..(index + count) as usize;
        array_from(a.as_slice()[range].to_vec())
    }

    pub fn forEach<T: Clone + 'static>(a: Array<T>, action: Func1<T, ()>) {
        a.iter().for_each(|x| action(x.clone()));
    }

    pub fn findAll<T: Clone + 'static>(a: Array<T>, predicate: Func1<T, bool>) -> Array<T> {
        let it = a.iter().filter(|&x| predicate(x.clone()));
        array_from(it.cloned().collect())
    }

    pub fn find<T: Clone + Default + 'static>(a: Array<T>, predicate: Func1<T, bool>) -> T {
        let pos = a.iter().position(|x| predicate(x.clone()));
        match pos {
            Some(index) => a[index as i32].clone(),
            None => T::default(),
        }
    }

    pub fn findLast<T: Clone + Default + 'static>(a: Array<T>, predicate: Func1<T, bool>) -> T {
        let pos = a.iter().rposition(|x| predicate(x.clone()));
        match pos {
            Some(index) => a[index as i32].clone(),
            None => T::default(),
        }
    }

    pub fn findIndex<T: Clone + 'static>(a: Array<T>, predicate: Func1<T, bool>) -> i32 {
        let mut it = a.iter();
        unwrap_pos(it.position(|x| predicate(x.clone())))
    }

    pub fn findLastIndex<T: Clone + 'static>(a: Array<T>, predicate: Func1<T, bool>) -> i32 {
        let mut it = a.iter();
        unwrap_pos(it.rposition(|x| predicate(x.clone())))
    }

    fn unwrap_pos(pos: Option<usize>) -> i32 {
        match pos {
            Some(index) => index as i32,
            None => -1,
        }
    }

    pub fn indexOf<T: Clone + PartialEq>(a: Array<T>, v: T) -> i32 {
        let mut it = a.iter();
        unwrap_pos(it.position(|x| x.eq(&v)))
    }

    pub fn indexOf2<T: Clone + PartialEq>(a: Array<T>, v: T, index: i32) -> i32 {
        let len = a.len() as i32;
        if index < 0 || index >= len {
            panic!("{}", indexOutOfBounds());
        }
        let mut it = a.iter().skip(index as usize);
        unwrap_pos(it.position(|x| x.eq(&v)).map(|i| i + index as usize))
    }

    pub fn indexOf3<T: Clone + PartialEq>(a: Array<T>, v: T, index: i32, count: i32) -> i32 {
        let len = a.len() as i32;
        if index < 0 || index >= len || count < 0 || index + count > len {
            panic!("{}", indexOutOfBounds());
        }
        let mut it = a.iter().skip(index as usize).take(count as usize);
        unwrap_pos(it.position(|x| x.eq(&v)).map(|i| i + index as usize))
    }

    pub fn lastIndexOf<T: Clone + PartialEq>(a: Array<T>, v: T) -> i32 {
        let mut it = a.iter();
        unwrap_pos(it.rposition(|x| x.eq(&v)))
    }

    pub fn lastIndexOf2<T: Clone + PartialEq>(a: Array<T>, v: T, index: i32) -> i32 {
        lastIndexOf3(a, v, index, index + 1)
    }

    pub fn lastIndexOf3<T: Clone + PartialEq>(a: Array<T>, v: T, index: i32, count: i32) -> i32 {
        let len = a.len() as i32;
        if index < 0 || index >= len || count < 0 || index + 1 < count {
            panic!("{}", indexOutOfBounds());
        }
        let start = index as usize + 1 - count as usize;
        let mut it = a.iter().skip(start).take(count as usize);
        unwrap_pos(it.rposition(|x| x.eq(&v)).map(|i| i + start))
    }

    pub fn insert<T: Clone>(a: Array<T>, pos: i32, v: T) {
        a.get_mut().insert(pos as usize, v);
    }

    pub fn insertRange<T: Clone + 'static>(a: Array<T>, index: i32, seq: Seq<T>) {
        let len = a.len() as i32;
        if index < 0 || index >= len {
            panic!("{}", indexOutOfBounds());
        }
        let range = index as usize..index as usize;
        a.get_mut().splice(range, seq_to_iter(&seq));
    }

    pub fn remove<T: Clone + PartialEq>(a: Array<T>, v: T) -> bool {
        let pos = a.iter().position(|x| x.eq(&v));
        match pos {
            Some(index) => {
                a.get_mut().remove(index);
                true
            }
            None => false,
        }
    }

    pub fn removeAt<T: Clone>(a: Array<T>, index: i32) {
        let len = a.len() as i32;
        if index < 0 || index >= len {
            panic!("{}", indexOutOfBounds());
        }
        a.get_mut().remove(index as usize);
    }

    pub fn removeRange<T: Clone>(a: Array<T>, index: i32, count: i32) {
        let len = a.len() as i32;
        if index < 0 || index >= len || count < 0 || index + count > len {
            panic!("{}", indexOutOfBounds());
        }
        let range = index as usize..(index + count) as usize;
        a.get_mut().splice(range, core::iter::empty::<T>());
    }

    pub fn removeAll<T: Clone + 'static>(a: Array<T>, predicate: Func1<T, bool>) -> i32 {
        let old_len = a.len();
        a.get_mut().retain(|x| !predicate(x.clone()));
        (old_len - a.len()) as i32
    }

    pub fn reverse<T: Clone>(a: Array<T>) {
        a.get_mut().reverse();
    }

    pub fn reverse2<T: Clone>(a: Array<T>, index: i32, count: i32) {
        let len = a.len() as i32;
        if index < 0 || index >= len || count < 0 || index + count > len {
            panic!("{}", indexOutOfBounds());
        }
        let range = index as usize..(index + count) as usize;
        a.get_mut()[range].reverse();
    }

    pub fn slice<T: Clone>(a: Array<T>, index: i32, count: i32) -> Array<T> {
        getRange(a, index, count)
    }

    pub fn sort<T: Clone + PartialOrd>(a: Array<T>) {
        a.get_mut().sort_by(partial_compare);
    }

    pub fn sortBy<T: Clone + 'static>(a: Array<T>, comparer: Func2<T, T, i32>) {
        a.get_mut().sort_by(make_compare(comparer));
    }

    pub fn sortWith<T: Clone + 'static>(a: Array<T>, comparer: LrcPtr<dyn IComparer_1<T>>) {
        let cmp = Func2::new(move |x: T, y: T| comparer.Compare(x, y));
        a.get_mut().sort_by(make_compare(cmp));
    }

    pub fn sortWith2<T: Clone + 'static>(
        a: Array<T>,
        index: i32,
        count: i32,
        comparer: LrcPtr<dyn IComparer_1<T>>,
    ) {
        let len = a.len() as i32;
        if index < 0 || index >= len || count < 0 || index + count > len {
            panic!("{}", indexOutOfBounds());
        }
        let range = index as usize..(index + count) as usize;
        let cmp = Func2::new(move |x: T, y: T| comparer.Compare(x, y));
        a.get_mut()[range].sort_by(make_compare(cmp));
    }

    pub fn toArray<T: Clone>(a: Array<T>) -> Array<T> {
        array_from(a.to_vec())
    }

    pub fn trimExcess<T: Clone>(a: Array<T>) {
        a.get_mut().shrink_to_fit();
    }

    pub fn trueForAll<T: Clone + 'static>(a: Array<T>, predicate: Func1<T, bool>) -> bool {
        a.iter().all(|x| predicate(x.clone()))
    }
}
