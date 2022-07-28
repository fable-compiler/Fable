pub mod ExtInteropTests {
    pub mod ListTests {
        use fable_library_rust::{List, List_::{singleton, cons}};
        #[test]
        pub fn can_interop_between_list_and_vec() {
            //todo
            let raw = vec![1, 2, 3];
            let lst = List::from_vec(&vec![1, 2, 3]);
            let tgt = List::into_vec(lst.clone());

            assert_eq!(lst, cons(1, cons(2, singleton(3))));
            assert_eq!(raw, tgt);
        }
    }

    pub mod ArrayTests {
        use fable_library_rust::{Array, Array_::{}};
        #[test]
        pub fn can_interop_between_list_and_vec() {
            //todo
            let raw = vec![1, 2, 3];
            let arr = Array::from_vec(&vec![1, 2, 3]);
            let tgt = Array::into_vec(arr.clone());

            assert_eq!(raw, tgt);
        }
    }
}
