pub mod ExtInteropTests {
    pub mod ListTests {
        use fable_library_rust::{
            List::*,
            List_::{cons, singleton},
        };
        #[test]
        pub fn can_interop_between_list_and_vec() {
            //todo
            let raw = vec![1, 2, 3];
            let lst = List::from(&vec![1, 2, 3]);
            let tgt:Vec<i32> = lst.clone().into();

            let expectedLst = List::from(cons(1, cons(2, singleton(3))));
            assert_eq!(lst, expectedLst );
            assert_eq!(raw, tgt);
        }
    }

    pub mod ArrayTests {
        use fable_library_rust::{ Array};
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
