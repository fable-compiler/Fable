pub mod ExtInteropTests {
    pub mod ListTests {
        use fable_library_rust::{
            List_::{cons, singleton},
            Native_::List_1,
        };
        #[test]
        pub fn can_interop_between_list_and_vec() {
            //todo
            let raw = vec![1, 2, 3];
            let lst = List_1::from(&vec![1, 2, 3]);
            let tgt: Vec<i32> = lst.clone().into();

            let expectedLst = List_1::from(cons(1, cons(2, singleton(3))));
            assert_eq!(lst, expectedLst);
            assert_eq!(raw, tgt);
        }
    }

    pub mod ArrayTests {
        //Work in progress - Array needs a built in wrapper as first class citizen before this can be fleshed out
        use fable_library_rust::ArrayExt::Array;
        #[test]
        pub fn can_interop_between_array_and_vec() {
            //todo
            let raw = vec![1, 2, 3];
            let arr = Array::from(&vec![1, 2, 3]);
            let tgt: Vec<i32> = arr.clone().into();

            assert_eq!(raw, tgt);
        }
    }

    pub mod SetTests {
        use fable_library_rust::{
            Set_::{Set_1},
            Native_::List_1,
        };
        #[test]
        pub fn can_interop_between_set_and_vec() {
            //todo
            let raw = vec![1, 2, 3];
            let arr = Set_1::from(&vec![1, 2, 3]);
            let tgt: Vec<i32> = arr.clone().into();

            assert_eq!(raw, tgt);
        }
    }
}
