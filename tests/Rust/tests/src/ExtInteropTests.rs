pub mod ExtInteropTests {
    pub mod ListTests {
        use fable_library_rust::List_::{cons, singleton, List};
        use fable_library_rust::String_::string;

        #[test]
        pub fn can_interop_between_list_and_vec() {
            let raw = vec![1, 2, 3];
            let lst = List::from(&raw);
            let tgt: Vec<i32> = lst.clone().into();

            let expectedLst = cons(1, cons(2, singleton(3)));
            assert_eq!(lst, expectedLst);
            assert_eq!(raw, tgt);
        }

        #[test]
        pub fn can_iter() {
            let lst = List::from(&vec![1, 2, 3]);
            let res: Vec<i32> = lst.into_iter().map(|x| x + 1).collect();
            assert_eq!(res, vec![2, 3, 4]);
        }

        #[test]
        pub fn can_collect_while_consuming() {
            let raw = vec![1, 2, 3];
            let expected = List::from(&raw);
            let res: List<i32> = raw.into_iter().collect();
            assert_eq!(res, expected);
        }

        #[test]
        pub fn can_collect() {
            let raw = vec![1, 2, 3];
            let expected = List::from(&raw);
            let res: List<i32> = raw.iter().collect();
            assert_eq!(res, expected);
        }
    }

    pub mod ArrayTests {
        //Work in progress - Array needs a built in wrapper as first class citizen before this can be fleshed out
        use fable_library_rust::NativeArray_::Array;

        #[test]
        pub fn can_interop_between_array_and_vec() {
            //todo
            let raw = vec![1, 2, 3];
            let arr = Array::from(&raw);
            let tgt: Vec<i32> = arr.clone().into();
            assert_eq!(raw, tgt);
        }
    }

    pub mod SetTests {
        use fable_library_rust::Set_::Set;

        #[test]
        pub fn can_interop_between_set_and_vec() {
            //todo
            let raw = vec![1, 2, 3];
            let set = Set::from(&raw);
            let tgt: Vec<i32> = set.clone().into();
            assert_eq!(raw, tgt);
        }

        #[test]
        pub fn can_collect_while_consuming() {
            let raw = vec![1, 2, 3];
            let expected = Set::from(&raw);
            let res: Set<i32> = raw.into_iter().collect();
            assert_eq!(res, expected);
        }
    }

    pub mod MapTests {
        use fable_library_rust::Map_::Map;
        use fable_library_rust::String_::string;

        #[test]
        pub fn can_interop_between_map_and_vec() {
            //todo
            let raw = vec![(string("a"), 1), (string("b"), 2), (string("c"), 3)];
            let map = Map::from(&raw);
            let tgt: Vec<(string, i32)> = map.clone().into();
            assert_eq!(raw, tgt);
        }

        #[test]
        pub fn can_iter() {
            let raw = vec![(string("a"), 1), (string("b"), 2), (string("c"), 3)];
            let map = Map::from(&raw);
            let res: Vec<i32> = map.into_iter().map(|(a, b)| b + 1).collect();
            assert_eq!(res, vec![2, 3, 4]);
        }

        #[test]
        pub fn can_collect_while_consuming() {
            let raw = vec![(string("a"), 1), (string("b"), 2), (string("c"), 3)];
            let expected: Map<string, i32> = Map::from(&raw);
            let res: Map<string, i32> = raw.into_iter().collect();
            assert_eq!(res, expected);
        }
    }
}
