pub mod Seq {
    use std::rc::Rc;
    use crate::MutCell;
    use crate::Native;
    use crate::System::Collections::Generic::*;

    pub fn as_iter<T>(en: &Rc<dyn IEnumerator_1<T>>) -> impl Iterator<Item=T> {
        let en = en.clone();
        let next = move || if en.MoveNext() { Some(en.get_Current()) } else { None };
        std::iter::from_fn(next)
    }

    pub struct ArrayEnumerator<T> {
        pos: MutCell<i32>,
        arr: Rc<[MutCell<T>]>,
    }

    impl<T> ArrayEnumerator<T> {
        pub fn new(arr: &Rc<[MutCell<T>]>) -> Self {
            ArrayEnumerator {
                pos: MutCell::from(-1),
                arr: arr.clone(),
            }
        }
    }

    impl<T: Clone> IEnumerator_1<T> for ArrayEnumerator<T> {
        fn get_Current(&self) -> T {
            let i = self.pos.get();
            if i >= 0 && (i as usize) < self.arr.len() {
                self.arr[i as usize].get()
            } else {
                Native::defaultOf()
            }
        }

        fn MoveNext(&self) -> bool {
            let i = self.pos.get();
            if ((i + 1) as usize) < self.arr.len() {
                self.pos.set(i + 1);
                true
            } else {
                false
            }
        }

        fn Reset(&self) -> () {
            self.pos.set(-1);
        }
    }

}