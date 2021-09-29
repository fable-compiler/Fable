pub mod Option {
    use std::rc::Rc;

    pub fn get<T: Clone>(opt: &Rc<Option<T>>) -> T {
        match (*opt).as_ref() {
            Some(x) => x.clone(),
            None => panic!("Option has no value")
        }
    }

    pub fn map<T: Clone, U: Clone>(opt: &Rc<Option<T>>, mapping: &impl Fn(T) -> U) -> Rc<Option<U>> {
        match (*opt).as_ref() {
            Some(x) => Rc::from(Some(mapping(x.clone()))),
            None => Rc::from(None)
        }
    }

}