pub mod Option {
    use std::rc::Rc;

    pub fn getValue<T: Clone>(opt: &Rc<Option<T>>) -> T {
        match (*opt).as_ref() {
            Some(x) => x.clone(),
            None => panic!("Option has no value")
        }
    }

    pub fn bind<T: Clone, U: Clone>(binder: &impl Fn(&T) -> Rc<Option<U>>, opt: &Rc<Option<T>>) -> Rc<Option<U>> {
        match (*opt).as_ref() {
            Some(x) => Rc::from(binder(x)),
            None => Rc::from(None)
        }
    }

    pub fn map<T: Clone, U: Clone>(mapping: &impl Fn(&T) -> U, opt: &Rc<Option<T>>) -> Rc<Option<U>> {
        match (*opt).as_ref() {
            Some(x) => Rc::from(Some(mapping(x))),
            None => Rc::from(None)
        }
    }
}
