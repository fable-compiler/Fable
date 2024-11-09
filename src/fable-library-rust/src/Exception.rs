pub mod Exception_ {
    use crate::Native_::{Any, Box, Func0, LrcPtr};
    use crate::String_::{fromSlice, string};
    use crate::System::Exception;
    use crate::Util_::new_Exception;

    #[cfg(feature = "no_std")]
    pub fn try_catch<F, G, R>(try_f: F, catch_f: G) -> R
    where
        F: FnOnce() -> R,
        G: FnOnce(LrcPtr<Exception>) -> R,
    {
        try_f() // no catching when no_std
    }

    #[cfg(not(feature = "no_std"))]
    pub fn try_catch<F, G, R>(try_f: F, catch_f: G) -> R
    where
        F: FnOnce() -> R,
        G: FnOnce(LrcPtr<Exception>) -> R,
    {
        fn get_ex(err: Box<dyn Any>) -> LrcPtr<Exception> {
            match err.downcast_ref::<&'static str>() {
                Some(s) => new_Exception(string(*s)),
                None => match err.downcast_ref::<String>() {
                    Some(s) => new_Exception(fromSlice(s)),
                    None => match err.downcast_ref::<LrcPtr<Exception>>() {
                        Some(ex) => ex.clone(),
                        None => new_Exception(string("Unknown error")),
                    },
                },
            }
        }
        let prev_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(try_f));
        std::panic::set_hook(prev_hook);
        match result {
            Ok(res) => res,
            Err(err) => catch_f(get_ex(err)),
        }
    }

    pub struct finally<R: 'static>(pub Func0<R>);

    impl<R: 'static> Drop for finally<R> {
        fn drop(&mut self) {
            (self.0)();
        }
    }
}
