pub mod Exception_ {
    use crate::Native_::{Any, Box_, Lrc};
    use crate::String_::{fromSlice, string};
    use crate::System::Exception;
    use crate::Util_::new_exception;

    #[cfg(feature = "no_std")]
    pub fn try_catch<F, G, R>(f: F, g: G) -> R
    where
        F: FnOnce() -> R + core::panic::UnwindSafe,
        G: FnOnce(Lrc<Exception>) -> R,
    {
        f() // no catching when no_std
    }

    #[cfg(not(feature = "no_std"))]
    pub fn try_catch<F, G, R>(f: F, g: G) -> R
    where
        F: FnOnce() -> R + core::panic::UnwindSafe,
        G: FnOnce(Lrc<Exception>) -> R,
    {
        fn get_string(err: &Box<dyn Any + Send>) -> string {
            match err.downcast_ref::<&'static str>() {
                Some(s) => string(*s),
                None => match err.downcast_ref::<String>() {
                    Some(s) => fromSlice(s),
                    None => string("Unknown error"),
                },
            }
        }
        let prev_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        let result = std::panic::catch_unwind(f);
        std::panic::set_hook(prev_hook);
        match result {
            Ok(res) => res,
            Err(err) => g(new_exception(get_string(&err))),
        }
    }

    pub struct finally<F, R>(pub F)
    where
        F: FnMut() -> R;

    impl<F, R> Drop for finally<F, R>
    where
        F: FnMut() -> R,
    {
        fn drop(&mut self) {
            (self.0)();
        }
    }
}
