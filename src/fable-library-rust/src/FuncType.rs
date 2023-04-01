use crate::Native_::Lrc;

macro_rules! func {
    ($f:ident $(,$i:ident)*) => {

        #[cfg(not(feature = "enum_func"))]
        #[derive(Clone)]
        #[repr(transparent)]
        pub struct $f<$($i, )*R>(Lrc<dyn Fn($($i), *) -> R>);

        #[cfg(feature = "enum_func")]
        #[derive(Clone)]
        pub enum $f<$($i, )*R> {
            Static(fn($($i), *) -> R),
            Shared(Lrc<dyn Fn($($i), *) -> R>),
        }

        impl<$($i, )*R> core::fmt::Debug for $f<$($i, )*R> {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{}", core::any::type_name::<Self>())
            }
        }

        impl<$($i, )*R> core::fmt::Display for $f<$($i, )*R> {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{}", core::any::type_name::<Self>())
            }
        }

        #[cfg(not(feature = "enum_func"))]
        impl<$($i, )*R> core::ops::Deref for $f<$($i, )*R>
        where
            $($i: 'static, )*
            R: 'static,
        {
            type Target = dyn Fn($($i), *) -> R;
            fn deref(&self) -> &Self::Target {
                self.0.deref()
            }
        }

        #[cfg(feature = "enum_func")]
        impl<$($i, )*R> core::ops::Deref for $f<$($i, )*R>
        where
            $($i: 'static, )*
            R: 'static,
        {
            type Target = dyn Fn($($i), *) -> R;
            fn deref(&self) -> &Self::Target {
                match self {
                    $f::Static(f) => f,
                    $f::Shared(p) => p.as_ref(),
                }
            }
        }

        #[cfg(not(feature = "enum_func"))]
        impl<$($i, )*R> $f<$($i, )*R>
        where
            $($i: 'static, )*
            R: 'static,
        {
            pub fn from(f: fn($($i), *) -> R) -> Self {
                $f(Lrc::new(f))
            }
            pub fn new<F: Fn($($i), *) -> R + 'static>(f: F) -> Self {
                $f(Lrc::new(f))
            }
        }

        #[cfg(feature = "enum_func")]
        impl<$($i, )*R> $f<$($i, )*R>
        where
            $($i: 'static, )*
            R: 'static,
        {
            pub fn from(f: fn($($i), *) -> R) -> Self {
                $f::Static(f)
            }
            pub fn new<F: Fn($($i), *) -> R + 'static>(f: F) -> Self {
                $f::Shared(Lrc::new(f))
            }
        }

        #[cfg(feature = "threaded")]
        unsafe impl<$($i, )*R> Send for $f<$($i, )*R> {}

        #[cfg(feature = "threaded")]
        unsafe impl<$($i, )*R> Sync for $f<$($i, )*R> {}

        impl<$($i, )*R> core::panic::UnwindSafe for $f<$($i, )*R> {}
        impl<$($i, )*R> core::panic::RefUnwindSafe for $f<$($i, )*R> {}

    };
}

func!(Func0);
func!(Func1, T1);
func!(Func2, T1, T2);
func!(Func3, T1, T2, T3);
func!(Func4, T1, T2, T3, T4);
func!(Func5, T1, T2, T3, T4, T5);
func!(Func6, T1, T2, T3, T4, T5, T6);
func!(Func7, T1, T2, T3, T4, T5, T6, T7);
func!(Func8, T1, T2, T3, T4, T5, T6, T7, T8);
func!(Func9, T1, T2, T3, T4, T5, T6, T7, T8, T9);
func!(Func10, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
func!(Func11, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
func!(Func12, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
