pub mod Observable_ {
    use crate::Choice_::Choice_2;
    use crate::Native_::{interface_cast, refCell, Func0, Func1, Func2, Lrc, LrcPtr};
    use crate::System::{Exception, IDisposable, IObservable_1, IObserver_1};

    // -----------------------------------------------------------
    // Observer
    // -----------------------------------------------------------

    #[derive(Clone)]
    pub struct Observer<T: Clone + 'static> {
        on_next: Func1<T, ()>,
        on_error: Func1<LrcPtr<Exception>, ()>,
        on_completed: Func0<()>,
    }

    impl<T: Clone + 'static> core::fmt::Debug for Observer<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl<T: Clone + 'static> core::fmt::Display for Observer<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl<T: Clone + 'static> IObserver_1<T> for Observer<T> {
        fn OnNext(&self, arg0: T) {
            (self.on_next)(arg0)
        }
        fn OnError(&self, arg0: LrcPtr<Exception>) {
            (self.on_error)(arg0)
        }
        fn OnCompleted(&self) {
            (self.on_completed)()
        }
    }

    // -----------------------------------------------------------
    // Observable
    // -----------------------------------------------------------

    #[derive(Clone)]
    pub struct Observable<T: Clone + 'static> {
        subscribe: Func1<LrcPtr<dyn IObserver_1<T>>, LrcPtr<dyn IDisposable>>,
    }

    impl<T: Clone + 'static> core::fmt::Debug for Observable<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl<T: Clone + 'static> core::fmt::Display for Observable<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl<T: Clone + 'static> IObservable_1<T> for Observable<T> {
        fn Subscribe(&self, arg0: LrcPtr<dyn IObserver_1<T>>) -> LrcPtr<dyn IDisposable> {
            (self.subscribe)(arg0)
        }
    }

    // -----------------------------------------------------------
    // Disposable
    // -----------------------------------------------------------

    #[derive(Clone)]
    pub struct Disposable {
        dispose: Func0<()>,
    }

    impl core::fmt::Debug for Disposable {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl core::fmt::Display for Disposable {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl IDisposable for Disposable {
        fn Dispose(&self) {
            (self.dispose)()
        }
    }

    // -----------------------------------------------------------
    // Constructors
    // -----------------------------------------------------------

    pub fn mkObserver<T: Clone + 'static>(
        on_next: Func1<T, ()>,
        on_error: Func1<LrcPtr<Exception>, ()>,
        on_completed: Func0<()>,
    ) -> LrcPtr<dyn IObserver_1<T>> {
        interface_cast!(
            LrcPtr::new(Observer {
                on_next,
                on_error,
                on_completed,
            }),
            Lrc<dyn IObserver_1<T>>,
        )
    }

    fn mkObservable<T: Clone + 'static>(
        subscribe: Func1<LrcPtr<dyn IObserver_1<T>>, LrcPtr<dyn IDisposable>>,
    ) -> LrcPtr<dyn IObservable_1<T>> {
        interface_cast!(
            LrcPtr::new(Observable { subscribe }),
            Lrc<dyn IObservable_1<T>>,
        )
    }

    pub fn mkDisposable(dispose: Func0<()>) -> LrcPtr<dyn IDisposable> {
        interface_cast!(LrcPtr::new(Disposable { dispose }), Lrc<dyn IDisposable>,)
    }

    // A no-op error handler (errors are propagated by combinators when present,
    // but the default subscriber ignores them, mirroring the JS/TS reference).
    pub fn noError() -> Func1<LrcPtr<Exception>, ()> {
        Func1::new(|_e: LrcPtr<Exception>| ())
    }

    pub fn noCompleted() -> Func0<()> {
        Func0::new(|| ())
    }

    // Build an error handler that forwards to a downstream observer.
    fn fwdError<T: Clone + 'static>(
        observer: LrcPtr<dyn IObserver_1<T>>,
    ) -> Func1<LrcPtr<Exception>, ()> {
        Func1::new(move |e: LrcPtr<Exception>| observer.OnError(e))
    }

    // Build a completion handler that forwards to a downstream observer.
    fn fwdCompleted<T: Clone + 'static>(observer: LrcPtr<dyn IObserver_1<T>>) -> Func0<()> {
        Func0::new(move || observer.OnCompleted())
    }

    // -----------------------------------------------------------
    // Combinators
    // -----------------------------------------------------------

    pub fn add<T: Clone + 'static>(callback: Func1<T, ()>, source: LrcPtr<dyn IObservable_1<T>>) {
        source.Subscribe(mkObserver(callback, noError(), noCompleted()));
    }

    pub fn subscribe<T: Clone + 'static>(
        callback: Func1<T, ()>,
        source: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<dyn IDisposable> {
        source.Subscribe(mkObserver(callback, noError(), noCompleted()))
    }

    pub fn choose<T: Clone + 'static, U: Clone + 'static>(
        chooser: Func1<T, Option<U>>,
        source: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<dyn IObservable_1<U>> {
        mkObservable(Func1::new(
            move |observer: LrcPtr<dyn IObserver_1<U>>| -> LrcPtr<dyn IDisposable> {
                let chooser = chooser.clone();
                let obs = observer.clone();
                let inner = mkObserver(
                    Func1::new(move |t: T| {
                        if let Some(u) = chooser(t) {
                            obs.OnNext(u);
                        }
                    }),
                    fwdError(observer.clone()),
                    fwdCompleted(observer.clone()),
                );
                source.Subscribe(inner)
            },
        ))
    }

    pub fn filter<T: Clone + 'static>(
        predicate: Func1<T, bool>,
        source: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<dyn IObservable_1<T>> {
        choose(
            Func1::new(move |x: T| if predicate(x.clone()) { Some(x) } else { None }),
            source,
        )
    }

    pub fn map<T: Clone + 'static, U: Clone + 'static>(
        mapping: Func1<T, U>,
        source: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<dyn IObservable_1<U>> {
        mkObservable(Func1::new(
            move |observer: LrcPtr<dyn IObserver_1<U>>| -> LrcPtr<dyn IDisposable> {
                let mapping = mapping.clone();
                let obs = observer.clone();
                let inner = mkObserver(
                    Func1::new(move |t: T| obs.OnNext(mapping(t))),
                    fwdError(observer.clone()),
                    fwdCompleted(observer.clone()),
                );
                source.Subscribe(inner)
            },
        ))
    }

    pub fn merge<T: Clone + 'static>(
        source1: LrcPtr<dyn IObservable_1<T>>,
        source2: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<dyn IObservable_1<T>> {
        mkObservable(Func1::new(
            move |observer: LrcPtr<dyn IObserver_1<T>>| -> LrcPtr<dyn IDisposable> {
                let stopped = refCell(false);
                let completed1 = refCell(false);
                let completed2 = refCell(false);

                let h1 = {
                    let stopped = stopped.clone();
                    let completed1 = completed1.clone();
                    let completed2 = completed2.clone();
                    let obs_n = observer.clone();
                    let obs_e = observer.clone();
                    let obs_c = observer.clone();
                    let se = stopped.clone();
                    let sc = stopped.clone();
                    source1.Subscribe(mkObserver(
                        Func1::new(move |v: T| {
                            if !*stopped.get() {
                                obs_n.OnNext(v);
                            }
                        }),
                        Func1::new(move |e: LrcPtr<Exception>| {
                            if !*se.get() {
                                se.set(true);
                                obs_e.OnError(e);
                            }
                        }),
                        Func0::new(move || {
                            if !*sc.get() {
                                completed1.set(true);
                                if *completed2.get() {
                                    sc.set(true);
                                    obs_c.OnCompleted();
                                }
                            }
                        }),
                    ))
                };

                let h2 = {
                    let stopped = stopped.clone();
                    let completed1 = completed1.clone();
                    let completed2 = completed2.clone();
                    let obs_n = observer.clone();
                    let obs_e = observer.clone();
                    let obs_c = observer.clone();
                    let se = stopped.clone();
                    let sc = stopped.clone();
                    source2.Subscribe(mkObserver(
                        Func1::new(move |v: T| {
                            if !*stopped.get() {
                                obs_n.OnNext(v);
                            }
                        }),
                        Func1::new(move |e: LrcPtr<Exception>| {
                            if !*se.get() {
                                se.set(true);
                                obs_e.OnError(e);
                            }
                        }),
                        Func0::new(move || {
                            if !*sc.get() {
                                completed2.set(true);
                                if *completed1.get() {
                                    sc.set(true);
                                    obs_c.OnCompleted();
                                }
                            }
                        }),
                    ))
                };

                mkDisposable(Func0::new(move || {
                    h1.Dispose();
                    h2.Dispose();
                }))
            },
        ))
    }

    pub fn pairwise<T: Clone + 'static>(
        source: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<dyn IObservable_1<LrcPtr<(T, T)>>> {
        mkObservable(Func1::new(
            move |observer: LrcPtr<dyn IObserver_1<LrcPtr<(T, T)>>>| -> LrcPtr<dyn IDisposable> {
                let last = refCell::<Option<T>>(None);
                let obs = observer.clone();
                let inner = mkObserver(
                    Func1::new(move |next: T| {
                        if let Some(l) = last.get().clone() {
                            obs.OnNext(LrcPtr::new((l, next.clone())));
                        }
                        last.set(Some(next));
                    }),
                    fwdError(observer.clone()),
                    fwdCompleted(observer.clone()),
                );
                source.Subscribe(inner)
            },
        ))
    }

    pub fn partition<T: Clone + 'static>(
        predicate: Func1<T, bool>,
        source: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<(LrcPtr<dyn IObservable_1<T>>, LrcPtr<dyn IObservable_1<T>>)> {
        let pred = predicate.clone();
        let first = filter(predicate, source.clone());
        let second = filter(Func1::new(move |x: T| !pred(x)), source);
        LrcPtr::new((first, second))
    }

    pub fn scan<U: Clone + 'static, T: Clone + 'static>(
        collector: Func2<U, T, U>,
        state: U,
        source: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<dyn IObservable_1<U>> {
        mkObservable(Func1::new(
            move |observer: LrcPtr<dyn IObserver_1<U>>| -> LrcPtr<dyn IDisposable> {
                let collector = collector.clone();
                let st = refCell(state.clone());
                let obs = observer.clone();
                let inner = mkObserver(
                    Func1::new(move |t: T| {
                        let u = collector(st.get().clone(), t);
                        st.set(u.clone());
                        obs.OnNext(u);
                    }),
                    fwdError(observer.clone()),
                    fwdCompleted(observer.clone()),
                );
                source.Subscribe(inner)
            },
        ))
    }

    pub fn split<T: Clone + 'static, U1: Clone + 'static, U2: Clone + 'static>(
        splitter: Func1<T, LrcPtr<Choice_2<U1, U2>>>,
        source: LrcPtr<dyn IObservable_1<T>>,
    ) -> LrcPtr<(LrcPtr<dyn IObservable_1<U1>>, LrcPtr<dyn IObservable_1<U2>>)> {
        let sp = splitter.clone();
        let first = choose(
            Func1::new(move |v: T| match &*sp(v) {
                Choice_2::Choice1Of2(x) => Some(x.clone()),
                _ => None,
            }),
            source.clone(),
        );
        let second = choose(
            Func1::new(move |v: T| match &*splitter(v) {
                Choice_2::Choice2Of2(x) => Some(x.clone()),
                _ => None,
            }),
            source,
        );
        LrcPtr::new((first, second))
    }
}
