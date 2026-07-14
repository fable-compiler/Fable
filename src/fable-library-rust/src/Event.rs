// F#-style events for Rust. Mirrors src/fable-library-ts/Event.ts and
// src/fable-library-py/fable_library/event.py.
//
// The Microsoft.FSharp.Control event *interfaces* (IDelegateEvent`1, IEvent`2)
// live in Interfaces.fs so they take part in Fable's namespace aggregation.
// The concrete runtime types (FSharpEvent`1, AnonymousEvent) live here in the
// `Event_` module; a transformType mapping in the Rust backend points the
// FSharpEvent`1 type at `Event_::FSharpEvent`1`.

pub mod Event_ {
    use crate::Choice_::Choice_2;
    use crate::Microsoft::FSharp::Control::{IDelegateEvent_1, IEvent_2};
    use crate::Native_::{
        getZeroObj, referenceEquals, refCell, Any, Func0, Func1, Func2, LrcPtr, MutCell, Vec,
    };
    use crate::Observable_::{mkDisposable, mkObserver, noCompleted, noError};
    use crate::System::{IDisposable, IObservable_1, IObserver_1};

    // Handler<'T> = delegate of obj * 'T -> unit
    pub type Handler<T> = Func2<LrcPtr<dyn Any>, T, ()>;

    // ----- AnonymousEvent: the published IEvent -----
    // A concrete type so the Publish field can be used through IEvent_2
    // (AddHandler/RemoveHandler) *and* unsize-coerced to `dyn IObservable_1`
    // (in F#/JS an IEvent is also an IObservable).
    #[derive(Clone)]
    pub struct AnonymousEvent<T: Clone + 'static> {
        add_handler: Func1<Handler<T>, ()>,
        remove_handler: Func1<Handler<T>, ()>,
    }

    impl<T: Clone + 'static> core::fmt::Debug for AnonymousEvent<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl<T: Clone + 'static> core::fmt::Display for AnonymousEvent<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl<T: Clone + 'static> AnonymousEvent<T> {
        fn do_sub(&self, observer: LrcPtr<dyn IObserver_1<T>>) -> LrcPtr<dyn IDisposable> {
            let h: Handler<T> =
                Func2::new(move |_sender: LrcPtr<dyn Any>, args: T| observer.OnNext(args));
            (self.add_handler)(h.clone());
            let rem = self.remove_handler.clone();
            mkDisposable(Func0::new(move || rem(h.clone())))
        }

        // Inherent methods so calls on the concrete `Publish` field resolve
        // without the interface trait having to be imported at the call site
        // (Fable does not always emit a `use` for the flattened event trait).
        pub fn Subscribe(&self, observer: LrcPtr<dyn IObserver_1<T>>) -> LrcPtr<dyn IDisposable> {
            self.do_sub(observer)
        }
        pub fn AddHandler(&self, handler: Handler<T>) {
            (self.add_handler)(handler)
        }
        pub fn RemoveHandler(&self, handler: Handler<T>) {
            (self.remove_handler)(handler)
        }
    }

    impl<T: Clone + 'static> IEvent_2<Handler<T>, T> for AnonymousEvent<T> {
        fn Subscribe(&self, observer: LrcPtr<dyn IObserver_1<T>>) -> LrcPtr<dyn IDisposable> {
            self.do_sub(observer)
        }
        fn AddHandler(&self, handler: Handler<T>) {
            (self.add_handler)(handler)
        }
        fn RemoveHandler(&self, handler: Handler<T>) {
            (self.remove_handler)(handler)
        }
    }

    impl<T: Clone + 'static> IDelegateEvent_1<Handler<T>> for AnonymousEvent<T> {
        fn AddHandler(&self, handler: Handler<T>) {
            (self.add_handler)(handler)
        }
        fn RemoveHandler(&self, handler: Handler<T>) {
            (self.remove_handler)(handler)
        }
    }

    impl<T: Clone + 'static> IObservable_1<T> for AnonymousEvent<T> {
        fn Subscribe(&self, observer: LrcPtr<dyn IObserver_1<T>>) -> LrcPtr<dyn IDisposable> {
            self.do_sub(observer)
        }
    }

    // ----- FSharpEvent`1 -----
    #[derive(Clone)]
    pub struct FSharpEvent_1<T: Clone + 'static> {
        handlers: LrcPtr<MutCell<Vec<Handler<T>>>>,
        pub Publish: LrcPtr<AnonymousEvent<T>>,
    }

    impl<T: Clone + 'static> core::fmt::Debug for FSharpEvent_1<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl<T: Clone + 'static> core::fmt::Display for FSharpEvent_1<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
            write!(f, "{}", core::any::type_name::<Self>())
        }
    }

    impl<T: Clone + 'static> FSharpEvent_1<T> {
        pub fn Trigger(&self, value: T) {
            // Snapshot so a handler may unsubscribe during dispatch.
            let hs = self.handlers.get().clone();
            for h in hs.iter() {
                h(getZeroObj(), value.clone());
            }
        }
    }

    // Constructor for `new Event<'T>()` (routed here from the events replacement).
    pub fn default<T: Clone + 'static>() -> LrcPtr<FSharpEvent_1<T>> {
        let handlers: LrcPtr<MutCell<Vec<Handler<T>>>> = refCell(Vec::new());
        let add_handler = Func1::new({
            let handlers = handlers.clone();
            move |h: Handler<T>| handlers.get_mut().push(h)
        });
        let remove_handler = Func1::new({
            let handlers = handlers.clone();
            move |h: Handler<T>| {
                let vec = handlers.get_mut();
                // Compare by underlying function identity (the shared `dyn Fn`
                // pointer). Func2's own PartialEq compares wrapper-handle
                // addresses, which differ between clones, and the handler removed
                // is a clone of the one stored.
                if let Some(pos) = vec.iter().position(|x| referenceEquals(&**x, &*h)) {
                    vec.remove(pos);
                }
            }
        });
        let publish = LrcPtr::new(AnonymousEvent {
            add_handler,
            remove_handler,
        });
        LrcPtr::new(FSharpEvent_1 {
            handlers,
            Publish: publish,
        })
    }

    // `CreateEvent`: build an IEvent from add/remove handler callbacks.
    pub fn createEvent<T: Clone + 'static>(
        add_handler: Func1<Handler<T>, ()>,
        remove_handler: Func1<Handler<T>, ()>,
    ) -> LrcPtr<AnonymousEvent<T>> {
        LrcPtr::new(AnonymousEvent {
            add_handler,
            remove_handler,
        })
    }

    // ----- module functions -----

    pub fn add<T: Clone + 'static>(
        callback: Func1<T, ()>,
        source: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) {
        source.Subscribe(mkObserver(callback, noError(), noCompleted()));
    }

    pub fn choose<T: Clone + 'static, U: Clone + 'static>(
        chooser: Func1<T, Option<U>>,
        source: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) -> LrcPtr<dyn IEvent_2<Handler<U>, U>> {
        let ev = default::<U>();
        let evc = ev.clone();
        add(
            Func1::new(move |t: T| {
                if let Some(u) = chooser(t) {
                    evc.Trigger(u);
                }
            }),
            source,
        );
        ev.Publish.clone()
    }

    pub fn filter<T: Clone + 'static>(
        predicate: Func1<T, bool>,
        source: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) -> LrcPtr<dyn IEvent_2<Handler<T>, T>> {
        choose(
            Func1::new(move |x: T| if predicate(x.clone()) { Some(x) } else { None }),
            source,
        )
    }

    pub fn map<T: Clone + 'static, U: Clone + 'static>(
        mapping: Func1<T, U>,
        source: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) -> LrcPtr<dyn IEvent_2<Handler<U>, U>> {
        let ev = default::<U>();
        let evc = ev.clone();
        add(Func1::new(move |t: T| evc.Trigger(mapping(t))), source);
        ev.Publish.clone()
    }

    pub fn merge<T: Clone + 'static>(
        event1: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
        event2: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) -> LrcPtr<dyn IEvent_2<Handler<T>, T>> {
        let ev = default::<T>();
        let ev1 = ev.clone();
        let ev2 = ev.clone();
        add(Func1::new(move |t: T| ev1.Trigger(t)), event1);
        add(Func1::new(move |t: T| ev2.Trigger(t)), event2);
        ev.Publish.clone()
    }

    pub fn pairwise<T: Clone + 'static>(
        source: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) -> LrcPtr<dyn IEvent_2<Handler<LrcPtr<(T, T)>>, LrcPtr<(T, T)>>> {
        let ev = default::<LrcPtr<(T, T)>>();
        let evc = ev.clone();
        let last = refCell::<Option<T>>(None);
        add(
            Func1::new(move |next: T| {
                if let Some(l) = last.get().clone() {
                    evc.Trigger(LrcPtr::new((l, next.clone())));
                }
                last.set(Some(next));
            }),
            source,
        );
        ev.Publish.clone()
    }

    pub fn partition<T: Clone + 'static>(
        predicate: Func1<T, bool>,
        source: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) -> LrcPtr<(
        LrcPtr<dyn IEvent_2<Handler<T>, T>>,
        LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    )> {
        let pred = predicate.clone();
        let first = filter(predicate, source.clone());
        let second = filter(Func1::new(move |x: T| !pred(x)), source);
        LrcPtr::new((first, second))
    }

    pub fn scan<U: Clone + 'static, T: Clone + 'static>(
        collector: Func2<U, T, U>,
        state: U,
        source: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) -> LrcPtr<dyn IEvent_2<Handler<U>, U>> {
        let st = refCell(state);
        map(
            Func1::new(move |t: T| {
                let u = collector(st.get().clone(), t);
                st.set(u.clone());
                u
            }),
            source,
        )
    }

    pub fn split<T: Clone + 'static, U1: Clone + 'static, U2: Clone + 'static>(
        splitter: Func1<T, LrcPtr<Choice_2<U1, U2>>>,
        source: LrcPtr<dyn IEvent_2<Handler<T>, T>>,
    ) -> LrcPtr<(
        LrcPtr<dyn IEvent_2<Handler<U1>, U1>>,
        LrcPtr<dyn IEvent_2<Handler<U2>, U2>>,
    )> {
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
