#[cfg(feature = "threaded")]
pub mod Async_ {
    use std::future::{self, Future, ready};
    use std::pin::Pin;
    use std::sync::Arc;
    use std::thread;
    use std::time::Duration;

    use futures::FutureExt;
    use futures::executor::{self, LocalPool};
    use futures::lock::Mutex;
    use futures_timer::Delay;

    use super::Task_::Task;
    use crate::NativeArray_::{array_from, Array};
    use crate::Native_::Seq;
    use crate::System::Threading::CancellationToken;

    pub struct Async<T: Sized + Send + Sync> {
        pub future: Arc<Mutex<Pin<Box<dyn Future<Output = T> + Send + Sync>>>>,
    }

    impl<T: Clone + Send + Sync> Future for &Async<T> {
        type Output = T;

        fn poll(
            self: Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            // Poll the lock's own future so contention parks on its waker.
            // try_lock plus a 10ms sleep spent that long doing nothing whenever
            // two pollers met.
            let mut lock = self.future.lock();

            match lock.poll_unpin(cx) {
                std::task::Poll::Ready(mut guard) => guard.as_mut().poll(cx),
                std::task::Poll::Pending => std::task::Poll::Pending,
            }
        }
    }

    pub fn sleep(milliseconds: i32) -> Arc<Async<()>> {
        let fut = Delay::new(Duration::from_millis(milliseconds as u64));
        let a: Pin<Box<dyn Future<Output = ()> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }

    pub fn startAsTask<T: Clone + Send + Sync + 'static>(
        a: Arc<Async<T>>,
        taskCreationOptions: Option<i32>,
        cancellationToken: Option<CancellationToken>,
    ) -> Arc<Task<T>> {
        let unitFut = async move {
            let mut res = a.future.lock().await;
            let res = res.as_mut().await;
            res
        };
        let task = Arc::from(Task::new(unitFut));
        Task::start(task.clone());
        task
    }

    pub fn runSynchronously<T: Clone + Send + Sync + 'static>(
        a: Arc<Async<T>>,
        timeout: Option<i32>,
        cancellationToken: Option<CancellationToken>,
    ) -> T {
        let unitFut = async move {
            let mut res = a.future.lock().await;
            let res = res.as_mut().await;
            res
        };
        executor::block_on(unitFut)
    }

    /// Async.Parallel. Runs every computation on one executor and collects the
    /// results in order. Previously missing entirely, so any use of it failed to
    /// resolve.
    pub fn parallel<T: Clone + Send + Sync + 'static>(
        computations: Seq<Arc<Async<T>>>,
    ) -> Arc<Async<Array<T>>> {
        // F# hands this a seq, so materialise it before anything is awaited.
        let items: Vec<Arc<Async<T>>> = crate::Seq_::toArray(computations).iter().cloned().collect();

        let fut = async move {
            let futures = items.into_iter().map(|a| async move {
                let mut guard = a.future.lock().await;
                guard.as_mut().await
            });

            let results: Vec<T> = futures::future::join_all(futures).await;
            array_from(results)
        };

        let b: Pin<Box<dyn Future<Output = Array<T>> + Send + Sync + 'static>> = Box::pin(fut);

        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    /// Async.StartImmediateAsTask. Runs on the calling thread rather than
    /// queueing onto the thread pool, so it does not pay for a hand-off. The
    /// returned task is therefore already complete: there is no scheduler here
    /// to leave it pending on, so this trades .NET's concurrency for the speed
    /// the name promises. Use startAsTask when the work should overlap.
    pub fn startImmediateAsTask<T: Clone + Send + Sync + 'static>(
        a: Arc<Async<T>>,
        cancellationToken: Option<CancellationToken>,
    ) -> Arc<Task<T>> {
        let result = runSynchronously(a, None, cancellationToken);
        Arc::from(Task::from_result(result))
    }

    pub fn awaitTask<T: Clone + Send + Sync + 'static>(a: Arc<Task<T>>) -> Arc<Async<T>> {
        let fut = async move { (&*a).await };
        let a: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }
}

#[cfg(feature = "threaded")]
pub mod AsyncBuilder_ {
    use std::future::{Future, ready};
    use std::pin::Pin;
    use std::sync::Arc;

    use futures::lock::Mutex;

    use super::Async_::Async;
    use crate::Native_::{Func0, Func1};

    pub fn delay<T: Send + Sync + 'static>(binder: Func0<Arc<Async<T>>>) -> Arc<Async<T>> {
        let pr = binder();
        Arc::from(Async {
            future: pr.future.clone(),
        })
    }

    pub fn bind<T: Clone + Send + Sync + 'static, U: Clone + Send + Sync + 'static>(
        opt: Arc<Async<T>>,
        binder: Func1<T, Arc<Async<U>>>,
    ) -> Arc<Async<U>> {
        let next = async move {
            let mut m = opt.future.lock().await;
            let m = m.as_mut().await;
            let nextAsync = binder(m);
            let mut next = nextAsync.future.lock().await;
            next.as_mut().await
        };

        let b: Pin<Box<dyn Future<Output = U> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn r_return<T: Send + Sync + 'static>(item: T) -> Arc<Async<T>> {
        let r = ready(item);
        let b: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(r);
        Arc::from(Async {
            future: Arc::from(Mutex::from(b)),
        })
    }

    pub fn return_from<T: Send + Sync + 'static>(computation: Arc<Async<T>>) -> Arc<Async<T>> {
        computation
    }

    pub fn zero<T: Send + Sync + 'static>() -> Arc<Async<()>> {
        r_return(())
    }
}

#[cfg(feature = "threaded")]
pub mod ThreadPool {
    use std::sync::{OnceLock, RwLock};

    use futures::executor::ThreadPool;

    static POOL: OnceLock<RwLock<ThreadPool>> = OnceLock::new();
    pub fn try_init_and_get_pool() -> &'static RwLock<ThreadPool> {
        POOL.get_or_init(|| RwLock::new(ThreadPool::new().unwrap()))
    }
}

#[cfg(feature = "threaded")]
pub mod Monitor_ {
    use std::any::Any;
    use std::collections::HashSet;
    use std::sync::{Arc, Mutex, OnceLock, RwLock, Weak};
    use std::thread;
    use std::time::Duration;

    use crate::Native_::{Func0, LrcPtr};

    static LOCKS: OnceLock<RwLock<HashSet<usize>>> = OnceLock::new();
    fn try_init_and_get_locks() -> &'static RwLock<HashSet<usize>> {
        LOCKS.get_or_init(|| RwLock::new(HashSet::new()))
    }

    // `?Sized` so a boxed `obj` (LrcPtr<dyn Any>) can be locked on: casting to
    // `*const ()` first discards the vtable of a fat pointer, leaving the data
    // address, which is what identifies the lock.
    pub fn enter<T: ?Sized>(o: LrcPtr<T>) {
        let p = Arc::<T>::as_ptr(&o) as *const () as usize;
        loop {
            let otherHasLock = try_init_and_get_locks().read().unwrap().get(&p).is_some();
            if otherHasLock {
                thread::sleep(Duration::from_millis(10));
            } else {
                try_init_and_get_locks().write().unwrap().insert(p);
                return;
            }
        }
    }

    pub fn exit<T: ?Sized>(o: LrcPtr<T>) {
        let p = Arc::<T>::as_ptr(&o) as *const () as usize;
        let hasRemoved = try_init_and_get_locks().write().unwrap().remove(&p);
        if (!hasRemoved) {
            panic!("Not removed {}", p)
        }
    }

    // Not technically part of monitor, but it needs to be behind a feature switch, so cannot just dump this in Native
    pub fn lock<T: Clone + Send + Sync, U: 'static>(toLock: LrcPtr<T>, f: Func0<U>) -> U {
        enter(toLock.clone());
        let returnVal = f();
        // panics will bypass this - need some finally mechanism
        exit(toLock.clone());
        returnVal
    }
}

#[cfg(feature = "threaded")]
pub mod Task_ {
    use std::pin::Pin;
    use std::sync::{Arc, Mutex, RwLock};
    use std::task::{Poll, Waker};
    use std::thread::{self, JoinHandle};
    use std::time::Duration;

    use futures::{Future, FutureExt};

    use super::ThreadPool::try_init_and_get_pool;
    use crate::Native_::{Func0, Func1};

    pub enum TaskState<T: Sized + Clone + Send> {
        New(Pin<Box<dyn Future<Output = T> + Send + Sync>>),
        Running,
        Complete(T),
    }

    impl<T: Sized + Clone + Send + 'static> TaskState<T> {
        pub fn is_new(&self) -> bool {
            match self {
                TaskState::New(_) => true,
                _ => false,
            }
        }

        pub fn is_running(&self) -> bool {
            match self {
                TaskState::Running => true,
                _ => false,
            }
        }

        pub fn is_complete(&self) -> bool {
            match self {
                TaskState::Complete(_) => true,
                _ => false,
            }
        }

        pub fn unwrap(&self) -> T {
            match self {
                TaskState::Complete(t) => t.clone(),
                _ => panic!("Task not yet complete"),
            }
        }

        pub fn replace(&mut self, next: TaskState<T>) -> TaskState<T> {
            core::mem::replace(self, next)
        }
    }

    #[derive(Clone)]
    pub struct Task<T: Sized + Clone + Send> {
        result: Arc<RwLock<TaskState<T>>>,
        // Whoever is awaiting this task. Polling used to sleep the thread for
        // 100ms and try again, which made awaiting a task cost far more than
        // the work it was waiting for.
        wakers: Arc<Mutex<Vec<Waker>>>,
    }

    impl<T: Clone + Send + Sync> Future for &Task<T> {
        type Output = T;

        fn poll(
            self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            if let TaskState::Complete(res) = &*self.result.read().unwrap() {
                return Poll::Ready(res.clone());
            }

            // Register before re-reading the state: set_result may complete the
            // task between the check above and this push, and the re-read below
            // is what stops that racing completion from being missed.
            self.wakers.lock().unwrap().push(cx.waker().clone());

            match &*self.result.read().unwrap() {
                TaskState::Complete(res) => Poll::Ready(res.clone()),
                // Not started yet. start() flips New to Running synchronously
                // before spawning, so this window is short.
                TaskState::New(_) => {
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                // Running: set_result wakes us.
                TaskState::Running => Poll::Pending,
            }
        }
    }

    impl<T: Clone + Send + Sync + 'static> Task<T> {
        pub fn new(fut: impl Future<Output = T> + Send + Sync + 'static) -> Task<T> {
            Task {
                result: Arc::from(RwLock::from(TaskState::New(Box::pin(fut)))),
                wakers: Arc::from(Mutex::from(Vec::new())),
            }
        }

        pub fn from_result(value: T) -> Task<T> {
            Task {
                result: Arc::from(RwLock::from(TaskState::Complete(value))),
                wakers: Arc::from(Mutex::from(Vec::new())),
            }
        }

        pub fn set_result(&self, value: T) {
            {
                let mut m = self.result.write().unwrap();
                (*m) = TaskState::Complete(value);
            }

            // Drain first, then wake outside the lock: a waker may poll straight
            // back into this task.
            let wakers: Vec<Waker> = self.wakers.lock().unwrap().drain(..).collect();

            for w in wakers {
                w.wake();
            }
        }

        pub fn is_new(&self) -> bool {
            self.result.read().unwrap().is_new()
        }

        fn is_complete(&self) -> bool {
            self.result.read().unwrap().is_complete()
        }

        pub fn get_result(&self) -> T {
            if let TaskState::Complete(res) = &*self.result.read().unwrap() {
                return res.clone();
            }

            // Parks on the waker rather than waking every 10ms to look again.
            futures::executor::block_on(self)
        }

        pub fn start(t: Arc<Task<T>>) {
            if !t.result.read().unwrap().is_new() {
                return;
            }
            let ts = t.result.write().unwrap().replace(TaskState::Running);
            match ts {
                TaskState::New(mut fut) => {
                    let f2 = async move {
                        let res = fut.as_mut().await;
                        t.set_result(res);
                    };
                    let pool = super::ThreadPool::try_init_and_get_pool();
                    //eprintln!("{:?} new task added to queue", thread::current().id());
                    pool.write().unwrap().spawn_ok(f2);
                }
                _ => {}
            }
        }
    }

    pub fn bind<T: Clone + Send + Sync + 'static, U: Clone + Send + Sync + 'static>(
        opt: Arc<Task<T>>,
        binder: Func1<T, Arc<Task<U>>>,
    ) -> Arc<Task<U>> {
        let next = async move {
            //eprintln!("{:?} begin await source fut", thread::current().id());
            let m = opt.as_ref().await;
            //eprintln!("{:?} awaiting source future success", thread::current().id());
            let nextAsync = binder(m);
            if nextAsync.is_new() {
                Task::start(nextAsync.clone());
            }
            let next = nextAsync.as_ref().await;
            //eprintln!("{:?} setting result", thread::current().id());
            next
        };

        let task = Task::new(next);
        Arc::from(task)
    }

    pub fn delay<T: Clone + Send + Sync + 'static>(binder: Func0<Arc<Task<T>>>) -> Arc<Task<T>> {
        let pr = binder();
        pr
    }

    pub fn r_return<T: Clone + Send + Sync + 'static>(item: T) -> Arc<Task<T>> {
        let t = Task::from_result(item);
        Arc::from(t)
    }

    pub fn zero<T: Clone + Send + Sync + 'static>() -> Arc<Task<()>> {
        r_return(())
    }

    pub fn from_result<T: Clone + Send>(t: T) -> Arc<Task<T>> {
        let t = Task {
            result: Arc::from(RwLock::from(TaskState::Complete(t))),
            wakers: Arc::from(Mutex::from(Vec::new())),
        };
        Arc::from(t)
    }
}

#[cfg(feature = "threaded")]
pub mod TaskBuilder_ {
    use super::super::Native_::LrcPtr;
    use super::Task_::Task;
    use std::sync::Arc;

    pub struct TaskBuilder {}

    impl TaskBuilder {
        pub fn run<T: Clone + Send + Sync + 'static>(&self, task: Arc<Task<T>>) -> Arc<Task<T>> {
            Task::start(task.clone());
            task
        }
    }

    pub fn new() -> LrcPtr<TaskBuilder> {
        LrcPtr::new(TaskBuilder {})
    }
}

#[cfg(feature = "threaded")]
pub mod Thread_ {
    use std::thread;
    use std::time::Duration;

    use crate::Native_::{Func0, LrcPtr, MutCell};

    enum ThreadInt {
        New(Func0<()>),
        //Building(thread::Builder),
        Running(thread::JoinHandle<()>),
        Empty,
    }
    impl Default for ThreadInt {
        fn default() -> Self {
            ThreadInt::Empty
        }
    }
    pub struct Thread(MutCell<ThreadInt>);

    pub fn new(f: Func0<()>) -> LrcPtr<Thread> {
        LrcPtr::new(Thread(MutCell::from(ThreadInt::New(f))))
    }

    impl Thread {
        pub fn start(&self) {
            match self.0.take() {
                ThreadInt::New(f) => {
                    let t = std::thread::spawn(move || f());
                    self.0.set(ThreadInt::Running(t));
                }
                _ => {}
            }
        }

        pub fn join(&self) {
            match self.0.take() {
                ThreadInt::Running(jh) => {
                    jh.join().expect("Couldn't join on the associated thread");
                }
                _ => {}
            }
        }
    }

    pub fn sleep(millis: i32) {
        thread::sleep(Duration::from_millis(millis as u64));
    }
}
