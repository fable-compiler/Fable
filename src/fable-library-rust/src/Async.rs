#![allow(non_snake_case)]

#[cfg(feature = "futures")]
pub mod Async_ {
    use std::future::{self, ready, Future};
    use std::pin::Pin;
    use std::sync::Arc;
    use std::thread;
    use std::time::Duration;

    use futures::executor::{self, LocalPool};
    use futures::lock::Mutex;
    use futures::FutureExt;

    use super::Task_::Task;

    pub struct Async<T: Sized + Send + Sync> {
        pub future: Arc<Mutex<Pin<Box<dyn Future<Output = T> + Send + Sync>>>>,
    }

    impl<T: Clone + Send + Sync> Future for &Async<T> {
        type Output = T;

        fn poll(
            self: Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            let p = self
                .future
                .try_lock()
                .map(|mut f| f.poll_unpin(cx))
                .unwrap_or_else(|| {
                    //Again blocking wait, not good
                    thread::sleep(Duration::from_millis(10));
                    cx.waker().wake_by_ref();

                    std::task::Poll::Pending
                });
            p
        }
    }

    pub fn startAsTask<T: Clone + Send + Sync + 'static>(a: Arc<Async<T>>) -> Arc<Task<T>> {
        let unitFut = async move {
            let mut res = a.future.lock().await;
            let res = res.as_mut().await;
            res
        };
        let task = Arc::from(Task::new(unitFut));
        Task::start(task.clone());
        task
    }

    pub fn runSynchronously<T: Clone + Send + Sync + 'static>(a: Arc<Async<T>>) -> T {
        let unitFut = async move {
            let mut res = a.future.lock().await;
            let res = res.as_mut().await;
            res
        };
        executor::block_on(unitFut)
    }

    pub fn awaitTask<T: Clone + Send + Sync + 'static>(a: Arc<Task<T>>) -> Arc<Async<T>> {
        let fut = async move { (&*a).await };
        let a: Pin<Box<dyn Future<Output = T> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async {
            future: Arc::from(Mutex::from(a)),
        })
    }
}

#[cfg(feature = "futures")]
pub mod AsyncBuilder_ {
    use std::{
        future::{ready, Future},
        pin::Pin,
        sync::Arc,
    };

    use futures::lock::Mutex;

    use super::Async_::Async;

    pub fn delay<T: Send + Sync>(
        binder: Arc<impl Fn() -> Arc<Async<T>> + 'static>,
    ) -> Arc<Async<T>> {
        let pr = binder();
        Arc::from(Async {
            future: pr.future.clone(),
        })
    }

    pub fn bind<T: Clone + Send + Sync + 'static, U: Clone + Send + Sync + 'static>(
        opt: Arc<Async<T>>,
        binder: Arc<impl Fn(T) -> Arc<Async<U>> + Send + Sync + 'static>,
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

    pub fn zero() -> Arc<Async<()>> {
        r_return(())
    }
}

#[cfg(feature = "futures")]
pub mod ThreadPool {
    use std::sync::RwLock;

    use futures::executor::ThreadPool;

    static mut POOL: Option<RwLock<ThreadPool>> = None;
    pub fn try_init_and_get_pool() -> &'static RwLock<ThreadPool> {
        unsafe {
            if POOL.is_none() {
                let pool = ThreadPool::new().unwrap();
                POOL = Some(RwLock::new(pool));
            }

            POOL.as_ref().unwrap()
        }
    }
}

#[cfg(feature = "futures")]
pub mod Monitor_ {
    use std::{
        any::Any,
        collections::HashSet,
        sync::{Arc, Mutex, RwLock, Weak},
        thread,
        time::Duration,
    };

    use crate::Native_::Lrc;

    static mut LOCKS: Option<RwLock<HashSet<usize>>> = None;
    fn try_init_and_get_locks() -> &'static RwLock<HashSet<usize>> {
        unsafe {
            let hs = HashSet::new();
            if LOCKS.is_none() {
                LOCKS = Some(RwLock::new(hs));
            }

            LOCKS.as_ref().unwrap()
        }
    }

    pub fn enter<T>(o: Lrc<T>) {
        let p = Arc::<T>::as_ptr(&o) as usize;
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

    pub fn exit<T>(o: Lrc<T>) {
        let p = Arc::<T>::as_ptr(&o) as usize;
        let hasRemoved = try_init_and_get_locks().write().unwrap().remove(&p);
        if (!hasRemoved) {
            panic!("Not removed {}", p)
        }
    }
}

#[cfg(feature = "futures")]
pub mod Task_ {
    use std::{
        pin::Pin,
        sync::{Arc, RwLock},
        task::Poll,
        thread::{self, JoinHandle},
        time::Duration,
    };

    use futures::{Future, FutureExt};

    use super::ThreadPool::try_init_and_get_pool;

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
            std::mem::replace(self, next)
        }
    }

    #[derive(Clone)]
    pub struct Task<T: Sized + Clone + Send> {
        result: Arc<RwLock<TaskState<T>>>,
    }

    impl<T: Clone + Send + Sync> Future for &Task<T> {
        type Output = T;

        fn poll(
            self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            //eprintln!("{:?} Polling task for result", thread::current().id());
            let m = self.result.read().unwrap();

            match &*m {
                TaskState::New(_) => {
                    //schedule?
                    //eprintln!("{:?} poll: task is new, waking up", thread::current().id());
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                TaskState::Running => {
                    //eprintln!("{:?} poll: pending, nothing to do", thread::current().id());

                    //todo - this is no good as it blocks the thread. It needs to be non-blocking and delegate out
                    thread::sleep(Duration::from_millis(100));
                    cx.waker().wake_by_ref();

                    Poll::Pending
                }
                TaskState::Complete(res) => {
                    //eprintln!("{:?} Poll succeeded", thread::current().id());
                    Poll::Ready(res.clone())
                }
            }
        }
    }

    impl<T: Clone + Send + Sync + 'static> Task<T> {
        pub fn new(fut: impl Future<Output = T> + Send + Sync + 'static) -> Task<T> {
            Task {
                result: Arc::from(RwLock::from(TaskState::New(Box::pin(fut)))),
            }
        }

        pub fn from_result(value: T) -> Task<T> {
            Task {
                result: Arc::from(RwLock::from(TaskState::Complete(value))),
            }
        }

        pub fn set_result(&self, value: T) {
            let mut m = self.result.write().unwrap();
            //eprintln!("{:?} set task result", thread::current().id());
            (*m) = TaskState::Complete(value);
            //eprintln!("{:?} set task result completed", thread::current().id());
        }

        pub fn is_new(&self) -> bool {
            self.result.read().unwrap().is_new()
        }

        fn is_complete(&self) -> bool {
            self.result.read().unwrap().is_complete()
        }

        pub fn get_result(&self) -> T {
            while !self.is_complete() {
                //eprintln!("{:?} try get result", thread::current().id());
                thread::sleep(Duration::from_millis(10));
            }
            //eprintln!("{:?} has result", thread::current().id());
            let t = self.result.read().unwrap().unwrap();
            t
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
        binder: Arc<impl Fn(T) -> Arc<Task<U>> + Send + Sync + 'static>,
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

    pub fn delay<T: Clone + Send + Sync>(
        binder: Arc<impl Fn() -> Arc<Task<T>> + 'static>,
    ) -> Arc<Task<T>> {
        let pr = binder();
        pr
    }

    pub fn r_return<T: Clone + Send + Sync + 'static>(item: T) -> Arc<Task<T>> {
        let t = Task::from_result(item);
        Arc::from(t)
    }

    pub fn zero() -> Arc<Task<()>> {
        r_return(())
    }

    pub fn from_result<T: Clone + Send>(t: T) -> Arc<Task<T>> {
        let t = Task {
            result: Arc::from(RwLock::from(TaskState::Complete(t))),
        };
        Arc::from(t)
    }
}

#[cfg(feature = "futures")]
pub mod TaskBuilder_ {
    use std::{rc::Rc, sync::Arc};

    use super::super::Native_::Lrc;
    use super::Task_::Task;

    pub struct TaskBuilder {}

    impl TaskBuilder {
        pub fn run<T: Clone + Send + Sync + 'static>(&self, task: Arc<Task<T>>) -> Arc<Task<T>> {
            Task::start(task.clone());
            task
        }
    }

    pub fn new() -> Lrc<TaskBuilder> {
        Lrc::from(TaskBuilder {})
    }
}

#[cfg(feature = "futures")]
pub mod Thread_ {
    use std::{thread, time::Duration};

    pub fn sleep(millis: i32) {
        thread::sleep(Duration::from_millis(millis as u64));
    }
}