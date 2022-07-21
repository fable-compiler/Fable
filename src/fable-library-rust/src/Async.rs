#![allow(non_snake_case)]

#[cfg(feature = "futures")]
pub mod Async_ {
    use std::future::{self, Future, ready};
    use std::pin::Pin;
    use std::sync::{Arc};
    use std::thread;
    use std::time::Duration;

    use futures::FutureExt;
    use futures::executor::{self, LocalPool};
    use futures::lock::Mutex;

    use super::Task_::Task_1;

    pub struct Async_1<T: Sized + Send + Sync> {
        pub future: Arc<Mutex<Pin<Box<dyn Future<Output = T> + Send + Sync>>>>
    }

    impl <T: Clone + Send + Sync> Future for &Async_1<T> {
        type Output = T;

        fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
            let p = self.future.try_lock()
                .map(|mut f|f.poll_unpin(cx))
                .unwrap_or_else(||{
                    //Again blocking wait, not good
                    thread::sleep(Duration::from_millis(10));
                    cx.waker().wake_by_ref();

                    std::task::Poll::Pending
                });
            p
        }
    }

    pub fn startAsTask<T: Clone + Send + Sync + 'static>(a: Arc<Async_1<T>>) -> Arc<Task_1<T>> {
        let unitFut = async move {
            let mut res = a.future.lock().await;
            let res = res.as_mut().await;
            res
        };
        let task = Arc::from(Task_1::new(unitFut));
        Task_1::start(task.clone());
        task
    }

    pub fn runSynchronously<T: Clone + Send + Sync + 'static>(a: Arc<Async_1<T>>) -> T {
        let unitFut = async move {
            let mut res = a.future.lock().await;
            let res = res.as_mut().await;
            res
        };
        executor::block_on(unitFut)
    }

    pub fn awaitTask<T: Clone + Send + Sync + 'static>(a: Arc<Task_1<T>>) -> Arc<Async_1<T>> {
        let fut = async move { (&*a).await };
        let a: Pin<Box<dyn Future<Output=T> + Send + Sync + 'static>> = Box::pin(fut);
        Arc::from(Async_1{ future: Arc::from(Mutex::from(a)) })
    }
}

#[cfg(feature = "futures")]
pub mod AsyncBuilder_ {
    use std::{sync::Arc, pin::Pin, future::{Future, ready}};

    use futures::lock::Mutex;

    use super::Async_::Async_1;

    pub fn delay<T: Send + Sync>(binder: Arc<impl Fn() -> Arc<Async_1<T>> + 'static>) -> Arc<Async_1<T>> {
        let pr = binder();
        Arc::from(Async_1 {future: pr.future.clone()})
    }

    pub fn bind<T: Clone  + Send + Sync + 'static, U: Clone  + Send + Sync + 'static>(
        opt: Arc<Async_1<T>>,
        binder: Arc<impl Fn(T) -> Arc<Async_1<U>> + Send + Sync + 'static>
                         ) -> Arc<Async_1<U>>
    {
        let next =
            async move {
                let mut m = opt.future.lock().await;
                let m = m.as_mut().await;
                let nextAsync = binder(m);
                let mut next = nextAsync.future.lock().await;
                next.as_mut().await
            };

        let b :Pin<Box<dyn Future<Output=U> + Send + Sync + 'static>> = Box::pin(next);
        Arc::from(Async_1 { future: Arc::from(Mutex::from(b)) })
    }

    pub fn r_return<T: Send + Sync + 'static>(item: T) -> Arc<Async_1<T>>{
        let r = ready(item);
        let b :Pin<Box<dyn Future<Output=T> + Send + Sync + 'static>> = Box::pin(r);
        Arc::from(Async_1 { future: Arc::from(Mutex::from(b)) })
    }
}

#[cfg(feature = "futures")]
pub mod ThreadPool {
    use std::sync::RwLock;

    use futures::executor::ThreadPool;

    static mut POOL: Option<RwLock<ThreadPool>> = None;
    pub fn try_init_and_get_pool() -> & 'static RwLock<ThreadPool> {
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
pub mod Task_ {
    use std::{sync::{Arc, RwLock}, thread::{self, JoinHandle}, time::Duration, task::Poll, pin::Pin};

    use futures::{FutureExt, Future};

    use super::ThreadPool::try_init_and_get_pool;

    pub enum TaskState<T: Sized + Clone + Send> {
        New(Pin<Box<dyn Future<Output = T> + Send + Sync>>),
        Running,
        Complete(T)
    }

    impl <T: Sized + Clone + Send + 'static> TaskState<T> {

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
    pub struct Task_1<T: Sized + Clone + Send> {
        result: Arc<RwLock<TaskState<T>>>,
    }

    impl <T: Clone + Send + Sync> Future for &Task_1<T> {
        type Output = T;

        fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
            //eprintln!("{:?} Polling task for result", thread::current().id());
            let m = self.result.read().unwrap();

            match &*m {
                TaskState::New(_) => {
                    //schedule?
                    //eprintln!("{:?} poll: task is new, waking up", thread::current().id());
                    cx.waker().wake_by_ref();
                    Poll::Pending
                },
                TaskState::Running  => {
                    //eprintln!("{:?} poll: pending, nothing to do", thread::current().id());

                    //todo - this is no good as it blocks the thread. It needs to be non-blocking and delegate out
                    thread::sleep(Duration::from_millis(100));
                    cx.waker().wake_by_ref();

                    Poll::Pending
                },
                TaskState::Complete(res) => {
                    //eprintln!("{:?} Poll succeeded", thread::current().id());
                    Poll::Ready(res.clone())
                },
            }
        }
    }

    impl <T: Clone + Send + Sync + 'static> Task_1<T>{
        pub fn new(fut: impl Future<Output = T> + Send + Sync + 'static) -> Task_1<T> {
            Task_1 { result: Arc::from(RwLock::from(TaskState::New(Box::pin(fut)))) }
        }

        pub fn from_result(value: T) -> Task_1<T> {
            Task_1 { result: Arc::from(RwLock::from(TaskState::Complete(value))) }
        }

        pub fn set_result(&self, value: T){
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

        pub fn start(t: Arc<Task_1<T>>) {
            if !t.result.read().unwrap().is_new() {
                return
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

    pub fn bind<T: Clone  + Send + Sync + 'static, U: Clone  + Send + Sync + 'static>(
        opt: Arc<Task_1<T>>,
        binder: Arc<impl Fn(T) -> Arc<Task_1<U>> + Send + Sync + 'static>
                         ) -> Arc<Task_1<U>> {
        let next =
            async move {
                //eprintln!("{:?} begin await source fut", thread::current().id());
                let m = opt.as_ref().await;
                //eprintln!("{:?} awaiting source future success", thread::current().id());
                let nextAsync = binder(m);
                if nextAsync.is_new() {
                    Task_1::start(nextAsync.clone());
                }
                let next = nextAsync.as_ref().await;
                //eprintln!("{:?} setting result", thread::current().id());
                next
            };

        let task = Task_1::new(next);
        Arc::from(task)
    }

    pub fn delay<T: Clone + Send + Sync>(binder: Arc<impl Fn() -> Arc<Task_1<T>> + 'static>) -> Arc<Task_1<T>> {
        let pr = binder();
        pr
    }

    pub fn r_return<T: Clone + Send + Sync + 'static>(item: T) -> Arc<Task_1<T>> {
        let t = Task_1::from_result(item);
        Arc::from(t)
    }

    pub fn from_result<T: Clone + Send>(t: T) -> Arc<Task_1<T>> {
        let t = Task_1 { result: Arc::from(RwLock::from(TaskState::Complete(t))) };
        Arc::from(t)
    }
}

#[cfg(feature = "futures")]
pub mod TaskBuilder_ {
    use std::{sync::Arc, rc::Rc};

    use super::Task_::Task_1;

    pub struct TaskBuilder {}

    impl TaskBuilder {
        pub fn run<T: Clone + Send + Sync + 'static>(&self, task: Arc<Task_1<T>>) -> Arc<Task_1<T>> {
            Task_1::start(task.clone());
            task
        }
    }

    pub fn new() -> Rc<TaskBuilder> {
        Rc::from(TaskBuilder {})
    }
}