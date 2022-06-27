#![allow(non_snake_case)]

pub mod Async_ {
    use std::future::{self, Future, ready};
    use std::pin::Pin;
    use std::sync::{Arc};
    use std::thread;

    use futures::executor::{self, LocalPool};
    use futures::lock::Mutex;

    use super::Task_::Task_1;

    pub struct FSharpAsync_1<T: Sized + Send + Sync> {
        pub future: Arc<Mutex<Pin<Box<dyn Future<Output = T> + Send + Sync>>>>
    }

    // todo - set up global thread pool
    // thread_local! {
    //     pub static POOL: std::cell::RefCell<LocalPool>  = std::cell::RefCell::from(LocalPool::new());
    // }

    pub fn startAsTask<T: Clone + Send + Sync + 'static>(a: Arc<FSharpAsync_1<T>>) -> Task_1<T>{
        //todo
        let task = Task_1::<T>::new();
        let task1 = task.clone();
        let handle = thread::spawn(move || {
            let unitFut = async {
                let mut res = a.future.lock().await;
                let res = res.as_mut().await;
                task1.set_result(res);
            };
            executor::block_on(unitFut);
        });

        //todo - use threadpool as spinning up a new thread for every Task is obviously super inefficient
        // POOL::with(|pool|{
        //    pool.run_until(unitFut);
        // });
        //pool.run_until(unitFut);
        //handle.join().expect("great");

        task.set_handle(handle);
        task
    }

    pub fn bind<T: Clone + Send + Sync + 'static, U: Clone + Send + Sync + 'static>(
        opt: FSharpAsync_1<T>,
        binder: Arc<impl Fn(T) -> FSharpAsync_1<U> + 'static + Send + Sync>
                         ) -> FSharpAsync_1<U>
    {
        let next =
            async move {
                let mut m = opt.future.lock().await;
                let m = m.as_mut().await;
                let nextAsync = binder(m);
                let mut next = nextAsync.future.lock().await;
                next.as_mut().await
            };

        let b :Pin<Box<dyn Future<Output=U> + Send + Sync>> = Box::pin(next);
        FSharpAsync_1 { future: Arc::from(Mutex::from(b)) }
    }

    pub fn r_return<T: Send + Sync + 'static>(item: T) -> FSharpAsync_1<T>{
        let r = ready(item);
        let b :Pin<Box<dyn Future<Output=T> + Send + Sync>> = Box::pin(r);
        FSharpAsync_1 { future: Arc::from(Mutex::from(b)) }
    }
}

pub mod AsyncBuilder_ {
    use std::{sync::Arc, pin::Pin, future::{Future, ready}};

    use futures::lock::Mutex;

    use super::Async_::FSharpAsync_1;

    pub fn delay<T: Send + Sync>(binder: Arc<impl Fn() -> Arc<FSharpAsync_1<T>> + 'static>) -> Arc<FSharpAsync_1<T>> {
        let pr = binder();
        Arc::from(FSharpAsync_1 {future: pr.future.clone()})
    }

    pub fn bind<T: Clone  + Send + Sync + 'static, U: Clone  + Send + Sync + 'static>(
        opt: Arc<FSharpAsync_1<T>>,
        binder: Arc<impl Fn(T) -> Arc<FSharpAsync_1<U>> + Send + Sync + 'static>
                         ) -> Arc<FSharpAsync_1<U>>
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
        Arc::from(FSharpAsync_1 { future: Arc::from(Mutex::from(b)) })
    }

    pub fn r_return<T: Send + Sync + 'static>(item: T) -> Arc<FSharpAsync_1<T>>{
        let r = ready(item);
        let b :Pin<Box<dyn Future<Output=T> + Send + Sync + 'static>> = Box::pin(r);
        Arc::from(FSharpAsync_1 { future: Arc::from(Mutex::from(b)) })
    }
}


pub mod Task_ {
    use std::{sync::{Mutex, Arc}, thread::{self, JoinHandle}, time::Duration};

    #[derive(Default, Clone)]
    pub struct Task_1<T: Sized + Clone + Send> {
        result: Arc<Mutex<Option<T>>>,
        handle: Arc<Mutex<Option<JoinHandle<()>>>>,
    }

    impl <T: Clone + Send> Task_1<T>{
        pub fn new() -> Task_1<T> {
            Task_1 { result: Arc::from(Mutex::default()), handle: Arc::from(Mutex::from(None)) }
        }

        pub fn set_result(self, value: T){
            let mut m = self.result.lock().unwrap();
            (*m).get_or_insert(value);
        }

        pub fn get_result(&self) -> T {
            while self.result.lock().unwrap().is_none() {
                thread::sleep(Duration::from_millis(10));
                let handle = self.handle.lock().unwrap().take();
                match handle {
                    Some(h) => {
                        h.join().expect("Join should work");
                    },
                    None => {}
                }
            }
            let t = self.result.lock().unwrap().as_ref().unwrap().clone();
            t
        }

        pub fn set_handle(&self, handle: JoinHandle<()>){
            self.handle.lock().unwrap().replace(handle);
        }
    }

    pub fn bind(){
        //todo
    }

    pub fn r_return(){
        //todo
    }

    pub fn from_result<T: Clone + Send>(t: T) -> Arc<Task_1<T>> {
        let t = Task_1 { result: Arc::from(Mutex::from(Some(t))), handle: Arc::from(Mutex::from(None)) };
        Arc::from(t)
    }
}