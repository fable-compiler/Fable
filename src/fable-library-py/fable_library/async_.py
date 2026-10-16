from __future__ import annotations

import asyncio
from asyncio import Future, ensure_future
from collections.abc import Awaitable, Callable
from concurrent.futures import ThreadPoolExecutor
from threading import RLock, Timer
from typing import (
    Any,
    cast,
)

from .array_ import Array
from .async_builder import (
    Async,
    CancellationToken,
    Continuations,
    IAsyncContext,
    OperationCanceledError,
    TaskCanceledError,
    Trampoline,
    empty_continuation,
    protected_bind,
    protected_cont,
    protected_return,
    singleton,
)

# F# generated code (from Choice.fs)
from .choice import (
    Choice_makeChoice1Of2,
    Choice_makeChoice2Of2,
    FSharpChoice_2,
)
from .event import IEvent_2
from .protocols import IEnumerable_1
from .system import ArgumentException__ctor_Z721C83C5, InvalidOperationException__ctor_Z721C83C5, TimeoutException__ctor
from .task import TaskCompletionSource
from .time_span import TimeSpan, to_milliseconds
from .types import UNIT, Unit


def cancellation_token() -> Async[CancellationToken]:
    def cont(ctx: IAsyncContext[Any]):
        ctx.on_success(ctx.cancel_token)

    return protected_cont(cont)


default_cancellation_token = CancellationToken()


# see AsyncBuilder.Delay
def delay[T](generator: Callable[[], Async[T]]):
    def cont(ctx: IAsyncContext[T]):
        generator()(ctx)

    return protected_cont(cont)


def create_cancellation_token(arg: int | bool | None = None) -> CancellationToken:
    cancelled = arg if isinstance(arg, bool) else False
    token = CancellationToken(cancelled)
    if isinstance(arg, int) and not isinstance(arg, bool):
        timer = Timer(arg / 1000.0, token.cancel)
        timer.start()

    return token


def cancel(token: CancellationToken) -> None:
    token.cancel()


def cancel_after(token: CancellationToken, ms: int | TimeSpan) -> None:
    ms_value = to_milliseconds(ms)
    timer = Timer(ms_value / 1000.0, token.cancel)
    timer.start()


def is_cancellation_requested(token: CancellationToken) -> bool:
    return token.is_cancelled


def call_on_owner_loop(loop: asyncio.AbstractEventLoop, action: Callable[[], None]) -> None:
    # decision: foreign-thread cancellation settles on the owning loop while it is running
    if loop.is_closed() or not loop.is_running():
        action()
        return
    try:
        current_loop = asyncio.get_running_loop()
    except RuntimeError:
        current_loop = None
    if current_loop is loop:
        action()
        return
    try:
        loop.call_soon_threadsafe(action)
    except RuntimeError:
        if not loop.is_closed():
            raise
        action()


def sleep(milliseconds_duetime: int | TimeSpan) -> Async[None]:
    def cont(ctx: IAsyncContext[None]) -> None:
        loop = asyncio.get_running_loop()
        gate = RLock()
        completed = False
        timer: asyncio.TimerHandle | None = None
        token_id: int | None = None

        def settle(cancelled: bool) -> None:
            nonlocal completed
            with gate:
                if completed:
                    return
                cancelled = cancelled or ctx.cancel_token.is_cancelled
                completed = True
                if timer is not None:
                    timer.cancel()
                listener_id = token_id
            # Dispose outside the gate: another thread may already be running cancel().
            if listener_id is not None:
                ctx.cancel_token.remove_listener(listener_id)
            if cancelled:
                ctx.on_cancel(OperationCanceledError())
            else:
                ctx.on_success(None)

        def cancel() -> None:
            call_on_owner_loop(loop, lambda: settle(True))

        # invariant: installation and settlement cannot interleave before both resources are owned
        try:
            with gate:
                token_id = ctx.cancel_token.add_listener(cancel)
                if not completed:
                    due_time = to_milliseconds(milliseconds_duetime) / 1000.0
                    timer = ctx.trampoline.run_later(lambda: settle(False), due_time)
                    if completed:
                        timer.cancel()
        except Exception:
            with gate:
                completed = True
            if token_id is not None:
                ctx.cancel_token.remove_listener(token_id)
            raise

    return protected_cont(cont)


def await_event[T](event: IEvent_2[Any, T], cancel_action: Callable[[], None] | None = None) -> Async[T]:
    def cont(ctx: IAsyncContext[T]) -> None:
        token_id: list[int] = [0]

        def handler(_sender: Any, arg: T) -> None:
            ctx.cancel_token.remove_listener(token_id[0])
            event.RemoveHandler(handler)
            ctx.on_success(arg)

        def cancel() -> None:
            event.RemoveHandler(handler)
            if cancel_action is not None:
                cancel_action()
            ctx.on_cancel(OperationCanceledError())

        token_id[0] = ctx.cancel_token.add_listener(cancel)
        if ctx.cancel_token.is_cancelled:
            return
        event.AddHandler(handler)

    return protected_cont(cont)


def ignore(computation: Async[Any]) -> Async[None]:
    def binder(_: Any | Unit = UNIT) -> Async[None]:
        return protected_return(None)

    return protected_bind(computation, binder)


def parallel[T](computations: IEnumerable_1[Async[T]], max_degree_of_parallelism: int | None = None) -> Async[Array[T]]:
    if max_degree_of_parallelism is not None and max_degree_of_parallelism < 1:
        raise ArgumentException__ctor_Z721C83C5("maxDegreeOfParallelism must be positive")

    def cont(ctx: IAsyncContext[Array[T]]) -> None:
        works = list(computations)
        token = CancellationToken()
        limit = max_degree_of_parallelism or max(1, len(works))
        semaphore = asyncio.Semaphore(limit)

        async def run_one(work: Async[T]) -> T:
            async with semaphore:
                try:
                    return await start_as_task(work, cancellation_token=token)
                except asyncio.CancelledError as error:
                    raise TaskCanceledError() from error

        async def run_all() -> Array[T]:
            # decision: retain the first failure while cancellation settles every started sibling.
            registration = ctx.cancel_token.register(cancel_children)
            try:
                tasks = [asyncio.create_task(run_one(work)) for work in works]
                first_error: Exception | None = None
                for completed in asyncio.as_completed(tasks):
                    try:
                        await completed
                    except Exception as error:
                        if first_error is None:
                            first_error = error
                            cancel_children()
                if first_error is not None:
                    raise first_error
                return Array[T]([task.result() for task in tasks])
            finally:
                registration.Dispose()

        def cancel_children() -> None:
            try:
                token.cancel()
            except Exception:
                pass

        def report_error(error: Exception) -> None:
            if ctx.cancel_token.is_cancelled and isinstance(error, OperationCanceledError):
                ctx.on_cancel(error)
            else:
                ctx.on_error(error)

        child_ctx = IAsyncContext.create(ctx.trampoline, ctx.cancel_token, ctx.on_success, report_error, ctx.on_cancel)
        await_task(run_all())(child_ctx)

    return protected_cont(cont)


def choice[T](computations: IEnumerable_1[Async[T | None]]) -> Async[T | None]:
    def cont(ctx: IAsyncContext[T | None]) -> None:
        works = list(computations)
        if not works:
            ctx.on_success(None)
            return
        token = CancellationToken()

        def cancel_children() -> None:
            try:
                token.cancel()
            except Exception:
                pass

        async def run_choice() -> T | None:
            registration = ctx.cancel_token.register(cancel_children)
            tasks = [asyncio.ensure_future(start_as_task(work, cancellation_token=token)) for work in works]
            # Observe sibling failures after a winner cancels them.
            asyncio.gather(*tasks, return_exceptions=True)
            try:
                for completed in asyncio.as_completed(tasks):
                    result = await completed
                    if result is not None:
                        cancel_children()
                        return result
                return None
            except Exception:
                cancel_children()
                raise
            finally:
                registration.Dispose()

        await_task(run_choice())(ctx)

    return protected_cont(cont)


class _OnCancelRegistration:
    def __init__(self, token: CancellationToken, interruption: Callable[[], None]) -> None:
        self._token = token
        self._interruption = interruption
        self._interrupted = False
        self._gate = RLock()
        self._listener_id = token.add_listener(self._interrupt)

    def _interrupt(self) -> None:
        with self._gate:
            if self._interrupted:
                return
            self._interrupted = True
        self._interruption()

    def Dispose(self) -> None:
        self._token.remove_listener(self._listener_id)
        # decision: a use! finalizer may dispose during cancellation before its callback is visited.
        if self._token.is_cancelled:
            try:
                self._interrupt()
            except Exception:
                pass


def on_cancel(interruption: Callable[[], None]) -> Async[_OnCancelRegistration]:
    def cont(ctx: IAsyncContext[_OnCancelRegistration]) -> None:
        ctx.on_success(_OnCancelRegistration(ctx.cancel_token, interruption))

    return protected_cont(cont)


def sequential[T](computations: IEnumerable_1[Async[T]]) -> Async[Array[T]]:
    def delayed() -> Async[Array[T]]:
        results: list[T] = []

        def _arrow20(_arg: Async[T]) -> Async[None]:
            cmp: Async[T] = _arg

            def _arrow19(_arg_1: T) -> Async[None]:
                result: T = _arg_1
                (results.append(result))
                return singleton.Zero()

            return singleton.Bind(cmp, _arrow19)

        def _arrow21(__unit: Unit = UNIT) -> Async[Array[T]]:
            return singleton.Return(Array[T](results))

        return singleton.Combine(singleton.For(computations, _arrow20), singleton.Delay(_arrow21))

    return delay(delayed)


def try_cancelled[T](computation: Async[T], compensation: Callable[[OperationCanceledError], None]) -> Async[T]:
    def cont(ctx: IAsyncContext[T]) -> None:
        def on_cancel(error: OperationCanceledError) -> None:
            # decision: preserves the original cancellation when compensation throws, matching .NET
            try:
                compensation(error)
            except Exception:
                pass
            ctx.trampoline.run(lambda: ctx.on_cancel(error))

        ctx_ = IAsyncContext.create(ctx.trampoline, ctx.cancel_token, ctx.on_success, ctx.on_error, on_cancel)
        # invariant: installs the handler without adding a cancellation checkpoint, including for nested handlers
        computation(ctx_)

    return cont


def catch_async[T](work: Async[T]) -> Async[FSharpChoice_2[T, Exception]]:
    def cont(ctx: IAsyncContext[FSharpChoice_2[T, Exception]]) -> None:
        def on_success(x: T):
            ctx.on_success(Choice_makeChoice1Of2(x))

        def on_error(err: Exception):
            ctx.on_success(Choice_makeChoice2Of2(err))

        ctx_ = IAsyncContext.create(ctx.trampoline, ctx.cancel_token, on_success, on_error, ctx.on_cancel)
        work(ctx_)

    return protected_cont(cont)


def from_continuations[T](f: Callable[[Continuations[T]], None]) -> Async[T]:
    def cont(ctx: IAsyncContext[T]) -> None:
        settled = False
        gate = RLock()

        def claim() -> None:
            nonlocal settled
            with gate:
                if settled:
                    raise InvalidOperationException__ctor_Z721C83C5("A continuation was invoked more than once")
                settled = True

        # Compiled F# calls the success continuation with no argument when T is unit
        def success(value: T = cast(T, None)) -> None:
            claim()
            ctx.on_success(value)

        def error(value: Exception) -> None:
            claim()
            ctx.on_error(value)

        def cancelled(value: OperationCanceledError) -> None:
            claim()
            ctx.on_cancel(value)

        def on_exception(value: Exception) -> None:
            with gate:
                completed = settled
            if completed:
                raise value
            error(value)

        guarded = IAsyncContext.create(ctx.trampoline, ctx.cancel_token, success, on_exception, cancelled)
        protected_cont(lambda _: f((success, error, cancelled)))(guarded)

    return cont


def from_continuations_unit(
    f: Callable[
        [tuple[Callable[[], None], Callable[[Exception], None], Callable[[OperationCanceledError], None]]],
        None,
    ],
) -> Async[None]:
    def callback(conts: Continuations[None]) -> None:
        success, error, cancelled = conts

        # Generic callers pass None explicitly; specialized F# unit callers erase it.
        def unit_success(value: None = None) -> None:
            success(value)

        f((unit_success, error, cancelled))

    return from_continuations(callback)


def await_task[T](task: Awaitable[T]) -> Async[T]:
    """Return an asynchronous computation that will wait for the given
    task to complete and return its result.
    """
    continuation: Continuations[T] = (
        empty_continuation,
        empty_continuation,
        empty_continuation,
    )
    task = ensure_future(task)

    def done(tsk: Future[T]) -> None:
        try:
            value = tsk.result()
        except Exception as ex:
            continuation[1](ex)
        else:
            continuation[0](value)

    def callback(conts: Continuations[T]) -> None:
        nonlocal continuation
        continuation = conts

    task.add_done_callback(done)
    return from_continuations(callback)


def await_task_with_cancellation[T](task: Awaitable[T]) -> Async[T]:
    async def observe() -> T:
        try:
            return await task
        except asyncio.CancelledError as error:
            # decision: cancelled tasks are ordinary AwaitTask failures, matching .NET TaskCanceledException
            raise TaskCanceledError() from error

    return await_task(observe())


def run_in_loop(computation: Callable[..., None]) -> Any:
    """Run a computation on the event loop.

    If no event loop is running, then one is created and run.
    """

    async def runner() -> None:
        return computation()

    try:
        asyncio.get_running_loop()
    except RuntimeError:
        return asyncio.run(runner())
    else:
        return computation()


def start_with_continuations[T](
    computation: Async[T],
    continuation: Callable[[T], None] | None = None,
    exception_continuation: Callable[[Exception], None] | None = None,
    cancellation_continuation: Callable[[OperationCanceledError], None] | None = None,
    cancellation_token: CancellationToken | None = None,
) -> None:
    """Runs an asynchronous computation.

    Runs an asynchronous computation, starting immediately on the
    current operating system thread. Call one of the three continuations
    when the operation completes.

    If no cancellation token is provided then the default cancellation
    token is used."""
    trampoline = Trampoline()

    def done[U](cont: Callable[[U], None]) -> Callable[[U], None]:
        def terminal(value: U) -> None:
            trampoline.completed = True
            cont(value)

        return terminal

    ctx = IAsyncContext.create(
        trampoline,
        cancellation_token or default_cancellation_token,
        done(continuation or empty_continuation),
        done(exception_continuation or empty_continuation),
        done(cancellation_continuation or empty_continuation),
    )

    def runner() -> None:
        computation(ctx)

    run_in_loop(runner)


def start_as_task[T](
    computation: Async[T],
    task_creation_options: Any = None,
    cancellation_token: CancellationToken | None = None,
) -> Awaitable[T]:
    """Executes a computation in the thread pool.

    If no cancellation token is provided then the default cancellation
    token is used. The task_creation_options parameter is ignored (for .NET compatibility).
    """
    tcs: TaskCompletionSource[T] = TaskCompletionSource()

    def resolve(value: T) -> None:
        tcs.SetResult(value)

    def reject(error: Exception) -> None:
        tcs.SetException(error)

    def cancel(_: OperationCanceledError) -> None:
        tcs.SetCancelled()

    start_with_continuations(
        computation,
        resolve,
        reject,
        cancel,
        cancellation_token or default_cancellation_token,
    )
    return tcs.get_task()


def start_immediate_as_task[T](
    computation: Async[T], cancellation_token: CancellationToken | None = None
) -> Awaitable[T]:
    return start_as_task(computation, cancellation_token=cancellation_token)


def _start_child_timeout[T](computation: Async[T], ms: int | TimeSpan) -> Async[Async[T]]:
    def cont(ctx: IAsyncContext[Async[T]]) -> None:
        # The child uses a linked token, so its deadline never cancels the parent.
        child_token = CancellationToken()
        registration = ctx.cancel_token.register(child_token.cancel)
        task = asyncio.ensure_future(start_as_task(computation, cancellation_token=child_token))

        async def with_timeout() -> T:
            try:
                return await asyncio.wait_for(task, timeout=to_milliseconds(ms) / 1000.0)
            except TimeoutError:
                # invariant: a completed child retains its exception; only deadline cancellation is translated.
                if not task.cancelled():
                    raise
                try:
                    child_token.cancel()
                except Exception:
                    pass
                raise TimeoutException__ctor()
            finally:
                registration.Dispose()

        protected_return(await_task(with_timeout()))(ctx)

    return protected_cont(cont)


def start_child[T](computation: Async[T], ms: int | TimeSpan | None = None) -> Async[Async[T]]:
    # decision: share TimeoutException across callers; its native base preserves Python TimeoutError handlers
    if ms is not None:
        return _start_child_timeout(computation, ms)

    def cont(ctx: IAsyncContext[Async[T]]) -> None:
        task = start_as_task(computation, cancellation_token=ctx.cancel_token)
        protected_return(await_task(task))(ctx)

    return protected_cont(cont)


def start_immediate(
    computation: Async[Any],
    cancellation_token: CancellationToken | None = None,
) -> None:
    """Start computation immediately.

    Runs an asynchronous computation, starting immediately on the
    current operating system thread
    """
    start_with_continuations(computation, cancellation_token=cancellation_token)


_executor: ThreadPoolExecutor | None = None


def start(
    computation: Callable[[IAsyncContext[Any]], None],
    cancellation_token: CancellationToken | None = None,
) -> None:
    """Starts the asynchronous computation.

    Starts the asynchronous computation in the thread pool. Do not await
    its result.

    If no cancellation token is provided then the default cancellation
    token is used."""
    global _executor

    def worker() -> None:
        start_immediate(computation, cancellation_token)

    if not _executor:
        _executor = ThreadPoolExecutor(max_workers=16)
    _executor.submit(worker)
    return


def run_synchronously[T](
    computation: Async[T],
    cancellation_token: CancellationToken | None = None,
) -> T | None:
    """Run computation synchronously.

    Runs an asynchronous computation and awaits its result on the
    calling thread. Propagates an exception should the computation yield
    one. This call is blocking.
    """

    async def runner() -> T:
        # Set a custom exception handler to suppress CancelledError noise during shutdown
        loop = asyncio.get_running_loop()

        def exception_handler(loop: asyncio.AbstractEventLoop, context: dict[str, Any]) -> None:
            # Suppress CancelledError exceptions during shutdown
            exception = context.get("exception")
            if isinstance(exception, asyncio.CancelledError):
                return
            # For other exceptions, use the default handler
            loop.default_exception_handler(context)

        loop.set_exception_handler(exception_handler)
        return await start_as_task(computation, cancellation_token=cancellation_token)

    return asyncio.run(runner())


__all__ = [
    "await_event",
    "await_task",
    "await_task_with_cancellation",
    "cancel",
    "cancel_after",
    "cancellation_token",
    "catch_async",
    "create_cancellation_token",
    "delay",
    "from_continuations",
    "ignore",
    "is_cancellation_requested",
    "parallel",
    "sequential",
    "sleep",
    "start",
    "start_as_task",
    "start_child",
    "start_immediate",
    "start_with_continuations",
    "try_cancelled",
]
