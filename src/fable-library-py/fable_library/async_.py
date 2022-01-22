from asyncio import Future, Task, ensure_future
import asyncio
from threading import Timer
from typing import Any, Awaitable, Callable, Iterable, List, Optional, TypeVar, Union
from .task import TaskCompletionSource

from .async_builder import (
    Async,
    CancellationToken,
    IAsyncContext,
    OperationCanceledError,
    Trampoline,
    empty_continuation,
    protected_bind,
    protected_cont,
    protected_return,
)

# F# generated code (from Choice.fs)
from .choice import (  # type: ignore
    Choice_makeChoice1Of2,
    Choice_makeChoice2Of2,
)

_T = TypeVar("_T")


default_cancellation_token = CancellationToken()

# see AsyncBuilder.Delay
def delay(generator: Callable[[], Async[_T]]):
    def cont(ctx: IAsyncContext[_T]):
        generator()(ctx)

    return protected_cont(cont)


def create_cancellation_token(arg: Union[int, bool, None] = None) -> CancellationToken:
    cancelled = arg if isinstance(arg, bool) else False
    token = CancellationToken(cancelled)
    if isinstance(arg, int):
        timer = Timer(arg / 1000.0, token.cancel)  # type: ignore
        timer.start()

    return token


def cancel(token: CancellationToken) -> None:
    token.cancel()


def cancel_after(token: CancellationToken, ms: int) -> None:
    timer = Timer(ms / 1000.0, token.cancel)
    timer.start()


def is_cancellation_requested(token: CancellationToken) -> bool:
    return token and token.is_cancelled


def sleep(millisecondsDueTime: int) -> Async[_T]:
    def cont(ctx: IAsyncContext[_T]):
        def cancel():
            timer.cancel()
            ctx.on_cancel(OperationCanceledError())

        token_id = ctx.cancel_token.add_listener(cancel)

        def timeout():
            ctx.cancel_token.remove_listener(token_id)
            ctx.on_success()

        timer = Timer(millisecondsDueTime / 1000.0, timeout)
        timer.start()

    return protected_cont(cont)


def ignore(computation: Async[_T]) -> Async[None]:
    def binder(_: Optional[_T] = None) -> Async[None]:
        return protected_return()

    return protected_bind(computation, binder)


def parallel(computations: Iterable[Async[_T]]) -> Async[List[_T]]:
    def delayed() -> Async[List[_T]]:
        all: Future[List[_T]] = asyncio.gather(*map(start_as_task, computations))

        return await_task(all)

    return delay(delayed)


def sequential(computations: Iterable[Async[_T]]) -> Async[List[_T]]:
    def delayed() -> Async[List[_T]]:
        async def sequence() -> List[_T]:
            results: List[_T] = []

            for c in computations:
                result = await start_as_task(c)
                results.append(result)

            return results

        return await_task(sequence())

    return delay(delayed)


def catch_async(work: Async[_T]) -> Async[_T]:
    def cont(ctx: IAsyncContext[_T]) -> None:
        def on_success(x: Optional[_T] = None):
            ctx.on_success(Choice_makeChoice1Of2(x))  # type: ignore

        def on_error(err: Exception):
            ctx.on_success(Choice_makeChoice2Of2(err))  # type: ignore

        ctx_ = IAsyncContext.create(
            on_success, on_error, ctx.on_cancel, ctx.trampoline, ctx.cancel_token
        )
        work(ctx_)

    return protected_cont(cont)


def from_continuations(
    f: Callable[[List[Callable[[Any], None]]], None]
) -> Callable[[IAsyncContext[Any]], None]:
    def cont(ctx: IAsyncContext[Any]) -> None:
        f([ctx.on_success, ctx.on_error, ctx.on_cancel])

    return protected_cont(cont)


def await_task(task: Awaitable[_T]) -> Async[_T]:
    continuation: List[Callable[[Any], None]] = []
    task = ensure_future(task)

    def done(tsk: Task[_T]) -> None:
        value = tsk.result()
        continuation[0](value)

    def callback(conts: List[Callable[[Any], None]]) -> None:
        nonlocal continuation
        continuation = conts

    task.add_done_callback(done)
    return from_continuations(callback)


def start_with_continuations(
    computation: Callable[[IAsyncContext[_T]], None],
    continuation: Optional[Callable[[Optional[_T]], None]] = None,
    exception_continuation: Optional[Callable[[Exception], None]] = None,
    cancellation_continuation: Optional[
        Callable[[OperationCanceledError], None]
    ] = None,
    cancellation_token: Optional[CancellationToken] = None,
) -> None:
    trampoline = Trampoline()

    ctx = IAsyncContext.create(
        continuation or empty_continuation,
        exception_continuation or empty_continuation,
        cancellation_continuation or empty_continuation,
        trampoline,
        cancellation_token or default_cancellation_token,
    )

    return computation(ctx)


def start_as_task(
    computation: Async[_T], cancellation_token: Optional[CancellationToken] = None
) -> Awaitable[_T]:
    task: TaskCompletionSource[_T] = TaskCompletionSource()

    def resolve(value: Optional[_T] = None) -> None:
        task.SetResult(value)

    def reject(error: Exception) -> None:
        task.SetException(error)

    def cancel(error: OperationCanceledError) -> None:
        task.SetCancelled()

    start_with_continuations(
        computation,
        resolve,
        reject,
        cancel,
        cancellation_token or default_cancellation_token,
    )
    return task.get_task()


def start(
    computation: Callable[[IAsyncContext[Any]], None],
    cancellation_token: Optional[CancellationToken] = None,
) -> None:
    return start_with_continuations(computation, cancellation_token=cancellation_token)


def start_immediate(
    computation: Callable[[IAsyncContext[Any]], None],
    cancellation_token: Optional[CancellationToken] = None,
) -> None:
    return start(computation, cancellation_token)


__all__ = [
    "await_task",
    "cancel",
    "cancel_after",
    "catch_async",
    "create_cancellation_token",
    "from_continuations",
    "ignore",
    "is_cancellation_requested",
    "sleep",
    "start",
    "start_immediate",
    "start_with_continuations",
]
