from asyncio import Task, ensure_future
from threading import Timer
from typing import Any, Awaitable, Callable, List, Optional, TypeVar, Union

from .async_builder import (
    CancellationToken,
    Async,
    IAsyncContext,
    OperationCanceledError,
    Trampoline,
    empty_continuation,
    protected_bind,
    protected_cont,
    protected_return,
)
from .choice import Choice_makeChoice1Of2, Choice_makeChoice2Of2  # type: ignore , F# generated from Choice.fs

_T = TypeVar("_T")


default_cancellation_token = CancellationToken()


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


def catch_async(work: Async[_T]) -> Async[_T]:
    def cont(ctx: IAsyncContext[_T]) -> None:
        def on_success(x: Optional[_T] = None):
            ctx.on_success(Choice_makeChoice1Of2(x))  # type: ignore

        def on_error(err: Exception):
            ctx.on_success(Choice_makeChoice2Of2(err))  # type: ignore

        ctx_ = IAsyncContext.create(on_success, on_error, ctx.on_cancel, ctx.trampoline, ctx.cancel_token)
        work(ctx_)

    return protected_cont(cont)


def from_continuations(f: Callable[[List[Callable[[Any], None]]], None]) -> Callable[[IAsyncContext[Any]], None]:
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
    cancellation_continuation: Optional[Callable[[OperationCanceledError], None]] = None,
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


def start(
    computation: Callable[[IAsyncContext[Any]], None], cancellation_token: Optional[CancellationToken] = None
) -> None:
    return start_with_continuations(computation, cancellation_token=cancellation_token)


def start_immediate(
    computation: Callable[[IAsyncContext[Any]], None], cancellation_token: Optional[CancellationToken] = None
) -> None:
    return start(computation, cancellation_token)
