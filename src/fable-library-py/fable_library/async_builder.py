from __future__ import annotations

import asyncio
from abc import abstractmethod
from collections.abc import Callable, Iterable
from dataclasses import dataclass, field
from threading import Lock, RLock
from typing import (
    Any,
    Literal,
    Protocol,
    overload,
)

from .util import IDisposable


class OperationCanceledError(Exception):
    def __init__(self, msg: str | None = None) -> None:
        super().__init__(msg or "The operation was canceled")


type Continuations[T] = tuple[
    Callable[[T], None],
    Callable[[Exception], None],
    Callable[[OperationCanceledError], None],
]


class _Listener(Protocol):
    def __call__(self, __state: Any | None = None) -> None: ...


class CancellationToken:
    __slots__ = "cancelled", "idx", "listeners", "lock"

    def __init__(self, cancelled: bool = False):
        self.cancelled = cancelled
        self.listeners: dict[int, Callable[[], None]] = {}
        self.idx = 0
        self.lock = RLock()

    @property
    def is_cancelled(self):
        return self.cancelled

    is_cancellation_requested = is_cancelled

    def cancel(self) -> None:
        cancel = False
        with self.lock:
            if not self.cancelled:
                cancel = True
                self.cancelled = True

        if cancel:
            for listener in self.listeners.values():
                listener()

    def add_listener(self, f: Callable[[], None]) -> int:
        with self.lock:
            id = self.idx
            self.idx = self.idx + 1
            self.listeners[id] = f

        return id

    def remove_listener(self, id: int) -> None:
        with self.lock:
            del self.listeners[id]

    def register(self, f: _Listener, state: Any | None = None) -> None:
        if state:
            id = self.add_listener(lambda: f(state))
        else:
            id = self.add_listener(f)

        def dispose():
            self.remove_listener(id)

        IDisposable.create(dispose)


class IAsyncContext[T]:
    __slots__ = ()

    @abstractmethod
    def on_success(self, value: T) -> None: ...

    @abstractmethod
    def on_error(self, error: Exception) -> None: ...

    @abstractmethod
    def on_cancel(self, error: OperationCanceledError) -> None: ...

    @property
    @abstractmethod
    def trampoline(self) -> Trampoline: ...

    @trampoline.setter
    @abstractmethod
    def trampoline(self, val: Trampoline): ...

    @property
    @abstractmethod
    def cancel_token(self) -> CancellationToken: ...

    @cancel_token.setter
    @abstractmethod
    def cancel_token(self, val: CancellationToken): ...

    @staticmethod
    def create[U](
        trampoline: Trampoline,
        cancel_token: CancellationToken,
        on_success: Callable[[U], None] | None,
        on_error: Callable[[Exception], None] | None,
        on_cancel: Callable[[OperationCanceledError], None] | None,
    ) -> IAsyncContext[U]:
        return AnonymousAsyncContext(trampoline, cancel_token, on_success, on_error, on_cancel)


""" FSharpAsync"""
type Async[T] = Callable[[IAsyncContext[T]], None]


def empty_continuation(x: Any = None) -> None:
    pass


class AnonymousAsyncContext[T](IAsyncContext[T]):
    __slots__ = "_cancel_token", "_on_cancel", "_on_error", "_on_success", "_trampoline"

    def __init__(
        self,
        trampoline: Trampoline,
        cancel_token: CancellationToken,
        on_success: Callable[[T], None] | None = None,
        on_error: Callable[[Exception], None] | None = None,
        on_cancel: Callable[[OperationCanceledError], None] | None = None,
    ) -> None:
        self._on_success: Callable[[T], None] = on_success or empty_continuation
        self._on_error: Callable[[Exception], None] = on_error or empty_continuation
        self._on_cancel: Callable[[OperationCanceledError], None] = on_cancel or empty_continuation

        self._cancel_token = cancel_token
        self._trampoline = trampoline

    def on_success(self, value: T) -> None:
        return self._on_success(value)

    def on_error(self, error: Exception) -> None:
        return self._on_error(error)

    def on_cancel(self, error: OperationCanceledError) -> None:
        return self._on_cancel(error)

    @property
    def trampoline(self) -> Trampoline:
        return self._trampoline

    @trampoline.setter
    def trampoline(self, val: Trampoline):
        self._trampoline = val

    @property
    def cancel_token(self) -> CancellationToken:
        return self._cancel_token

    @cancel_token.setter
    def cancel_token(self, val: CancellationToken):
        self._cancel_token = val


@dataclass(order=True)
class ScheduledItem:
    due_time: float
    action: Callable[[], None] = field(compare=False)
    cancel_token: CancellationToken | None = field(compare=False)


class Trampoline:
    __slots__ = "call_count", "lock", "running"

    MaxTrampolineCallCount = 75  # Max recursion depth: 1000

    def __init__(self) -> None:
        self.call_count: int = 0
        self.lock = Lock()
        self.running = False

    def increment_and_check(self):
        with self.lock:
            self.call_count = self.call_count + 1
            return self.call_count > Trampoline.MaxTrampolineCallCount

    def run_later(
        self,
        action: Callable[[], None],
        due_time: float = 0.0,
    ):
        loop = asyncio.get_event_loop()
        loop.call_later(due_time, action)

    def run(self, action: Callable[[], None]):
        loop = asyncio.get_event_loop()

        if self.increment_and_check():
            self.call_count = 0
            loop.call_soon(action)
        else:
            action()


def protected_cont[T](f: Async[T]) -> Async[T]:
    def _protected_cont(ctx: IAsyncContext[T]):
        if ctx.cancel_token and ctx.cancel_token.is_cancelled:
            ctx.on_cancel(OperationCanceledError())

        def fn():
            try:
                return f(ctx)
            except Exception as err:
                # print("Exception: ", err)
                ctx.on_error(err)

        ctx.trampoline.run(fn)

    return _protected_cont


def protected_bind[T, U](
    computation: Callable[[IAsyncContext[T]], None],
    binder: Callable[[T], Async[U]],
) -> Async[U]:
    def cont(ctx: IAsyncContext[U]) -> None:
        def on_success(x: T) -> None:
            try:
                binder(x)(ctx)
            except Exception as err:
                # print("Exception: ", err)
                ctx.on_error(err)

        ctx_ = IAsyncContext.create(ctx.trampoline, ctx.cancel_token, on_success, ctx.on_error, ctx.on_cancel)
        return computation(ctx_)

    return protected_cont(cont)


def protected_return[T](value: T) -> Async[T]:
    def f(ctx: IAsyncContext[T]) -> None:
        return ctx.on_success(value)

    return protected_cont(f)


class AsyncBuilder:
    __slots__ = ()

    def Bind[T, U](self, computation: Async[T], binder: Callable[[T], Async[U]]) -> Async[U]:
        return protected_bind(computation, binder)

    def Combine[T](self, computation1: Async[Any], computation2: Async[T]) -> Async[T]:
        def binder(_: T) -> Async[T]:
            return computation2

        return self.Bind(computation1, binder)

    def Delay[T](self, generator: Callable[[], Async[T]]) -> Async[T]:
        return protected_cont(lambda ctx: generator()(ctx))

    def For[T, U](self, sequence: Iterable[T], body: Callable[[T], Async[None]]) -> Async[None]:
        done: bool = False
        it = iter(sequence)
        try:
            cur = next(it)
        except StopIteration:
            done = True

        def delay() -> Async[None]:
            nonlocal cur, done
            res = body(cur)
            try:
                cur = next(it)
            except StopIteration:
                done = True
            return res

        return self.While(lambda: not done, self.Delay(delay))

    @overload
    def Return(self) -> Async[None]: ...

    @overload
    def Return[T](self, value: T) -> Async[T]: ...

    def Return(self, value: Any = None) -> Async[Any]:
        return protected_return(value)

    def ReturnFrom[T](self, computation: Async[T]) -> Async[T]:
        return computation

    def TryFinally[T](self, computation: Async[T], compensation: Callable[[], None]) -> Async[T]:
        def cont(ctx: IAsyncContext[T]) -> None:
            def on_success(x: T) -> None:
                compensation()
                ctx.on_success(x)

            def on_error(x: Exception) -> None:
                compensation()
                ctx.on_error(x)

            def on_cancel(x: OperationCanceledError) -> None:
                compensation()
                ctx.on_cancel(x)

            ctx_ = IAsyncContext.create(ctx.trampoline, ctx.cancel_token, on_success, on_error, on_cancel)
            computation(ctx_)

        return protected_cont(cont)

    def TryWith[T](self, computation: Async[T], catch_handler: Callable[[Exception], Async[T]]) -> Async[T]:
        def fn(ctx: IAsyncContext[T]):
            def on_error(err: Exception) -> None:
                try:
                    catch_handler(err)(ctx)
                except Exception as ex2:
                    ctx.on_error(ex2)

            ctx_ = IAsyncContext.create(
                on_success=ctx.on_success,
                on_cancel=ctx.on_cancel,
                cancel_token=ctx.cancel_token,
                trampoline=ctx.trampoline,
                on_error=on_error,
            )

            return computation(ctx_)

        return protected_cont(fn)

    def Using[D: IDisposable, U](self, resource: D, binder: Callable[[D], Async[U]]) -> Async[U]:
        def compensation() -> None:
            return resource.Dispose()

        return self.TryFinally(binder(resource), compensation)

    @overload
    def While(self, guard: Callable[[], bool], computation: Async[Literal[None]]) -> Async[None]: ...

    @overload
    def While[T](self, guard: Callable[[], bool], computation: Async[T]) -> Async[T]: ...

    def While(self, guard: Callable[[], bool], computation: Async[Any]) -> Async[Any]:
        if guard():

            def binder(_: Any) -> Async[Any]:
                return self.While(guard, computation)

            return self.Bind(computation, binder)
        else:
            return self.Return()

    def Zero(self) -> Async[None]:
        return protected_cont(lambda ctx: ctx.on_success(None))


singleton = AsyncBuilder()
