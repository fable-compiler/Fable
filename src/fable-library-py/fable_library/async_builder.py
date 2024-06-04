from __future__ import annotations

import asyncio
from abc import abstractmethod
from collections.abc import Callable, Iterable
from dataclasses import dataclass, field
from threading import Lock, RLock
from typing import (
    Any,
    Generic,
    Literal,
    Protocol,
    TypeVar,
    overload,
)

from .util import IDisposable


_T = TypeVar("_T")
_U = TypeVar("_U")
_D = TypeVar("_D", bound=IDisposable)


class OperationCanceledError(Exception):
    def __init__(self, msg: str | None = None) -> None:
        super().__init__(msg or "The operation was canceled")


Continuations = tuple[
    Callable[[_T], None],
    Callable[[Exception], None],
    Callable[[OperationCanceledError], None],
]


class _Listener(Protocol):
    def __call__(self, __state: Any | None = None) -> None: ...


class CancellationToken:
    __slots__ = "cancelled", "listeners", "idx", "lock"

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


class IAsyncContext(Generic[_T]):
    __slots__ = ()

    @abstractmethod
    def on_success(self, value: _T) -> None: ...

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
    def create(
        on_success: Callable[[_T], None] | None,
        on_error: Callable[[Exception], None] | None,
        on_cancel: Callable[[OperationCanceledError], None] | None,
        trampoline: Trampoline | None,
        cancel_token: CancellationToken | None,
    ) -> IAsyncContext[_T]:
        return AnonymousAsyncContext(on_success, on_error, on_cancel, trampoline, cancel_token)


""" FSharpAsync"""
Async = Callable[[IAsyncContext[_T]], None]


def empty_continuation(x: Any = None) -> None:
    pass


class AnonymousAsyncContext(IAsyncContext[_T]):
    __slots__ = "_on_success", "_on_error", "_on_cancel", "_trampoline", "_cancel_token"

    def __init__(
        self,
        on_success: Callable[[_T], None] | None = None,
        on_error: Callable[[Exception], None] | None = None,
        on_cancel: Callable[[OperationCanceledError], None] | None = None,
        trampoline: Trampoline | None = None,
        cancel_token: CancellationToken | None = None,
    ):
        self._on_success: Callable[[_T], None] = on_success or empty_continuation
        self._on_error: Callable[[Exception], None] = on_error or empty_continuation
        self._on_cancel: Callable[[OperationCanceledError], None] = on_cancel or empty_continuation

        self._cancel_token = cancel_token
        self._trampoline = trampoline

    def on_success(self, value: _T) -> None:
        return self._on_success(value)

    def on_error(self, error: Exception) -> None:
        return self._on_error(error)

    def on_cancel(self, error: OperationCanceledError) -> None:
        return self._on_cancel(error)

    @property
    @abstractmethod
    def trampoline(self) -> Trampoline:
        return self._trampoline

    @trampoline.setter
    @abstractmethod
    def trampoline(self, val: Trampoline):
        self._trampoline = val

    @property
    @abstractmethod
    def cancel_token(self) -> CancellationToken:
        return self._cancel_token

    @cancel_token.setter
    @abstractmethod
    def cancel_token(self, val: CancellationToken):
        self._cancel_token = val


@dataclass(order=True)
class ScheduledItem:
    due_time: float
    action: Callable[[], None] = field(compare=False)
    cancel_token: CancellationToken | None = field(compare=False)


class Trampoline:
    __slots__ = "lock", "running", "call_count"

    MaxTrampolineCallCount = 75  # Max recursion depth: 1000

    def __init__(self):
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


def protected_cont(f: Async[_T]) -> Async[_T]:
    def _protected_cont(ctx: IAsyncContext[_T]):
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


def protected_bind(
    computation: Callable[[IAsyncContext[_T]], None],
    binder: Callable[[_T], Async[_U]],
) -> Async[_U]:
    def cont(ctx: IAsyncContext[_U]) -> None:
        def on_success(x: _T) -> None:
            try:
                binder(x)(ctx)
            except Exception as err:
                # print("Exception: ", err)
                ctx.on_error(err)

        ctx_ = IAsyncContext.create(on_success, ctx.on_error, ctx.on_cancel, ctx.trampoline, ctx.cancel_token)
        return computation(ctx_)

    return protected_cont(cont)


def protected_return(value: _T) -> Async[_T]:
    def f(ctx: IAsyncContext[_T]) -> None:
        return ctx.on_success(value)

    return protected_cont(f)


class AsyncBuilder:
    __slots__ = ()

    def Bind(self, computation: Async[_T], binder: Callable[[_T], Async[_U]]) -> Async[_U]:
        return protected_bind(computation, binder)

    def Combine(self, computation1: Async[Any], computation2: Async[_T]) -> Async[_T]:
        def binder(_: _T) -> Async[_T]:
            return computation2

        return self.Bind(computation1, binder)

    def Delay(self, generator: Callable[[], Async[_T]]) -> Async[_T]:
        return protected_cont(lambda ctx: generator()(ctx))

    def For(self, sequence: Iterable[_T], body: Callable[[_T], Async[_U]]) -> Async[_U]:
        done: bool = False
        it = iter(sequence)
        try:
            cur = next(it)
        except StopIteration:
            done = True

        def delay() -> Async[_U]:
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
    def Return(self, value: _T) -> Async[_T]: ...

    def Return(self, value: Any = None) -> Async[Any]:
        return protected_return(value)

    def ReturnFrom(self, computation: Async[_T]) -> Async[_T]:
        return computation

    def TryFinally(self, computation: Async[_T], compensation: Callable[[], None]) -> Async[_T]:
        def cont(ctx: IAsyncContext[_T]) -> None:
            def on_success(x: _T) -> None:
                compensation()
                ctx.on_success(x)

            def on_error(x: Exception) -> None:
                compensation()
                ctx.on_error(x)

            def on_cancel(x: OperationCanceledError) -> None:
                compensation()
                ctx.on_cancel(x)

            ctx_ = IAsyncContext.create(on_success, on_error, on_cancel, ctx.trampoline, ctx.cancel_token)
            computation(ctx_)

        return protected_cont(cont)

    def TryWith(self, computation: Async[_T], catch_handler: Callable[[Exception], Async[_T]]) -> Async[_T]:
        def fn(ctx: IAsyncContext[_T]):
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

    def Using(self, resource: _D, binder: Callable[[_D], Async[_U]]) -> Async[_U]:
        def compensation() -> None:
            return resource.Dispose()

        return self.TryFinally(binder(resource), compensation)

    @overload
    def While(self, guard: Callable[[], bool], computation: Async[Literal[None]]) -> Async[None]: ...

    @overload
    def While(self, guard: Callable[[], bool], computation: Async[_T]) -> Async[_T]: ...

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
