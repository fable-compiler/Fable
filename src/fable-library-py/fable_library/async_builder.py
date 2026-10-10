from __future__ import annotations

import asyncio
from abc import abstractmethod
from collections.abc import Callable
from dataclasses import dataclass, field
from threading import Condition, Lock, RLock, get_ident
from typing import (
    Any,
    Literal,
    overload,
)

from .protocols import IEnumerable_1
from .util import IDisposable


class OperationCanceledError(Exception):
    def __init__(self, msg: str | None = None) -> None:
        super().__init__(msg or "The operation was canceled")


type Continuations[T] = tuple[
    Callable[[T], None],
    Callable[[Exception], None],
    Callable[[OperationCanceledError], None],
]


_NO_STATE = object()


class CancellationCallbackError(ExceptionGroup[Exception]):
    @property
    def inner_exception(self) -> Exception:
        # invariant: F# InnerException exposes the first failure without discarding the remaining errors
        return self.exceptions[0]


class CancellationToken:
    __slots__ = "_condition", "_running", "cancelled", "idx", "listeners", "lock"

    def __init__(self, cancelled: bool = False) -> None:
        self.cancelled = cancelled
        self.listeners: dict[int, Callable[[], None]] = {}
        self.idx = 0
        self.lock = RLock()
        self._condition = Condition(self.lock)
        self._running: dict[int, int] = {}

    @property
    def is_cancelled(self) -> bool:
        with self.lock:
            return self.cancelled

    is_cancellation_requested = is_cancelled

    def cancel(self) -> None:
        with self.lock:
            if self.cancelled:
                return
            self.cancelled = True
            # decision: snapshots IDs newest first to match .NET while allowing disposal of pending callbacks
            pending = list(reversed(self.listeners))

        errors: list[Exception] = []
        for listener_id in pending:
            try:
                self._invoke_listener(listener_id)
            except Exception as error:
                errors.append(error)
        if errors:
            raise CancellationCallbackError("Cancellation callbacks failed", errors)

    def _invoke_listener(self, listener_id: int) -> None:
        with self.lock:
            listener = self.listeners.pop(listener_id, None)
            if listener is None:
                return
            self._running[listener_id] = get_ident()
        # invariant: user callbacks execute outside the registry lock
        try:
            listener()
        finally:
            with self._condition:
                del self._running[listener_id]
                self._condition.notify_all()

    def add_listener(self, f: Callable[[], None]) -> int:
        with self.lock:
            listener_id = self.idx
            self.idx += 1
            if not self.cancelled:
                self.listeners[listener_id] = f
                return listener_id
        # .NET invokes late registrations synchronously and does not retain them.
        f()
        return listener_id

    def remove_listener(self, id: int) -> None:
        with self._condition:
            self.listeners.pop(id, None)
            # invariant: disposal waits for a running callback except when that callback disposes itself
            while id in self._running and self._running[id] != get_ident():
                self._condition.wait()

    @overload
    def register(self, f: Callable[[], None]) -> IDisposable: ...

    @overload
    def register[S](self, f: Callable[[S], None], state: S) -> IDisposable: ...

    def register(self, f: Callable[..., None], state: object = _NO_STATE) -> IDisposable:
        listener_id = self.add_listener(f if state is _NO_STATE else lambda: f(state))
        return _CancellationRegistration(self, listener_id)

    def Dispose(self) -> None:
        # Source disposal is currently a no-op, as in the compiler replacement and JS runtime.
        pass


class _CancellationRegistration:
    def __init__(self, token: CancellationToken, listener_id: int) -> None:
        self._token = token
        self._listener_id = listener_id

    def Dispose(self) -> None:
        self._token.remove_listener(self._listener_id)


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
        loop = asyncio.get_running_loop()
        loop.call_later(due_time, action)

    def run(self, action: Callable[[], None]):
        loop = asyncio.get_running_loop()

        if self.increment_and_check():
            self.call_count = 0
            loop.call_soon(action)
        else:
            action()


def protected_cont[T](f: Async[T]) -> Async[T]:
    def _protected_cont(ctx: IAsyncContext[T]):
        if ctx.cancel_token and ctx.cancel_token.is_cancelled:
            ctx.on_cancel(OperationCanceledError())
            return

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

    def For[T, U](self, sequence: IEnumerable_1[T], body: Callable[[T], Async[None]]) -> Async[None]:
        enumerator = sequence.GetEnumerator()
        has_next = enumerator.System_Collections_IEnumerator_MoveNext()

        def delay() -> Async[None]:
            nonlocal has_next
            cur = enumerator.System_Collections_Generic_IEnumerator_1_get_Current()
            res = body(cur)
            has_next = enumerator.System_Collections_IEnumerator_MoveNext()
            return res

        return self.While(lambda: has_next, self.Delay(delay))

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
