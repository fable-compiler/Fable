from __future__ import annotations

from abc import abstractmethod
from collections import deque
from threading import Lock, RLock, Timer
from typing import (
    Any,
    Callable,
    Dict,
    Generic,
    Iterable,
    Optional,
    Protocol,
    TypeVar,
    cast,
)

from .util import IDisposable


_T = TypeVar("_T")
_U = TypeVar("_U")
_D = TypeVar("_D", bound=IDisposable)


class OperationCanceledError(Exception):
    def __init__(self, msg: Optional[str] = None) -> None:
        super().__init__(msg or "The operation was canceled")


class _Listener(Protocol):
    def __call__(self, __state: Optional[Any] = None) -> None:
        ...


class CancellationToken:
    __slots__ = "cancelled", "listeners", "idx", "lock"

    def __init__(self, cancelled: bool = False):
        self.cancelled = cancelled
        self.listeners: Dict[int, Callable[[], None]] = {}
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

    def register(self, f: _Listener, state: Optional[Any] = None) -> None:
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
    def on_success(self, value: Optional[_T] = None) -> None:
        ...

    @abstractmethod
    def on_error(self, error: Exception) -> None:
        ...

    @abstractmethod
    def on_cancel(self, error: OperationCanceledError) -> None:
        ...

    @property
    @abstractmethod
    def trampoline(self) -> "Trampoline":
        ...

    @trampoline.setter
    @abstractmethod
    def trampoline(self, val: Trampoline):
        ...

    @property
    @abstractmethod
    def cancel_token(self) -> CancellationToken:
        ...

    @cancel_token.setter
    @abstractmethod
    def cancel_token(self, val: CancellationToken):
        ...

    @staticmethod
    def create(
        on_success: Optional[Callable[[Optional[_T]], None]],
        on_error: Optional[Callable[[Exception], None]],
        on_cancel: Optional[Callable[[OperationCanceledError], None]],
        trampoline: Optional[Trampoline],
        cancel_token: Optional[CancellationToken],
    ) -> AnonymousAsyncContext[_T]:
        return AnonymousAsyncContext(
            on_success, on_error, on_cancel, trampoline, cancel_token
        )


""" FSharpAsync"""
Async = Callable[[IAsyncContext[_T]], None]


def empty_continuation(x: Any = None) -> None:
    pass


class AnonymousAsyncContext(IAsyncContext[_T]):
    __slots__ = "_on_success", "_on_error", "_on_cancel", "_trampoline", "_cancel_token"

    def __init__(
        self,
        on_success: Optional[Callable[[Optional[_T]], None]] = None,
        on_error: Optional[Callable[[Exception], None]] = None,
        on_cancel: Optional[Callable[[OperationCanceledError], None]] = None,
        trampoline: Optional[Trampoline] = None,
        cancel_token: Optional[CancellationToken] = None,
    ):
        self._on_success: Callable[[Optional[_T]], None] = (
            on_success or empty_continuation
        )
        self._on_error: Callable[[Exception], None] = on_error or empty_continuation
        self._on_cancel: Callable[[OperationCanceledError], None] = (
            on_cancel or empty_continuation
        )

        self._cancel_token = cancel_token
        self._trampoline = trampoline

    def on_success(self, value: Optional[_T] = None) -> None:
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


class Trampoline:
    __slots__ = "queue", "lock", "running", "call_count"

    MaxTrampolineCallCount = 150  # Max recursion depth: 1000

    def __init__(self):
        self.call_count: int = 0
        self.lock = Lock()
        self.queue: deque[Callable[[], None]] = deque()
        self.running: bool = False

    def increment_and_check(self):
        with self.lock:
            self.call_count = self.call_count + 1
            return self.call_count > Trampoline.MaxTrampolineCallCount

    def run(self, action: Callable[[], None]):

        if self.increment_and_check():
            with self.lock:
                self.queue.append(action)

            if not self.running:
                self.running = True
                timer = Timer(0.0, self._run)
                timer.start()
        else:
            action()

    def _run(self) -> None:
        while len(self.queue):
            with self.lock:
                self.call_count = 0
                action = self.queue.popleft()

            action()

        self.running = False


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
    def cont(ctx: IAsyncContext[_T]):
        def on_success(x: Optional[_T] = None) -> None:
            try:
                binder(cast(_T, x))(ctx)
            except Exception as err:
                # print("Exception: ", err)
                ctx.on_error(err)

        ctx_ = IAsyncContext.create(
            on_success, ctx.on_error, ctx.on_cancel, ctx.trampoline, ctx.cancel_token
        )
        return computation(ctx_)

    return protected_cont(cont)


def protected_return(value: Optional[_T] = None) -> Async[_T]:
    f: Callable[[IAsyncContext[_T]], None] = lambda ctx: ctx.on_success(value)
    return protected_cont(f)


class AsyncBuilder:
    __slots__ = ()

    def Bind(
        self, computation: Async[_T], binder: Callable[[_T], Async[_U]]
    ) -> Async[_U]:
        return protected_bind(computation, binder)

    def Combine(self, computation1: Async[Any], computation2: Async[_T]) -> Async[_T]:
        binder: Callable[[Optional[_T]], Async[_T]] = lambda _: computation2
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

        def delay():
            nonlocal cur, done
            res = body(cur)
            try:
                cur = next(it)
            except StopIteration:
                done = True
            return res

        return self.While(lambda: not done, self.Delay(delay))

    def Return(self, value: Optional[_T] = None) -> Async[_T]:
        return protected_return(value)

    def ReturnFrom(self, computation: Async[_T]) -> Async[_T]:
        return computation

    def TryFinally(
        self, computation: Async[_T], compensation: Callable[[], None]
    ) -> Async[_T]:
        def cont(ctx: IAsyncContext[_T]) -> None:
            def on_success(x: Optional[_T] = None) -> None:
                compensation()
                ctx.on_success(x)

            def on_error(x: Exception) -> None:
                compensation()
                ctx.on_error(x)

            def on_cancel(x: OperationCanceledError) -> None:
                compensation()
                ctx.on_cancel(x)

            ctx_ = IAsyncContext.create(
                on_success, on_error, on_cancel, ctx.trampoline, ctx.cancel_token
            )
            computation(ctx_)

        return protected_cont(cont)

    def TryWith(
        self, computation: Async[_T], catch_handler: Callable[[Exception], Async[_T]]
    ) -> Async[_T]:
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
        compensation: Callable[[], None] = lambda: resource.Dispose()
        return self.TryFinally(binder(resource), compensation)

    def While(self, guard: Callable[[], bool], computation: Async[_T]) -> Async[_T]:
        if guard():
            binder: Callable[[Optional[_T]], Async[_T]] = lambda _: self.While(
                guard, computation
            )
            return self.Bind(computation, binder)
        else:
            return self.Return()

    def Zero(self) -> Async[Any]:
        return protected_cont(lambda ctx: ctx.on_success())


singleton = AsyncBuilder()
