from __future__ import annotations

from abc import abstractmethod
from collections import deque
from threading import Lock, RLock, Timer
from typing import Any, Callable, Dict, Generic, Iterable, Optional, TypeVar

from .util import IDisposable

T = TypeVar("T")
U = TypeVar("U")
D = TypeVar("D", bound=IDisposable)


class OperationCanceledError(Exception):
    def __init__(self, msg: Optional[str] = None) -> None:
        super().__init__(msg or "The operation was canceled")


class CancellationToken:
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

    def add_listener(self, f: Callable[[], None]):
        with self.lock:
            id = self.idx
            self.idx = self.idx + 1
            self.listeners[id] = f

        return id

    def remove_listener(self, id: int):
        with self.lock:
            del self.listeners[id]

    def register(self, f: Callable[[Any], None], state: Any = None):
        if state:
            id = self.add_listener(lambda: f(state))
        else:
            id = self.add_listener(f)

        def dispose():
            self.remove_listener(id)

        IDisposable.create(dispose)


class IAsyncContext(Generic[T]):
    @abstractmethod
    def on_success(self, value: Optional[T] = None):
        ...

    @abstractmethod
    def on_error(self, error: Exception):
        ...

    @abstractmethod
    def on_cancel(self, error: OperationCanceledError):
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
        on_success: Optional[Callable[[Optional[T]], None]],
        on_error: Optional[Callable[[Exception], None]],
        on_cancel: Optional[Callable[[OperationCanceledError], None]],
        trampoline: Optional[Trampoline],
        cancel_token: Optional[CancellationToken],
    ) -> AnonymousAsyncContext[T]:
        return AnonymousAsyncContext(on_success, on_error, on_cancel, trampoline, cancel_token)


IAsync = Callable[[IAsyncContext[T]], None]


def empty_continuation(x: Any = None) -> None:
    pass


class AnonymousAsyncContext(IAsyncContext[T]):
    def __init__(
        self,
        on_success: Optional[Callable[[Optional[T]], None]] = None,
        on_error: Optional[Callable[[Exception], None]] = None,
        on_cancel: Optional[Callable[[OperationCanceledError], None]] = None,
        trampoline: Optional[Trampoline] = None,
        cancel_token: Optional[CancellationToken] = None,
    ):
        self._on_success: Callable[[Optional[T]], None] = on_success or empty_continuation
        self._on_error: Callable[[Exception], None] = on_error or empty_continuation
        self._on_cancel: Callable[[OperationCanceledError], None] = on_cancel or empty_continuation

        self._cancel_token = cancel_token
        self._trampoline = trampoline

    def on_success(self, value: Optional[T] = None) -> None:
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
    MaxTrampolineCallCount = 150  # Max recursion depth: 1000

    def __init__(self):
        self.call_count = 0
        self.lock = Lock()
        self.queue: deque[Callable[[], None]] = deque()
        self.running = False

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


def protected_cont(f: IAsync[T]) -> IAsync[T]:
    def _protected_cont(ctx: IAsyncContext[T]):
        if ctx.cancel_token and ctx.cancel_token.is_cancelled:
            ctx.on_cancel(OperationCanceledError())

        def fn():
            try:
                return f(ctx)
            except Exception as err:
                print("Exception: ", err)
                ctx.on_error(err)

        ctx.trampoline.run(fn)

    return _protected_cont


def protected_bind(computation: Callable[[IAsyncContext[T]], None], binder: Callable[[T], IAsync[U]]) -> IAsync[U]:
    def cont(ctx: IAsyncContext[T]):
        def on_success(x: Optional[T]) -> None:
            try:
                binder(x)(ctx)
            except Exception as err:
                print("Exception: ", err)
                ctx.on_error(err)

        ctx_ = IAsyncContext.create(on_success, ctx.on_error, ctx.on_cancel, ctx.trampoline, ctx.cancel_token)
        return computation(ctx_)

    return protected_cont(cont)


def protected_return(value: Optional[T] = None) -> IAsync[T]:
    return protected_cont(lambda ctx: ctx.on_success(value))


class AsyncBuilder:
    def Bind(self, computation: IAsync[T], binder: Callable[[T], IAsync[U]]) -> IAsync[U]:
        return protected_bind(computation, binder)

    def Combine(self, computation1: IAsync[Any], computation2: IAsync[T]) -> IAsync[T]:
        return self.Bind(computation1, lambda _=None: computation2)

    def Delay(self, generator: Callable[[], IAsync[T]]) -> IAsync[T]:
        return protected_cont(lambda ctx: generator()(ctx))

    def For(self, sequence: Iterable[T], body: Callable[[T], IAsync[U]]) -> IAsync[U]:
        done = False
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

    def Return(self, value: T = None) -> IAsync[T]:
        return protected_return(value)

    def ReturnFrom(self, computation: IAsync[T]) -> IAsync[T]:
        return computation

    def TryFinally(self, computation: IAsync[T], compensation: Callable[[], None]) -> IAsync[T]:
        def cont(ctx: IAsyncContext[T]) -> None:
            def on_success(x: Optional[T] = None) -> None:
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

    def TryWith(self, computation: IAsync[T], catch_handler: Callable[[Exception], IAsync[T]]) -> IAsync[T]:
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

    def Using(self, resource: D, binder: Callable[[D], IAsync[U]]) -> IAsync[U]:
        return self.TryFinally(binder(resource), lambda _=None: resource.Dispose())

    def While(self, guard: Callable[[], bool], computation: IAsync[Any]) -> IAsync[Any]:
        if guard():
            return self.Bind(computation, lambda _=None: self.While(guard, computation))
        else:
            return self.Return()

    def Zero(self) -> IAsync[Any]:
        return protected_cont(lambda ctx: ctx.on_success())


singleton = AsyncBuilder()
