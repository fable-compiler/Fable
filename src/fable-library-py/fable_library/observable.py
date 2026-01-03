from abc import abstractmethod
from collections.abc import Callable
from typing import Protocol, runtime_checkable

from .choice import (
    Choice_tryValueIfChoice1Of2,
    Choice_tryValueIfChoice2Of2,
    FSharpChoice_2,
)
from .option import value
from .protocols import IDisposable
from .util import UNIT, Disposable


@runtime_checkable
class IObserver[T_contra](Protocol):
    __slots__ = ()

    @abstractmethod
    def OnNext(self, __value: T_contra) -> None: ...

    @abstractmethod
    def OnError(self, __error: Exception) -> None: ...

    @abstractmethod
    def OnCompleted(self) -> None: ...


def _noop(__arg=UNIT) -> None:
    pass


class Observer[T](IObserver[T]):
    __slots__ = "_on_completed", "_on_error", "_on_next"

    def __init__(
        self,
        on_next: Callable[[T], None],
        on_error: Callable[[Exception], None] | None = None,
        on_completed: Callable[[], None] | None = None,
    ) -> None:
        self._on_next = on_next
        self._on_error = on_error or _noop
        self._on_completed = on_completed or _noop

    def OnError(self, error: Exception) -> None:
        return self._on_error(error)

    def OnNext(self, value: T) -> None:
        return self._on_next(value)

    def OnCompleted(self) -> None:
        return self._on_completed()


class IObservable[T_co](Protocol):
    __slots__ = ()

    @abstractmethod
    def Subscribe(self, __obs: IObserver[T_co]) -> IDisposable: ...


class Observable[T](IObservable[T]):
    __slots__ = "subscribe"

    def __init__(self, subscribe: Callable[[IObserver[T]], IDisposable]) -> None:
        self.subscribe = subscribe

    def Subscribe(self, obv: IObserver[T]) -> IDisposable:
        return self.subscribe(obv)


def subscribe[T](callback: Callable[[T], None], source: IObservable[T]) -> IDisposable:
    return source.Subscribe(Observer(callback))


def add[T](callback: Callable[[T], None], source: IObservable[T]) -> IDisposable:
    return source.Subscribe(Observer(callback))


def protect[T](
    f: Callable[[], T],
    succeed: Callable[[T], None],
    fail: Callable[[Exception], None],
):
    try:
        return succeed(f())
    except Exception as e:
        fail(e)


def choose[T, U](chooser: Callable[[T], U | None], source: IObservable[T]) -> IObservable[U]:
    def subscribe(observer: IObserver[U]):
        def on_next(t: T) -> None:
            def success(u: U | None) -> None:
                if u is not None:
                    observer.OnNext(value(u))

            return protect(lambda: chooser(t), success, observer.OnError)

        obv = Observer(on_next, observer.OnError, observer.OnCompleted)
        return source.Subscribe(obv)

    return Observable(subscribe)


def filter[T](predicate: Callable[[T], bool], source: IObservable[T]) -> IObservable[T]:
    return choose(lambda x: x if predicate(x) else None, source)


def map[T, U](mapping: Callable[[T], U], source: IObservable[T]) -> IObservable[U]:
    def subscribe(observer: IObserver[U]) -> IDisposable:
        def on_next(value: T) -> None:
            return protect(lambda: mapping(value), observer.OnNext, observer.OnError)

        obv = Observer(on_next, observer.OnError, observer.OnCompleted)
        return source.Subscribe(obv)

    return Observable(subscribe)


def merge[T](source1: IObservable[T], source2: IObservable[T]) -> IObservable[T]:
    def subscribe(observer: IObserver[T]) -> IDisposable:
        stopped = False
        completed1 = False
        completed2 = False

        def on_next(value: T) -> None:
            if stopped:
                return
            observer.OnNext(value)

        def on_error(error: Exception) -> None:
            nonlocal stopped
            if stopped:
                return
            stopped = True
            observer.OnError(error)

        def on_completed1() -> None:
            nonlocal completed1, stopped
            if stopped:
                return

            completed1 = True
            if completed2:
                stopped = True
                observer.OnCompleted()

        obv1 = Observer(on_next, on_error, on_completed1)
        h1 = source1.Subscribe(obv1)

        def on_completed2() -> None:
            nonlocal completed2, stopped
            if stopped:
                return

            completed2 = True
            if completed1:
                stopped = True
                observer.OnCompleted()

        obv2 = Observer(on_next, on_error, on_completed2)
        h2 = source2.Subscribe(obv2)

        def dispose() -> None:
            h1.Dispose()
            h2.Dispose()

        return Disposable.create(dispose)

    return Observable(subscribe)


def pairwise[T](source: IObservable[T]) -> IObservable[tuple[T, T]]:
    def subscribe(observer: IObserver[tuple[T, T]]) -> IDisposable:
        last: T | None = None

        def on_next(value: T) -> None:
            nonlocal last
            if last is not None:
                observer.OnNext((last, value))

            last = value

        obv = Observer(on_next, observer.OnError, observer.OnCompleted)
        return source.Subscribe(obv)

    return Observable(subscribe)


def partition[T](predicate: Callable[[T], bool], source: IObservable[T]) -> tuple[IObservable[T], IObservable[T]]:
    return (filter(predicate, source), filter(lambda x: not predicate(x), source))


def scan[T, U](collector: Callable[[U, T], U], state: U, source: IObservable[T]) -> IObservable[U]:
    def subscribe(observer: IObserver[U]) -> IDisposable:
        def on_next(t: T) -> None:
            def success(u: U) -> None:
                nonlocal state
                state = u
                observer.OnNext(u)

            protect(lambda: collector(state, t), success, observer.OnError)

        obv = Observer(on_next, observer.OnError, observer.OnCompleted)
        return source.Subscribe(obv)

    return Observable(subscribe)


def split[T, U, V](
    splitter: Callable[[T], FSharpChoice_2[U, V]], source: IObservable[T]
) -> tuple[IObservable[U], IObservable[V]]:
    return (
        choose(lambda v: Choice_tryValueIfChoice1Of2(splitter(v)), source),
        choose(lambda v: Choice_tryValueIfChoice2Of2(splitter(v)), source),
    )
