from abc import abstractmethod
from typing import Any, Callable, Generic, Optional, Protocol, Tuple, TypeVar

from .choice import (
    Choice_tryValueIfChoice1Of2,
    Choice_tryValueIfChoice2Of2,
    FSharpChoice_2,
)
from .option import value
from .util import IDisposable


_T = TypeVar("_T")
_T_co = TypeVar("_T_co", covariant=True)
_T_contra = TypeVar("_T_contra", contravariant=True)
_U = TypeVar("_U")


class IObserver(Protocol, Generic[_T_contra]):
    __slots__ = ()

    @abstractmethod
    def OnNext(self, __value: _T_contra) -> None:
        ...

    @abstractmethod
    def OnError(self, __error: Exception) -> None:
        ...

    @abstractmethod
    def OnCompleted(self) -> None:
        ...


def _noop(__arg: Any = None) -> None:
    pass


class Observer(IObserver[_T]):
    __slots__ = "_on_error", "_on_next", "_on_completed"

    def __init__(
        self,
        on_next: Callable[[_T], None],
        on_error: Optional[Callable[[Exception], None]] = None,
        on_completed: Optional[Callable[[], None]] = None,
    ) -> None:
        self._on_next = on_next
        self._on_error = on_error or _noop
        self._on_completed = on_completed or _noop

    def OnError(self, error: Exception) -> None:
        return self._on_error(error)

    def OnNext(self, value: _T) -> None:
        return self._on_next(value)

    def OnCompleted(self) -> None:
        return self._on_completed()


class IObservable(Protocol, Generic[_T_co]):
    __slots__ = ()

    @abstractmethod
    def Subscribe(self, __obs: IObserver[_T_co]) -> IDisposable:
        ...


class Observable(IObservable[_T]):
    __slots__ = "subscribe"

    def __init__(self, subscribe: Callable[[IObserver[_T]], IDisposable]) -> None:
        self.subscribe = subscribe

    def Subscribe(self, obv: IObserver[_T]) -> IDisposable:
        return self.subscribe(obv)


def subscribe(callback: Callable[[_T], None], source: IObservable[_T]) -> IDisposable:
    return source.Subscribe(Observer(callback))


def add(callback: Callable[[_T], None], source: IObservable[_T]) -> IDisposable:
    return source.Subscribe(Observer(callback))


def protect(
    f: Callable[[], _T],
    succeed: Callable[[_T], None],
    fail: Callable[[Exception], None],
):
    try:
        return succeed(f())
    except Exception as e:
        fail(e)


def choose(
    chooser: Callable[[_T], Optional[_U]], source: IObservable[_T]
) -> IObservable[_U]:
    def subscribe(observer: IObserver[_U]):
        def on_next(t: _T) -> None:
            def success(u: Optional[_U]) -> None:
                if u is not None:
                    observer.OnNext(value(u))

            return protect(lambda: chooser(t), success, observer.OnError)

        obv = Observer(on_next, observer.OnError, observer.OnCompleted)
        return source.Subscribe(obv)

    return Observable(subscribe)


def filter(predicate: Callable[[_T], bool], source: IObservable[_T]) -> IObservable[_T]:
    return choose(lambda x: x if predicate(x) else None, source)


def map(mapping: Callable[[_T], _U], source: IObservable[_T]) -> IObservable[_U]:
    def subscribe(observer: IObserver[_U]) -> IDisposable:
        def on_next(value: _T) -> None:
            return protect(lambda: mapping(value), observer.OnNext, observer.OnError)

        obv = Observer(on_next, observer.OnError, observer.OnCompleted)
        return source.Subscribe(obv)

    return Observable(subscribe)


def merge(source1: IObservable[_T], source2: IObservable[_T]) -> IObservable[_T]:
    def subscribe(observer: IObserver[_T]) -> IDisposable:
        stopped = False
        completed1 = False
        completed2 = False

        def on_next(value: _T) -> None:
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

        return IDisposable.create(dispose)

    return Observable(subscribe)


def pairwise(source: IObservable[_T]) -> IObservable[Tuple[_T, _T]]:
    def subscribe(observer: IObserver[Tuple[_T, _T]]) -> IDisposable:
        last: Optional[_T] = None

        def on_next(value: _T) -> None:
            nonlocal last
            if last is not None:
                observer.OnNext((last, value))

            last = value

        obv = Observer(on_next, observer.OnError, observer.OnCompleted)
        return source.Subscribe(obv)

    return Observable(subscribe)


def partition(
    predicate: Callable[[_T], bool], source: IObservable[_T]
) -> Tuple[IObservable[_T], IObservable[_T]]:
    return (filter(predicate, source), filter(lambda x: not predicate(x), source))


def scan(
    collector: Callable[[_U, _T], _U], state: _U, source: IObservable[_T]
) -> IObservable[_U]:
    def subscribe(observer: IObserver[_U]) -> IDisposable:
        def on_next(t: _T) -> None:
            def success(u: _U) -> None:
                nonlocal state
                state = u
                observer.OnNext(u)

            protect(lambda: collector(state, t), success, observer.OnError)

        obv = Observer(on_next, observer.OnError, observer.OnCompleted)
        return source.Subscribe(obv)

    return Observable(subscribe)


def split(
    splitter: Callable[[_T], FSharpChoice_2], source: IObservable[_T]
) -> Tuple[IObservable[_T], IObservable[_T]]:
    return (
        choose(lambda v: Choice_tryValueIfChoice1Of2(splitter(v)), source),
        choose(lambda v: Choice_tryValueIfChoice2Of2(splitter(v)), source),
    )
