from abc import abstractmethod
from typing import Any, Callable, Generic, Optional, Protocol, TypeVar

from .util import IDisposable
from .option import value

_T = TypeVar("_T")
_T_co = TypeVar("_T_co", covariant=True)
_T_contra = TypeVar("_T_contra", contravariant=True)
_U = TypeVar("_U")


class IObserver(Protocol, Generic[_T_contra]):
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
    @abstractmethod
    def Subscribe(self, __obs: IObserver[_T_co]) -> IDisposable:
        ...


class Observable(IObservable[_T]):
    def __init__(self, subscribe: Callable[[IObserver[_T]], IDisposable]) -> None:
        self.subscribe = subscribe

    def Subscribe(self, obv: IObserver[_T]) -> IDisposable:
        return self.subscribe(obv)


def subscribe(callback: Callable[[_T], None], source: IObservable[_T]) -> IDisposable:
    return source.Subscribe(Observer(callback))


def add(callback: Callable[[_T], None], source: IObservable[_T]) -> IDisposable:
    return source.Subscribe(Observer(callback))


def protect(f: Callable[[], _T], succeed: Callable[[_T], None], fail: Callable[[Exception], None]):
    try:
        return succeed(f())
    except Exception as e:
        fail(e)


def choose(chooser: Callable[[_T], Optional[_U]], source: IObservable[_T]) -> IObservable[_U]:
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
