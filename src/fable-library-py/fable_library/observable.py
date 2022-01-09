from abc import abstractmethod
from typing import TypeVar, Generic, Callable, Optional, Any, Protocol
from .util import IDisposable

_T = TypeVar("_T")


class IObserver(Protocol, Generic[_T]):
    @abstractmethod
    def OnNext(self, __value: _T) -> None:
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
        on_error: Optional[Callable[[Exception], None]],
        on_completed: Optional[Callable[[], None]],
    ) -> None:
        self.on_next = on_next
        self.on_error = on_error or _noop
        self.on_completed = on_completed or _noop


class IObservable(Protocol, Generic[_T]):
    @abstractmethod
    def Subscribe(self, __obs: IObserver[_T]) -> IDisposable:
        ...


class Observable(IObservable[_T]):
    def __init__(self, subscribe: Callable[[IObserver[_T]], IDisposable]) -> None:
        self.subscribe = subscribe
