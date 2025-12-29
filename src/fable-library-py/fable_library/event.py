import inspect
from abc import abstractmethod
from collections.abc import Callable
from typing import Any, Final, Protocol, cast, overload

from .choice import (
    Choice_tryValueIfChoice1Of2,
    Choice_tryValueIfChoice2Of2,
    FSharpChoice_2,
)
from .observable import IObservable, IObserver, Observer
from .option import Option, some, value
from .protocols import IDisposable
from .util import DisposableBase


_MISSING: Final[object] = object()


type Delegate[T] = Callable[[T], None]
type DotNetDelegate[T] = Callable[[Any, T], None]
type EventDelegate[T] = Delegate[T] | DotNetDelegate[T]


class IDelegateEvent[T](Protocol):
    @abstractmethod
    def AddHandler(self, handler: DotNetDelegate[T]) -> None: ...

    @abstractmethod
    def RemoveHandler(self, handler: DotNetDelegate[T]) -> None: ...


class IEvent_2[Args, Delegate](IObservable[Args], IDelegateEvent[Delegate], Protocol): ...


type IEvent[T] = IEvent_2[T, T]


class Event[T](IEvent_2[T, T]):
    def __init__(self) -> None:
        self.delegates: list[EventDelegate[T]] = []

    def Add(self, f: Delegate[T]) -> None:
        self._addHandler(f)

    @property
    def Publish(self) -> IEvent[T]:
        return self

    @overload
    def Trigger(self, sender_or_value: T, /) -> None: ...

    @overload
    def Trigger(self, sender_or_value: Any, value_or_undefined: T, /) -> None: ...

    def Trigger(self, sender_or_value: Any, value_or_undefined: Any = _MISSING) -> None:
        if value_or_undefined is _MISSING:
            value = sender_or_value
            sender = None
        else:
            sender = sender_or_value
            value = value_or_undefined

        for f in self.delegates:
            if len(inspect.signature(f).parameters) == 1:
                cast(Delegate[T], f)(value)  # We checked the signature
            else:
                cast(DotNetDelegate[T], f)(sender, value)  # We checked the signature

    # IDelegateEvent<T> methods

    def AddHandler(self, handler: DotNetDelegate[T]) -> None:
        self._addHandler(handler)

    def RemoveHandler(self, handler: DotNetDelegate[T]) -> None:
        self._removeHandler(handler)

    # IObservable<T> methods

    @overload
    def Subscribe(self, observer_or_callback: IObserver[T], /) -> IDisposable: ...

    @overload
    def Subscribe(self, observer_or_callback: Delegate[T], /) -> IDisposable: ...

    def Subscribe(self, observer_or_callback: IObserver[T] | Delegate[T]) -> IDisposable:
        callback: Delegate[T]
        if isinstance(observer_or_callback, IObserver):
            observer = cast(IObserver[T], observer_or_callback)  # Must be IObserver[T]
            callback = observer.OnNext
        else:
            callback = observer_or_callback

        self._addHandler(callback)

        def dispose() -> None:
            self._removeHandler(callback)

        return DisposableBase.create(dispose)

    def _addHandler(self, f: EventDelegate[T]) -> None:
        self.delegates.append(f)

    def _removeHandler(self, f: EventDelegate[T]) -> None:
        if f in self.delegates:
            self.delegates.remove(f)


def add[T](callback: Delegate[T], source_event: IEvent[T]) -> None:
    source_event.Subscribe(Observer(callback))


def choose[T, U](chooser: Callable[[T], Option[U]], source_event: IEvent[T]) -> IEvent[U]:
    ev = Event[U]()

    def callback(t: T) -> None:
        u = chooser(t)
        if u is not None:
            ev.Trigger(value(u))

    add(callback, source_event)
    return ev


def filter[T](predicate: Callable[[T], bool], source_event: IEvent[T]) -> IEvent[T]:
    return choose(lambda t: some(t) if predicate(t) else None, source_event)


def map[T, U](mapping: Callable[[T], U], source_event: IEvent[T]) -> IEvent[U]:
    return choose(lambda t: mapping(t), source_event)


def merge[T](event1: IEvent[T], event2: IEvent[T]) -> IEvent[T]:
    ev = Event[T]()

    def fn(t: T) -> None:
        ev.Trigger(t)

    add(fn, event1)
    add(fn, event2)
    return ev


def pairwise[T](source_event: IEvent[T]) -> IEvent[list[T]]:
    ev = Event[list[T]]()
    last: T | None = None

    def fn(next: T) -> None:
        nonlocal last
        if last is not None:
            ev.Trigger([last, next])
        last = next

    add(fn, source_event)
    return ev


def partition[T](predicate: Callable[[T], bool], source_event: IEvent[T]) -> list[IEvent[T]]:
    return [
        filter(predicate, source_event),
        filter(lambda x: not predicate(x), source_event),
    ]


def scan[T, U](
    collector: Callable[[U, T], U],
    state: U,
    source_event: IEvent[T],
) -> IEvent[U]:
    return map(lambda t: collector(state, t), source_event)


def split[T, U, V](splitter: Callable[[T], FSharpChoice_2[U, V]], source_event: IEvent[T]) -> list[IEvent[Any]]:
    return [
        choose(lambda t: Choice_tryValueIfChoice1Of2(splitter(t)), source_event),
        choose(lambda t: Choice_tryValueIfChoice2Of2(splitter(t)), source_event),
    ]


class AnonymousEvent[U](IEvent_2[U, U]):
    def __init__(
        self,
        add_handler: Callable[[DotNetDelegate[U]], None],
        remove_handler: Callable[[DotNetDelegate[U]], None],
    ) -> None:
        self._add_handler = add_handler
        self._remove_handler = remove_handler

    def AddHandler(self, handler: DotNetDelegate[U]) -> None:
        self._add_handler(handler)

    def RemoveHandler(self, handler: DotNetDelegate[U]) -> None:
        self._remove_handler(handler)

    def Subscribe(self, observer: IObserver[U]) -> IDisposable:
        def h(sender: Any, value: U) -> None:
            observer.OnNext(value)

        self._add_handler(h)

        def dispose() -> None:
            self._remove_handler(h)

        return DisposableBase.create(dispose)


def create_event[T](
    add_handler: Callable[[DotNetDelegate[T]], None],
    remove_handler: Callable[[DotNetDelegate[T]], None],
) -> IEvent[T]:
    return AnonymousEvent[T](add_handler, remove_handler)
