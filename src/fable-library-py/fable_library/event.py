import inspect

from abc import abstractmethod
from typing import (
    Any,
    Callable,
    Generic,
    List,
    Optional,
    Protocol,
    TypeVar,
    Union,
    overload,
)

from .choice import (
    Choice_tryValueIfChoice1Of2,
    Choice_tryValueIfChoice2Of2,
    FSharpChoice_2,
)
from .observable import IObservable, IObserver, Observer
from .option import some, value
from .util import IDisposable


_T = TypeVar("_T")
_U = TypeVar("_U")
_V = TypeVar("_V")
_Args = TypeVar("_Args", covariant=True)
_Delegate = TypeVar("_Delegate", covariant=True)
_T_co = TypeVar("_T_co", covariant=True)

Delegate = Callable[[_T], None]
DotNetDelegate = Callable[[Any, _T], None]
EventDelegate = Union[Delegate[_T], DotNetDelegate[_T]]


class IDelegateEvent(Generic[_T_co], Protocol):
    @abstractmethod
    def AddHandler(self, d: DotNetDelegate[_T]) -> None:
        ...

    @abstractmethod
    def RemoveHandler(self, d: DotNetDelegate[_T]) -> None:
        ...


class IEvent_2(IObservable[_Args], IDelegateEvent[_Delegate], Protocol):
    ...


IEvent = IEvent_2[_T, _T]


class Event(IEvent[_T]):
    def __init__(self) -> None:
        self.delegates: List[EventDelegate[_T]] = []

    def Add(self, f: Delegate[_T]) -> None:
        self._addHandler(f)

    @property
    def Publish(self) -> IEvent[_T]:
        return self

    @overload
    def Trigger(self, value: _T) -> None:
        ...

    @overload
    def Trigger(self, sender: Any, value: _T) -> None:
        ...

    def Trigger(
        self, senderOrValue: Any, valueOrUndefined: Optional[_T] = None
    ) -> None:
        if valueOrUndefined is None:
            value = senderOrValue
            sender = None
        else:
            sender = senderOrValue
            value = valueOrUndefined

        for f in self.delegates:
            if len(inspect.signature(f).parameters) == 1:
                f(value)
            else:
                f(sender, value)

    # IDelegateEvent<T> methods

    def AddHandler(self, handler: DotNetDelegate[_T]) -> None:
        self._addHandler(handler)

    def RemoveHandler(self, handler: DotNetDelegate[_T]) -> None:
        self._removeHandler(handler)

    # IObservable<T> methods

    def Subscribe(self, __obs: Union[IObserver[_T], Delegate[_T]]) -> IDisposable:
        if callable(__obs):
            callback = __obs
        else:
            callback = __obs.OnNext

        self._addHandler(callback)

        def dispose() -> None:
            self._removeHandler(callback)

        return IDisposable.create(dispose)

    def _addHandler(self, f: Delegate[_T]) -> None:
        self.delegates.append(f)

    def _removeHandler(self, f: EventDelegate[_T]) -> None:
        if f in self.delegates:
            self.delegates.remove(f)


def add(callback: Delegate[_T], source_event: IEvent[_T]) -> None:
    if isinstance(source_event, Event):
        source_event.Add(callback)
    else:
        source_event.Subscribe(Observer(callback))


def choose(
    chooser: Callable[[_T], Optional[_U]], source_event: IEvent[_T]
) -> IEvent[_U]:
    ev = Event[_U]()

    def callback(t):
        u = chooser(t)
        if u is not None:
            ev.Trigger(value(u))

    add(callback, source_event)
    return ev


def filter(predicate: Callable[[_T], bool], source_event: IEvent[_T]) -> IEvent[_T]:
    return choose(lambda t: some(t) if predicate(t) else None, source_event)


def map(mapping: Callable[[_T], _U], source_event: IEvent[_T]) -> IEvent[_U]:
    return choose(lambda t: mapping(t), source_event)


def merge(event1: IEvent[_T], event2: IEvent[_T]) -> IEvent[_T]:
    ev = Event[_T]()

    def fn(t):
        ev.Trigger(t)

    add(fn, event1)
    add(fn, event2)
    return ev


def pairwise(source_event: IEvent[_T]) -> IEvent[List[_T]]:
    ev = Event[List[_T]]()
    last = None

    def fn(next):
        nonlocal last
        if last is not None:
            ev.Trigger([last, next])
        last = next

    add(fn, source_event)
    return ev


def partition(
    predicate: Callable[[_T], bool], source_event: IEvent[_T]
) -> List[IEvent[_T]]:
    return [
        filter(predicate, source_event),
        filter(lambda x: not predicate(x), source_event),
    ]


def scan(
    collector: Callable[[_U, _T], _U],
    state: _U,
    source_event: IEvent[_T],
) -> IEvent[_U]:
    return map(lambda t: collector(state, t), source_event)


def split(
    splitter: Callable[[_T], FSharpChoice_2[_U, _V]], source_event: IEvent[_T]
) -> List[IEvent[Union[_U, _V]]]:
    return [
        choose(lambda t: Choice_tryValueIfChoice1Of2(splitter(t)), source_event),
        choose(lambda t: Choice_tryValueIfChoice2Of2(splitter(t)), source_event),
    ]


def create_event(
    add_handler: Callable[[DotNetDelegate[_T]], None],
    remove_handler: Callable[[DotNetDelegate[_T]], None],
) -> IEvent[_T]:
    class AnonymousEvent(IEvent[_U]):
        def AddHandler(self, handler: DotNetDelegate[_T]) -> None:
            add_handler(handler)

        def RemoveHandler(self, handler: DotNetDelegate[_T]) -> None:
            remove_handler(handler)

        def Subscribe(self, observer: IObserver[_T]) -> IDisposable:
            def h(sender, value):
                observer.OnNext(value)

            add_handler(h)

            def dispose() -> None:
                remove_handler(h)

            return IDisposable.create(dispose)

    return AnonymousEvent[_T]()
