"""Native-generator replacements for the hottest `Seq` combinators.

`Seq.fs` otherwise builds these on `generate`/`Enumerator.generateWhileSome`: an `Option`
allocated per element plus a `MoveNext`/`Current` call per step. These use plain Python
generators instead, bound from `Seq.fs` via `[<Import>]` + `nativeOnly`.

Two rules keep F# `seq` semantics intact:
  1. Each `NativeSeq` holds a THUNK, not a live generator, so every enumeration gets a fresh
     generator - restartable, and source side effects re-run each time.
  2. Disposal calls the generator's `.close()` (runs `finally` via `GeneratorExit`), via
     `util.Enumerator.Dispose`.
"""

from __future__ import annotations

from collections.abc import Callable, Iterable, Iterator, Mapping
from typing import Any

from .array_ import Array
from .array_ import try_head as array_try_head
from .array_ import try_item as array_try_item
from .bases import EnumerableBase
from .core import int32
from .list import FSharpList
from .list import try_head as list_try_head
from .list import try_item as list_try_item
from .option import Option, some, value
from .protocols import IEnumerable_1, IEnumerator
from .types import UNIT, Unit
from .util import Enumerator, get_enumerator, to_iterator

_SENTINEL: Any = object()


class NativeSeq[T](EnumerableBase[T]):
    """An IEnumerable<T> backed by a thunk that produces a fresh iterator per enumeration.
    Like `util.Enumerable`, but takes a thunk instead of a plain iterable, so restartability
    doesn't depend on the iterable already being re-iterable."""

    __slots__ = ("_thunk",)

    def __init__(self, thunk: Callable[[], Iterable[T]]) -> None:
        self._thunk = thunk

    def GetEnumerator(self, __unit: Unit = UNIT) -> IEnumerator[T]:
        return Enumerator(iter(self._thunk()))

    def __iter__(self) -> Iterator[T]:
        return iter(self._thunk())


def _source_iter[T](xs: IEnumerable_1[T]) -> Iterator[T]:
    """A Python iterator over `xs` matching its IEnumerable<T> contract.

    Most sources' `__iter__` already matches `GetEnumerator`/`Current`, so iterating directly
    is correct and avoids MoveNext/Current overhead. Exception: `Mapping` (`FSharpMap`) - its
    `__iter__` yields keys only, diverging from the `KeyValuePair` tuples `Current` produces -
    those need the explicit bridge.
    """
    if isinstance(xs, Mapping):
        return to_iterator(get_enumerator(xs))
    if isinstance(xs, Iterable):
        return iter(xs)
    return to_iterator(get_enumerator(xs))


def map[T, U](mapping: Callable[[T], U], xs: IEnumerable_1[T]) -> NativeSeq[U]:
    return NativeSeq(lambda: (mapping(x) for x in _source_iter(xs)))


def filter[T](predicate: Callable[[T], bool], xs: IEnumerable_1[T]) -> NativeSeq[T]:
    return NativeSeq(lambda: (x for x in _source_iter(xs) if predicate(x)))


def collect[T, U](mapping: Callable[[T], IEnumerable_1[U]], xs: IEnumerable_1[T]) -> NativeSeq[U]:
    return NativeSeq(lambda: (y for x in _source_iter(xs) for y in _source_iter(mapping(x))))


def append[T](xs: IEnumerable_1[T], ys: IEnumerable_1[T]) -> NativeSeq[T]:
    def gen() -> Iterator[T]:
        yield from _source_iter(xs)
        yield from _source_iter(ys)

    return NativeSeq(gen)


def truncate[T](count: int, xs: IEnumerable_1[T]) -> NativeSeq[T]:
    def gen() -> Iterator[T]:
        if count <= 0:
            return

        i = 0
        for x in _source_iter(xs):
            yield x  # yield BEFORE checking, so we never pull the (count+1)-th element
            i += 1
            if i >= count:
                return

    return NativeSeq(gen)


def unfold[T, S](generator: Callable[[S], tuple[T, S] | None], state: S) -> NativeSeq[T]:
    def gen() -> Iterator[T]:
        s = state
        while True:
            r = generator(s)
            if r is None:  # F# option erases to None here: the payload is a tuple, never None
                return
            x, s = r
            yield x

    return NativeSeq(gen)


def choose[T, U](chooser: Callable[[T], Option[U]], xs: IEnumerable_1[T]) -> NativeSeq[U]:
    def gen() -> Iterator[U]:
        for x in _source_iter(xs):
            r = chooser(x)
            if r is not None:
                yield value(r)

    return NativeSeq(gen)


def map_indexed[T, U](mapping: Callable[[int32, T], U], xs: IEnumerable_1[T]) -> NativeSeq[U]:
    return NativeSeq(lambda: (mapping(int32(i), x) for i, x in enumerate(_source_iter(xs))))


def map2[T1, T2, U](mapping: Callable[[T1, T2], U], xs: IEnumerable_1[T1], ys: IEnumerable_1[T2]) -> NativeSeq[U]:
    return NativeSeq(lambda: (mapping(x, y) for x, y in zip(_source_iter(xs), _source_iter(ys))))


def map_indexed2[T1, T2, U](
    mapping: Callable[[int32, T1, T2], U], xs: IEnumerable_1[T1], ys: IEnumerable_1[T2]
) -> NativeSeq[U]:
    return NativeSeq(
        lambda: (mapping(int32(i), x, y) for i, (x, y) in enumerate(zip(_source_iter(xs), _source_iter(ys))))
    )


def map3[T1, T2, T3, U](
    mapping: Callable[[T1, T2, T3], U], xs: IEnumerable_1[T1], ys: IEnumerable_1[T2], zs: IEnumerable_1[T3]
) -> NativeSeq[U]:
    return NativeSeq(
        lambda: (mapping(x, y, z) for x, y, z in zip(_source_iter(xs), _source_iter(ys), _source_iter(zs)))
    )


def take_while[T](predicate: Callable[[T], bool], xs: IEnumerable_1[T]) -> NativeSeq[T]:
    def gen() -> Iterator[T]:
        for x in _source_iter(xs):
            if not predicate(x):
                return
            yield x

    return NativeSeq(gen)


def take[T](count: int, xs: IEnumerable_1[T]) -> NativeSeq[T]:
    def gen() -> Iterator[T]:
        it = _source_iter(xs)
        for _ in range(count):
            v = next(it, _SENTINEL)
            if v is _SENTINEL:
                raise Exception("The input sequence has an insufficient number of elements. (Parameter 'source')")
            yield v

    return NativeSeq(gen)


# Eager terminal ops: pull via _source_iter instead of the Option-per-element `generate`
# protocol the generic (non-Array/FSharpList) fallback used before.
def try_head[T](xs: IEnumerable_1[T]) -> Option[T]:
    if isinstance(xs, Array):
        return array_try_head(xs)
    if isinstance(xs, FSharpList):
        return list_try_head(xs)

    v = next(_source_iter(xs), _SENTINEL)
    return None if v is _SENTINEL else some(v)


def head[T](xs: IEnumerable_1[T]) -> T:
    result = try_head(xs)
    if result is None:
        raise Exception("The input sequence was empty. (Parameter 'source')")
    return value(result)


def try_item[T](index: int32, xs: IEnumerable_1[T]) -> Option[T]:
    if isinstance(xs, Array):
        return array_try_item(index, xs)
    if isinstance(xs, FSharpList):
        return list_try_item(index, xs)
    if index < 0:
        return None

    it = _source_iter(xs)
    for _ in range(index):
        if next(it, _SENTINEL) is _SENTINEL:
            return None

    v = next(it, _SENTINEL)
    return None if v is _SENTINEL else some(v)


def item[T](index: int32, xs: IEnumerable_1[T]) -> T:
    result = try_item(index, xs)
    if result is None:
        raise Exception("The input sequence has an insufficient number of elements. (Parameter 'index')")
    return value(result)
