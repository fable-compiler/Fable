from typing import Callable, Iterable, TypeVar
from expression.collections import Seq, seq, frozenlist

A = TypeVar("A")
B = TypeVar("B")


def map(mapper: Callable[[A], B], xs: Seq[A]) -> Seq[B]:
    return Seq(xs).map(mapper)


def filter(predicate: Callable[[A], bool], xs: Seq[A]) -> Seq[A]:
    return Seq(xs).filter(predicate)


def length(xs):
    return Seq(xs).length()


def empty():
    return seq.empty


def collect(mapper: Callable[[A], Seq[B]], source: Seq[A]) -> Seq[B]:
    return Seq(source).collect(mapper)


def skip(count: int, xs: Seq[A]) -> Seq[A]:
    return Seq(xs).skip(count)


def sum(source: Iterable[A]) -> A:
    return Seq(source).sum()


def sumBy(projection: Callable[[A], B], source: Iterable[A], _) -> B:
    return Seq(source).sum_by(projection)


delay = seq.delay
head = seq.head
rangeNumber = seq.range
singleton = seq.singleton
append = seq.concat
ofList = seq.of_list
toList = frozenlist.of_seq
concat = seq.concat
tail = seq.tail

__all__ = [
    "delay",
    "empty",
    "head",
    "map",
    "length",
    "rangeNumber",
    "singleton",
    "skip",
    "tail",
]
