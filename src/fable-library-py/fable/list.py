# flake8: noqa

from typing import Any, Callable, TypeVar

from expression.collections import FrozenList

A = TypeVar("A")
B = TypeVar("B")


def collect(mapper: Callable[[A], FrozenList[B]], lst: FrozenList[A]) -> FrozenList[B]:
    return lst.collect(mapper)


def empty() -> FrozenList[Any]:
    return FrozenList.empty()


def filter(predicate: Callable[[A], bool], lst: FrozenList[A]) -> FrozenList[A]:
    return lst.filter(predicate)


def forAll(predicate, source):
    return source.forall(predicate)


def length(xs):
    return len(xs)


def map(mapper: Callable[[A], B], lst: FrozenList[A]) -> FrozenList[B]:
    return lst.map(mapper)


ofArray = FrozenList.of_seq
ofSeq = FrozenList.of_seq
singleton = FrozenList.singleton

__all__ = ["collect", "empty", "forAll", "length", "map", "ofArray", "ofSeq", "singleton"]
