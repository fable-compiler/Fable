from typing import Any, Callable, TypeVar, List

_T = TypeVar("_T")


def exists(predicate: Callable[[_T], bool], xs: List[_T]) -> bool:
    for x in xs:
        if predicate(x):
            return True
    return False


def find_index(predicate: Callable[[_T], bool], xs: List[_T]) -> int:
    for i, x in enumerate(xs):
        if predicate(x):
            return i
    return -1


def remove_range(start: int, count: int, xs: List[Any]) -> None:
    del xs[start : start + count]
