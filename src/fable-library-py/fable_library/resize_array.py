from typing import Any, Callable, List, TypeVar


_T = TypeVar("_T")


def exists(predicate: Callable[[_T], bool], xs: List[_T]) -> bool:
    """Test if a predicate is true for at least one element in a list."""

    for x in xs:
        if predicate(x):
            return True
    return False


def find_index(predicate: Callable[[_T], bool], xs: List[_T]) -> int:
    """Find the index of the first element in a list that satisfies a predicate."""

    for i, x in enumerate(xs):
        if predicate(x):
            return i
    return -1


def remove_range(start: int, count: int, xs: List[Any]) -> None:
    """Remove a range of elements from a list in-place."""

    del xs[start : start + count]
