from __future__ import annotations

from collections.abc import Callable
from typing import Any, TypeVar

from .option import Option, some


_T = TypeVar("_T")


def exists(predicate: Callable[[_T], bool], xs: list[_T]) -> bool:
    """Test if a predicate is true for at least one element in a list."""

    for x in xs:
        if predicate(x):
            return True
    return False


def find_index(predicate: Callable[[_T], bool], xs: list[_T]) -> int:
    """Find the index of the first element in a list that satisfies a predicate."""

    for i, x in enumerate(xs):
        if predicate(x):
            return i
    return -1


def remove(item: _T, xs: list[_T]) -> bool:
    """Remove an item from a list in-place. Returns True if the item was removed, False if it was not found."""
    try:
        xs.remove(item)
        return True
    except ValueError:
        return False


def remove_range(start: int, count: int, xs: list[Any]) -> None:
    """Remove a range of elements from a list in-place."""

    del xs[start : start + count]


def remove_all_in_place(predicate: Callable[[_T], bool], xs: list[_T]) -> int:
    """Remove all elements matching predicate from the list in-place. Returns the number of removed elements."""
    removed = 0
    i = 0
    while i < len(xs):
        if predicate(xs[i]):
            del xs[i]
            removed += 1
        else:
            i += 1
    return removed


def find_last_index(predicate: Callable[[_T], bool], xs: list[_T]) -> int:
    """Find the index of the last element in a list that satisfies a predicate."""
    for i in range(len(xs) - 1, -1, -1):
        if predicate(xs[i]):
            return i
    return -1


def try_find(predicate: Callable[[_T], bool], xs: list[_T]) -> Option[_T]:
    if not xs:
        return None

    for x in xs:
        if predicate(x):
            return some(x)

    return None


def find_last(predicate: Callable[[_T], bool], xs: list[_T]) -> _T | None:
    """Return the last element in the list that satisfies the predicate, or raise ValueError if not found."""
    for x in reversed(xs):
        if predicate(x):
            return x
    return None


def filter(predicate: Callable[[_T], bool], xs: list[_T]) -> list[_T]:
    """Return a new list of elements that satisfy the predicate."""
    return [x for x in xs if predicate(x)]


def index_of(value: _T, start: int, count: int | None, xs: list[_T]) -> int:
    """Return the index of value in xs, or -1 if not found. Specify start and count."""
    end = min(len(xs), start + count if count is not None else len(xs))
    for i in range(start, end):
        if xs[i] == value:
            return i
    return -1


def insert_range_in_place(index: int, items: list[_T], xs: list[_T]) -> None:
    """Insert a range of items into xs at the given index."""
    xs[index:index] = items


def add_in_place(x: _T, xs: list[_T]) -> None:
    """Add an item to xs in-place."""
    xs.append(x)


def add_range_in_place(range: list[_T], array: list[_T]) -> None:
    """Add a range of items to xs at the given index."""
    for x in range:
        array.append(x)


def add_range(index: int, items: list[_T], xs: list[_T]) -> list[_T]:
    """Add a range of items to xs at the given index."""
    return xs[:index] + items + xs[index:]


def get_sub_array(xs: list[_T], start: int, count: int) -> list[_T]:
    """Get a sub-array of xs from the given start index and count."""
    return xs[start : start + count]


def iterate(action: Callable[[_T], None], xs: list[_T]) -> None:
    """Iterate over a list and apply an action to each element."""
    for x in xs:
        action(x)


def contains(value: _T, xs: list[_T], cons: Any | None = None) -> bool:
    return value in xs


__all__ = [
    "add_in_place",
    "add_range",
    "add_range_in_place",
    "exists",
    "find_index",
    "find_last",
    "find_last_index",
    "get_sub_array",
    "iterate",
    "remove_all_in_place",
    "remove_range",
]
