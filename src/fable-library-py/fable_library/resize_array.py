from __future__ import annotations

from collections.abc import Callable
from typing import Any

from .option import Option, some


def exists[T](predicate: Callable[[T], bool], xs: list[T]) -> bool:
    """Test if a predicate is true for at least one element in a list."""
    return any(predicate(x) for x in xs)


def find_index[T](predicate: Callable[[T], bool], xs: list[T]) -> int:
    """Find the index of the first element in a list that satisfies a predicate."""

    for i, x in enumerate(xs):
        if predicate(x):
            return i
    return -1


def remove[T](item: T, xs: list[T]) -> bool:
    """Remove an item from a list in-place. Returns True if the item was removed, False if it was not found."""
    try:
        xs.remove(item)
        return True
    except ValueError:
        return False


def remove_range[T](start: int, count: int, xs: list[T]) -> None:
    """Remove a range of elements from a list in-place."""

    del xs[start : start + count]


def remove_all_in_place[T](predicate: Callable[[T], bool], xs: list[T]) -> int:
    """Remove all elements matching predicate from the list in-place. Returns the number of removed elements."""
    # More efficient O(n) approach: build list of items to keep
    original_length = len(xs)
    write_index = 0

    for read_index in range(len(xs)):
        if not predicate(xs[read_index]):
            if write_index != read_index:
                xs[write_index] = xs[read_index]
            write_index += 1

    # Truncate the list to remove unwanted elements
    del xs[write_index:]

    return original_length - write_index


def find_last_index[T](predicate: Callable[[T], bool], xs: list[T]) -> int:
    """Find the index of the last element in a list that satisfies a predicate."""
    for i in range(len(xs) - 1, -1, -1):
        if predicate(xs[i]):
            return i
    return -1


def try_find[T](predicate: Callable[[T], bool], xs: list[T]) -> Option[T]:
    """Find the first element that satisfies the predicate, returning None if not found."""
    try:
        return some(next(x for x in xs if predicate(x)))
    except StopIteration:
        return None


def find_last[T](predicate: Callable[[T], bool], xs: list[T]) -> T | None:
    """Return the last element in the list that satisfies the predicate, or None if not found."""
    for x in reversed(xs):
        if predicate(x):
            return x
    return None


def filter[T](predicate: Callable[[T], bool], xs: list[T]) -> list[T]:
    """Return a new list of elements that satisfy the predicate."""
    return [x for x in xs if predicate(x)]


def index_of[T](value: T, start: int, count: int | None, xs: list[T]) -> int:
    """Return the index of value in xs, or -1 if not found. Specify start and count."""
    end = min(len(xs), start + count if count is not None else len(xs))
    try:
        # Use built-in index method with start/stop parameters for better performance
        return xs.index(value, start, end)
    except ValueError:
        return -1


def insert_range_in_place[T](index: int, items: list[T], xs: list[T]) -> None:
    """Insert a range of items into xs at the given index."""
    xs[index:index] = items


def add_in_place[T](x: T, xs: list[T]) -> None:
    """Add an item to xs in-place."""
    xs.append(x)


def add_range_in_place[T](items: list[T], array: list[T]) -> None:
    """Add a range of items to the array."""
    # Use extend for better performance instead of individual appends
    array.extend(items)


def add_range[T](index: int, items: list[T], xs: list[T]) -> list[T]:
    """Add a range of items to xs at the given index."""
    return xs[:index] + items + xs[index:]


def get_sub_array[T](xs: list[T], start: int, count: int) -> list[T]:
    """Get a sub-array of xs from the given start index and count."""
    return xs[start : start + count]


def iterate[T](action: Callable[[T], None], xs: list[T]) -> None:
    """Iterate over a list and apply an action to each element."""
    for x in xs:
        action(x)


def contains[T](value: T, xs: list[T], cons: Any | None = None) -> bool:
    """Check if a value is contained in the list."""
    return value in xs


__all__ = [
    "add_in_place",
    "add_range",
    "add_range_in_place",
    "contains",
    "exists",
    "filter",
    "find_index",
    "find_last",
    "find_last_index",
    "get_sub_array",
    "index_of",
    "insert_range_in_place",
    "iterate",
    "remove",
    "remove_all_in_place",
    "remove_range",
    "try_find",
]
