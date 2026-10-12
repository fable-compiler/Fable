"""Tests for the generic comparison helpers in `fable_library.util`.

These return `System.Int32`, which is a plain Python `int`. Every branch except a
user-supplied `CompareTo` yields an in-range literal or an already-normalized
`compare`, so only that one branch normalizes — these tests pin that down.
"""

from typing import Any

from fable_library.util import compare, compare_arrays, compare_dicts, compare_primitives


class OutOfRangeCompareTo:
    """`IComparable` whose `CompareTo` returns a value outside the 32-bit range."""

    def __init__(self, result: int) -> None:
        self._result = result

    def CompareTo(self, other: Any) -> int:
        return self._result


def test_compare_normalizes_out_of_range_compare_to() -> None:
    assert compare(OutOfRangeCompareTo(2**40), object()) == 0
    assert compare(OutOfRangeCompareTo(2147483648), object()) == -2147483648
    assert compare(OutOfRangeCompareTo(-2147483649), object()) == 2147483647
    assert compare(OutOfRangeCompareTo(-1), object()) == -1


def test_compare_arrays_and_dicts_propagate_a_normalized_result() -> None:
    """Neither helper normalizes; both return literals or a `compare` result."""
    assert compare_arrays([OutOfRangeCompareTo(2**40)], [object()]) == 0
    assert compare_arrays([OutOfRangeCompareTo(2147483648)], [object()]) == -2147483648
    assert compare_dicts({"a": OutOfRangeCompareTo(2**40)}, {"a": object()}) == 0
    assert compare_dicts({"a": OutOfRangeCompareTo(2147483648)}, {"a": object()}) == -2147483648


def test_compare_arrays_handles_none_and_length() -> None:
    assert compare_arrays(None, None) == 0
    assert compare_arrays(None, []) == 1
    assert compare_arrays([], None) == -1
    assert compare_arrays([1], [1, 2]) == -1
    assert compare_arrays([1, 2], [1]) == 1
    assert compare_arrays([1, 2], [1, 2]) == 0


def test_compare_dicts_orders_by_key_then_value() -> None:
    assert compare_dicts({"a": 1}, {"a": 1, "b": 2}) == -1
    assert compare_dicts({"a": 1}, {"b": 1}) == -1
    assert compare_dicts({"b": 1}, {"a": 1}) == 1
    assert compare_dicts({"a": 1}, {"a": 2}) == -1
    assert compare_dicts({"a": 1}, {"a": 1}) == 0


def test_compare_primitives_returns_plain_ints() -> None:
    assert compare_primitives(1, 1) == 0
    assert compare_primitives(1, 2) == -1
    assert compare_primitives(2, 1) == 1
    assert compare_primitives("a", "b") == -1
    assert type(compare_primitives(1, 2)) is int


def test_compare_primitives_matches_dotnet_nan_ordering() -> None:
    """.NET's `Double.CompareTo` treats NaN as equal to NaN and less than anything else."""
    nan = float("nan")
    assert compare_primitives(nan, nan) == 0
    assert compare_primitives(nan, 1.0) == -1
    assert compare_primitives(1.0, nan) == 1
