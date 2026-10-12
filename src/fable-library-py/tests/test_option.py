"""Tests for nested options, represented by `fable_library.core.option.SomeWrapper`.

Only nesting materializes a wrapper: `Some 42` and `Some (Some 42)` are the bare value,
`Some None` is `SomeWrapper(None)`. Two options of the same F# type can therefore put a
wrapper next to `None` or next to a bare value.
"""

from fable_library.option import some
from fable_library.util import compare, compare_arrays, equal_arrays, equals


def test_equality_between_different_nesting_depths() -> None:
    assert (some(None) == some(some(None))) is False
    assert (some(some(None)) == some(None)) is False
    assert (some(None) != some(some(None))) is True
    assert equals(some(None), some(some(None))) is False


def test_equality_between_equal_nesting_depths() -> None:
    assert (some(None) == some(None)) is True
    assert (some(some(None)) == some(some(None))) is True
    assert equals(some(some(None)), some(some(None))) is True


def test_equality_against_none_and_bare_values() -> None:
    assert (some(None) == None) is False  # noqa: E711
    assert (None == some(None)) is False  # noqa: E711
    assert (some(None) == 42) is False
    assert (42 == some(None)) is False


def test_equality_is_inherited_by_containers() -> None:
    assert equal_arrays([some(None)], [some(some(None))]) is False
    assert equal_arrays([some(None)], [some(None)]) is True
    assert compare_arrays([some(None)], [some(some(None))]) == -1


def test_comparison_orders_none_before_some() -> None:
    assert compare(some(None), some(some(None))) == -1
    assert compare(some(some(None)), some(None)) == 1
    assert compare(some(None), some(None)) == 0
    assert compare(None, some(None)) == -1
    assert compare(some(None), None) == 1
    # `int option option`: `Some None` against `Some (Some 42)`
    assert compare(some(None), 42) == -1
    assert compare(42, some(None)) == 1
