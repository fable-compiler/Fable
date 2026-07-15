import pytest
from fable_library.util import assert_equal, assert_not_equal


def test_assert_equal_passes_on_equal_values():
    assert_equal(1, 1)
    assert_equal([1, 2], [1, 2])


def test_assert_equal_raises_on_mismatch():
    with pytest.raises(Exception, match=r"^Expected: 1 - Actual: 2$"):
        assert_equal(2, 1)


def test_assert_equal_uses_the_custom_message():
    with pytest.raises(Exception, match=r"^values differ$"):
        assert_equal(2, 1, "values differ")


def test_assert_not_equal_passes_on_different_values():
    assert_not_equal(1, 2)


def test_assert_not_equal_raises_when_equal():
    # The failure has to describe what was expected -- some value *other* than 1. Rendering it as
    # "Expected: 1 - Actual: 1" reads as a passing assertion, which is what it used to say.
    with pytest.raises(Exception, match=r"^Expected not equal to: 1 - Actual: 1$"):
        assert_not_equal(1, 1)


def test_assert_not_equal_uses_the_custom_message():
    with pytest.raises(Exception, match=r"^values are equal$"):
        assert_not_equal(1, 1, "values are equal")
