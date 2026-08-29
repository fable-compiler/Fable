"""`Double.ToString()` semantics for the plain-float representation of Float64."""

import math

from fable_library.exceptions import float_to_string, to_string


def test_float_to_string_shortens_whole_doubles() -> None:
    assert float_to_string(5.0) == "5"
    assert float_to_string(-5.0) == "-5"
    assert float_to_string(2.5) == "2.5"
    assert float_to_string(-0.5) == "-0.5"


def test_float_to_string_spells_out_the_specials() -> None:
    # `int()` raises OverflowError on the infinities and ValueError on NaN, so the
    # whole-number path must not be reached for them
    assert float_to_string(math.inf) == "Infinity"
    assert float_to_string(-math.inf) == "-Infinity"
    assert float_to_string(math.nan) == "NaN"


def test_to_string_routes_floats() -> None:
    assert to_string(math.inf) == "Infinity"
    assert to_string(-math.inf) == "-Infinity"
    assert to_string(math.nan) == "NaN"
    assert to_string(5.0) == "5"


def test_to_string_keeps_the_other_arms() -> None:
    # bool is not a float, so it still takes its own arm
    assert to_string(True) == "true"
    assert to_string(False) == "false"
    assert to_string(5) == "5"
    assert to_string("x") == "x"
