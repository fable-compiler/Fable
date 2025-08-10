from typing import Any, Literal, SupportsFloat, SupportsInt, overload

from .core._core import get_range_64 as get_range
from .core._core import parse_int64 as parse
from .core._core import try_parse_int64 as try_parse
from .types import float64, int32, int64, uint64


def compare(x: int64, y: int64) -> int64:
    return int64.NEG_ONE if x < y else int64.ONE if x > y else int64.ZERO


def sign(x: int64) -> int64:
    return int64.NEG_ONE if x < int64.ZERO else int64.ONE if x > int64.ZERO else int64.ZERO


@overload
def max(x: int64, y: int64) -> int64: ...


@overload
def max(x: uint64, y: uint64) -> uint64: ...


def max(x: int64 | uint64, y: int64 | uint64) -> int64 | uint64:
    return x if x > y else y


@overload
def min(x: int64, y: int64) -> int64: ...


@overload
def min(x: uint64, y: uint64) -> uint64: ...


def min(x: int64 | uint64, y: int64 | uint64) -> int64 | uint64:
    return x if x < y else y


def op_unary_negation(value: int64) -> int64:
    # Note that we cannot negate the smallest negative number
    return -value if value != int64.NEG_ONE else int64.NEG_ONE


@overload
def op_unary_plus(a: int64) -> int64: ...


@overload
def op_unary_plus(a: uint64) -> uint64: ...


def op_unary_plus(a: int64 | uint64) -> int64 | uint64:
    return +a


@overload
def op_logical_not(a: int64) -> int64: ...


@overload
def op_logical_not(a: uint64) -> uint64: ...


def op_logical_not(a: int64 | uint64) -> int64 | uint64:
    return ~a


@overload
def op_addition(a: int64, b: int64) -> int64: ...


@overload
def op_addition(a: uint64, b: uint64) -> uint64: ...


def op_addition(a: int64 | uint64, b: int64 | uint64) -> int64 | uint64:
    return a + b


@overload
def op_subtraction(a: int64, b: int64) -> int64: ...


@overload
def op_subtraction(a: uint64, b: uint64) -> uint64: ...


def op_subtraction(a: int64 | uint64, b: int64 | uint64) -> int64 | uint64:
    return a - b


@overload
def op_multiply(a: int64, b: int64) -> int64: ...


@overload
def op_multiply(a: uint64, b: uint64) -> uint64: ...


def op_multiply(a: int64 | uint64, b: int64 | uint64) -> int64 | uint64:
    return a * b


@overload
def op_division(a: int64, b: int64) -> int64: ...


@overload
def op_division(a: uint64, b: uint64) -> uint64: ...


def op_division(a: int64 | uint64, b: int64 | uint64) -> int64 | uint64:
    return a // b


@overload
def op_modulus(a: int64, b: int64) -> int64: ...


@overload
def op_modulus(a: uint64, b: uint64) -> uint64: ...


def op_modulus(a: int64 | uint64, b: int64 | uint64) -> int64 | uint64:
    return a % b


@overload
def op_right_shift(a: int64, b: int32) -> int64: ...


@overload
def op_right_shift(a: uint64, b: int32) -> uint64: ...


def op_right_shift(a: int64 | uint64, b: int32) -> int64 | uint64:
    return a >> b


@overload
def op_left_shift(a: int64, b: int32) -> int64: ...


@overload
def op_left_shift(a: uint64, b: int32) -> uint64: ...


def op_left_shift(a: int64 | uint64, b: int32) -> int64 | uint64:
    return a << b


@overload
def op_bitwise_and(a: int64, b: int64) -> int64: ...


@overload
def op_bitwise_and(a: uint64, b: uint64) -> uint64: ...


def op_bitwise_and(a: int64 | uint64, b: int64 | uint64) -> int64 | uint64:
    return a & b


@overload
def op_bitwise_or(a: int64, b: int64) -> int64: ...


@overload
def op_bitwise_or(a: uint64, b: uint64) -> uint64: ...


def op_bitwise_or(a: int64 | uint64, b: int64 | uint64) -> int64 | uint64:
    return a | b


@overload
def op_exclusive_or(a: int64, b: int64) -> int64: ...


@overload
def op_exclusive_or(a: uint64, b: uint64) -> uint64: ...


def op_exclusive_or(a: int64 | uint64, b: int64 | uint64) -> int64 | uint64:
    return a ^ b


def op_less_than(a: int64 | uint64, b: int64 | uint64) -> bool:
    return a < b


def op_less_than_or_equal(a: int64 | uint64, b: int64 | uint64) -> bool:
    return a <= b


def op_greater_than(a: int64, b: int64) -> bool:
    return a > b


def op_greater_than_or_equal(a: int64, b: int64) -> bool:
    return a >= b


def op_equality(a: int64, b: int64) -> bool:
    return a == b


def op_inequality(a: int64, b: int64) -> bool:
    return a != b


def from_bits(lowBits: int64, high_bits: int64, unsigned: bool) -> int64:
    ret = lowBits + (high_bits << 32)
    if not unsigned and ret > 0x7FFFFFFFFFFFFFFF:
        return ret - 0x10000000000000000

    return ret


def from_int(value: int64, unsigned: bool = False) -> int64:
    if unsigned and value < 0:
        return value + 0x10000000000000000
    return value


def from_value(value: Any, unsigned: bool = False) -> int64:
    value = int(value)
    if unsigned and value < 0:
        return value + 0x10000000000000000
    elif not unsigned and value > 9223372036854775807:
        return value - 0x10000000000000000

    return value


@overload
def from_number(value: SupportsInt, unsigned: Literal[True]) -> uint64: ...


@overload
def from_number(value: SupportsInt, unsigned: Literal[False]) -> int64: ...


def from_number(value: SupportsInt, unsigned: bool) -> int64 | uint64:
    if unsigned:
        return uint64(value)
    else:
        return int64(value)


def to_number(value: SupportsFloat | SupportsInt) -> float64:
    return float64(value)


@overload
def from_integer(value: int, unsigned: Literal[True], kind: int) -> uint64: ...


@overload
def from_integer(value: int, unsigned: Literal[False], kind: int) -> int64: ...


def from_integer(value: int, unsigned: bool | None = None, kind: int | None = None) -> int64 | uint64:
    if unsigned:
        return uint64(value)
    else:
        return int64(value)


# Re-export constants for compatibility
AllowHexSpecifier = 0x00000200


def to_string(x: int64 | uint64) -> str:
    return str(x)


__all__ = [
    "compare",
    "from_bits",
    "from_int",
    "from_integer",
    "from_number",
    "from_value",
    "get_range",
    "max",
    "max",
    "min",
    "min",
    "op_addition",
    "op_addition",
    "op_bitwise_and",
    "op_bitwise_and",
    "op_bitwise_or",
    "op_bitwise_or",
    "op_division",
    "op_division",
    "op_equality",
    "op_equality",
    "op_exclusive_or",
    "op_exclusive_or",
    "op_greater_than",
    "op_greater_than",
    "op_greater_than_or_equal",
    "op_greater_than_or_equal",
    "op_inequality",
    "op_inequality",
    "op_left_shift",
    "op_left_shift",
    "op_less_than",
    "op_less_than",
    "op_less_than_or_equal",
    "op_less_than_or_equal",
    "op_logical_not",
    "op_logical_not",
    "op_modulus",
    "op_modulus",
    "op_multiply",
    "op_multiply",
    "op_right_shift",
    "op_right_shift",
    "op_subtraction",
    "op_subtraction",
    "op_unary_negation",
    "op_unary_negation",
    "op_unary_plus",
    "op_unary_plus",
    "parse",
    "parse",
    "sign",
    "sign",
    "to_number",
    "to_string",
    "to_string",
    "try_parse",
    "try_parse",
]
