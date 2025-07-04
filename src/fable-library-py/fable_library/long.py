from typing import Any

from .core._core import get_range_64 as get_range
from .core._core import parse_int64 as parse
from .core._core import try_parse_int64 as try_parse
from .types import float64, int64, uint64


def compare(x: int, y: int) -> int:
    return -1 if x < y else 1 if x > y else 0


def sign(x: int) -> int:
    return -1 if x < 0 else 1 if x > 0 else 0


def max(x: int, y: int) -> int:
    return x if x > y else y


def min(x: int, y: int) -> int:
    return x if x < y else y


def op_unary_negation(value: int) -> int:
    # Note that we cannot negate the smallest negative number
    return -value if value != -0x8000000000000000 else -0x8000000000000000


def op_unary_plus(a: int) -> int:
    return +a


def op_logical_not(a: int) -> int:
    return ~a


def op_addition(a: int, b: int) -> int:
    return a + b


def op_subtraction(a: int, b: int) -> int:
    return a - b


def op_multiply(a: int, b: int) -> int:
    return a * b


def op_division(a: int, b: int) -> int:
    return a // b


def op_modulus(a: int, b: int) -> int:
    return a % b


def op_right_shift(a: int, b: int) -> int:
    return a >> b


def op_left_shift(a: int, b: int) -> int:
    return a << b


def op_bitwise_and(a: int, b: int) -> int:
    return a & b


def op_bitwise_or(a: int, b: int) -> int:
    return a | b


def op_exclusive_or(a: int, b: int) -> int:
    return a ^ b


def op_less_than(a: int, b: int) -> bool:
    return a < b


def op_less_than_or_equal(a: int, b: int) -> bool:
    return a <= b


def op_greater_than(a: int, b: int) -> bool:
    return a > b


def op_greater_than_or_equal(a: int, b: int) -> bool:
    return a >= b


def op_equality(a: int, b: int) -> bool:
    return a == b


def op_inequality(a: int, b: int) -> bool:
    return a != b


def from_bits(lowBits: int, highBits: int, unsigned: bool) -> int:
    ret = lowBits + (highBits << 32)
    if not unsigned and ret > 0x7FFFFFFFFFFFFFFF:
        return ret - 0x10000000000000000

    return ret


def from_int(value: int, unsigned: bool = False) -> int:
    if unsigned and value < 0:
        return value + 0x10000000000000000
    return value


def from_value(value: Any, unsigned: bool = False) -> int:
    value = int(value)
    if unsigned and value < 0:
        return value + 0x10000000000000000
    elif not unsigned and value > 9223372036854775807:
        return value - 0x10000000000000000

    return value


def from_number(value: int, unsigned: bool) -> int:
    return int(value)


def to_number(value: int64) -> float64:
    return float64(value)


def from_integer(value: int, unsigned: bool | None = None, kind: int | None = None) -> int64 | uint64:
    if unsigned:
        return uint64(value)
    else:
        return int64(value)


# Re-export constants for compatibility
AllowHexSpecifier = 0x00000200


def to_string(x: int) -> str:
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
