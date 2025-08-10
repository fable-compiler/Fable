from decimal import Decimal
from typing import Any

from .types import FSharpRef, int32, int64


def compare(x: int, y: int) -> int:
    return -1 if x < y else 1 if x > y else 0


def equals(a: int, b: int) -> bool:
    return a == b


def abs(x: int) -> int:
    return -x if x < 0 else x


def sign(x: int) -> int:
    return -1 if x < 0 else 1 if x > 0 else 0


def max(x: int, y: int) -> int:
    return x if x > y else y


def min(x: int, y: int) -> int:
    return x if x < y else y


def add(x: int, y: int) -> int:
    return x + y


def subtract(x: int, y: int) -> int:
    return x - y


def multiply(x: int, y: int) -> int:
    return x * y


def divide(x: int, y: int) -> int:
    return x // y


def remainder(x: int, y: int) -> int:
    return x % y


def negate(x: int) -> int:
    return -x


def op_unary_negation(x: int) -> int:
    return -x


def op_logical_not(x: int) -> int:
    return ~x


def op_unary_plus(x: int) -> int:
    return x


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


def op_right_shift(a: int, num_bits: int) -> int:
    return a >> num_bits


def op_left_shift(a: int, num_bits: int) -> int:
    return a << num_bits


def op_bitwise_and(x: int, y: int) -> int:
    return x & y


def op_bitwise_or(x: int, y: int) -> int:
    return x | y


def op_exclusive_or(x: int, y: int) -> int:
    return x ^ y


def op_less_than(x: int, y: int) -> bool:
    return x < y


def op_less_than_or_equal(x: int, y: int) -> bool:
    return x <= y


def op_greater_than(x: int, y: int) -> bool:
    return x > y


def op_greater_than_or_equal(x: int, y: int) -> bool:
    return x >= y


def op_equality(x: int, y: int) -> bool:
    return x == y


def op_inequality(x: int, y: int) -> bool:
    return x != y


def get_zero() -> int:
    return 0


def get_one() -> int:
    return 1


def get_minus_one() -> int:
    return -1


def get_is_zero(x: int) -> bool:
    return x == 0


def get_is_one(x: int) -> bool:
    return x == 1


def get_is_even(x: int) -> bool:
    return is_even_integer(x)


def get_is_power_of_two(x: int) -> bool:
    return is_pow2(x)


def get_sign(x: int) -> int:
    return sign(x)


def is_negative(x: int) -> bool:
    return x < 0


def is_positive(x: int) -> bool:
    return x > 0


def is_even_integer(x: int) -> bool:
    return x % 2 == 0


def is_odd_integer(x: int) -> bool:
    return x % 2 != 0


def is_pow2(x: int) -> bool:
    return (x & (x - 1)) == 0


def from_zero() -> int:
    return 0


def from_one() -> int:
    return 1


def from_int32(x: int32) -> int:
    return int(x)


def from_int64(x: int64) -> int:
    return int(x)


def from_string(x: str) -> int:
    return int(x)


def parse(value: Any) -> int:
    return int(value)


def to_string(value: int) -> str:
    return str(value)


def to_byte(value: int) -> int:
    return value


def to_sbyte(value: int) -> int:
    return value


def to_int16(value: int) -> int:
    return value


def to_uint16(value: int) -> int:
    return value


def to_int32(value: int) -> int:
    return value


def to_uint32(value: int) -> int:
    return value


def to_int64(value: int) -> int:
    return value


def to_uint64(value: int) -> int:
    return value


def to_single(value: int) -> float:
    return float(value)


def to_double(value: int) -> float:
    return float(value)


def to_decimal(value: int) -> Decimal:
    return Decimal(value)


def try_parse(string: str, def_value: FSharpRef[int]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


BigInteger = int

__all__ = [
    "BigInteger",
    "abs",
    "add",
    "compare",
    "divide",
    "equals",
    "from_int32",
    "from_int64",
    "from_one",
    "from_string",
    "from_zero",
    "get_is_even",
    "get_is_one",
    "get_is_power_of_two",
    "get_is_zero",
    "get_minus_one",
    "get_one",
    "get_sign",
    "get_zero",
    "is_even_integer",
    "is_negative",
    "is_odd_integer",
    "is_positive",
    "is_pow2",
    "max",
    "min",
    "multiply",
    "negate",
    "op_addition",
    "op_bitwise_and",
    "op_bitwise_or",
    "op_division",
    "op_equality",
    "op_exclusive_or",
    "op_greater_than",
    "op_greater_than_or_equal",
    "op_inequality",
    "op_left_shift",
    "op_less_than",
    "op_less_than_or_equal",
    "op_logical_not",
    "op_modulus",
    "op_multiply",
    "op_right_shift",
    "op_subtraction",
    "op_unary_negation",
    "op_unary_plus",
    "parse",
    "remainder",
    "sign",
    "subtract",
    "to_byte",
    "to_decimal",
    "to_double",
    "to_int16",
    "to_int32",
    "to_int64",
    "to_sbyte",
    "to_single",
    "to_string",
    "to_uint16",
    "to_uint32",
    "to_uint64",
    "try_parse",
]
