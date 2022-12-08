from decimal import Decimal
from typing import Any

from .types import FSharpRef


def from_zero() -> int:
    return 0


def from_one() -> int:
    return 1


def from_int32(x: int) -> int:
    return int(x)


def from_int64(x: int) -> int:
    return int(x)


def from_string(x: str) -> int:
    return int(x)


def op_addition(a: int, b: int) -> int:
    return a + b


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


def equals(a: int, b: int) -> bool:
    return a == b


def try_parse(string: str, def_value: FSharpRef[int]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


def op_right_shift(a: int, num_bits: int) -> int:
    return a >> num_bits


def of_left_shift(a: int, num_bits: int) -> int:
    return a << num_bits


BigInteger = int

__all__ = [
    "BigInteger",
    "equals",
    "from_zero",
    "from_one",
    "from_int32",
    "from_int64",
    "from_string",
    "to_decimal",
    "to_double",
    "to_single",
    "to_uint64",
    "to_int64",
    "to_uint32",
    "to_int32",
    "to_uint16",
    "to_int16",
    "to_byte",
    "to_sbyte",
    "to_string",
    "op_addition",
    "parse",
    "try_parse",
]
