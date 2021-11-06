from decimal import Decimal
from typing import Any

from .types import FSharpRef


def from_zero() -> int:
    return 0


def from_one() -> int:
    return 1


def from_int32(x: int) -> int:
    return x


def from_int64(x: int) -> int:
    return x


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


def try_parse(string: str, defValue: FSharpRef[int]) -> bool:
    try:
        defValue.contents = parse(string)
        return True
    except Exception:
        return False
