from typing import Any, Optional, Tuple

from .types import FSharpRef


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


def to_number(value: int) -> float:
    return float(value)


def from_integer(
    value: int, unsigned: Optional[bool] = None, kind: Optional[int] = None
) -> int:
    if unsigned and value < 0:
        return value + 0x10000000000000000
    elif not unsigned and value > 9223372036854775807:
        return value - 0x10000000000000000
    return value


def op_left_shift(self: int, num_bits: int):
    return self << num_bits


def op_addition(a: int, b: int) -> int:
    return a + b


def op_subtraction(a: int, b: int) -> int:
    return a - b


def op_multiply(a: int, b: int) -> int:
    return a * b


def op_unary_negation(value: int) -> int:
    # Note that we cannot negate the smallest negative number
    return -value if value != -0x8000000000000000 else -0x8000000000000000


def get_range(unsigned: bool) -> Tuple[int, int]:
    return (
        (0, 18446744073709551615)
        if unsigned
        else (-9223372036854775808, 9223372036854775807)
    )


AllowHexSpecifier = 0x00000200


def parse(
    string: str, style: int, unsigned: bool, bitsize: int, radix: int = 10
) -> int:
    # const res = isValid(str, style, radix);
    if style & AllowHexSpecifier or string.startswith("0x"):
        radix = 16
    elif string.startswith("0b"):
        radix = 2
    elif string.startswith("0o"):
        radix = 8

    try:
        v = int(string, base=radix)
    except Exception:
        raise ValueError("Input string was not in a correct format.")

    (umin, umax) = get_range(True)
    if not unsigned and radix != 10 and v >= umin and v <= umax:
        mask = 1 << (bitsize - 1)
        if v & mask:  # Test if negative
            v = v - (mask << 1)

    (min, max) = get_range(unsigned)
    if v >= min and v <= max:
        return v

    raise ValueError("Input string was not in a correct format.")


def try_parse(
    string: str, style: int, unsigned: bool, bitsize: int, defValue: FSharpRef[int]
) -> bool:
    try:
        defValue.contents = parse(string, style, unsigned, bitsize)
        return True
    except Exception:
        return False


def to_string(x: int) -> str:
    return str(x)


def to_int(value: int) -> int:
    if value > 9223372036854775807:
        return value - 0x10000000000000000
    return value


def compare(a: int, b: int) -> int:
    return 0 if a == b else -1 if a < b else 1


def equals(a: int, b: int) -> bool:
    return a == b


long = int

__all__ = [
    "from_bits",
    "from_int",
    "from_value",
    "from_number",
    "to_number",
    "from_integer",
    "op_left_shift",
    "op_addition",
    "op_multiply",
    "op_unary_negation",
    "get_range",
    "parse",
    "try_parse",
    "to_string",
    "to_int",
    "compare",
    "equals",
    "long",
]
