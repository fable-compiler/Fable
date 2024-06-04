from typing import Any

from .types import FSharpRef


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


def op_unary_negation(value: int) -> int:
    # Note that we cannot negate the smallest negative number
    return -value if value != -0x8000000000000000 else -0x8000000000000000


# def op_unary_negation(a: int) -> int: return -a
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


def to_number(value: int) -> float:
    return float(value)


def from_integer(value: int, unsigned: bool | None = None, kind: int | None = None) -> int:
    if unsigned and value < 0:
        return value + 0x10000000000000000
    elif not unsigned and value > 9223372036854775807:
        return value - 0x10000000000000000
    return value


def get_range(unsigned: bool) -> tuple[int, int]:
    return (0, 18446744073709551615) if unsigned else (-9223372036854775808, 9223372036854775807)


AllowHexSpecifier = 0x00000200


def parse(string: str, style: int, unsigned: bool, bitsize: int, radix: int = 10) -> int:
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
        raise ValueError(f"The input string {string} was not in a correct format.")

    (umin, umax) = get_range(True)
    if not unsigned and radix != 10 and v >= umin and v <= umax:
        mask = 1 << (bitsize - 1)
        if v & mask:  # Test if negative
            v = v - (mask << 1)

    (min, max) = get_range(unsigned)
    if v >= min and v <= max:
        return v

    raise ValueError(f"The input string {string} was not in a correct format.")


def try_parse(string: str, style: int, unsigned: bool, bitsize: int, defValue: FSharpRef[int]) -> bool:
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


long = int

__all__ = [
    "compare",
    "equals",
    "abs",
    "sign",
    "max",
    "min",
    "op_unary_negation",
    "op_unary_plus",
    "op_logical_not",
    "op_addition",
    "op_subtraction",
    "op_multiply",
    "op_division",
    "op_modulus",
    "op_right_shift",
    "op_left_shift",
    "op_bitwise_and",
    "op_bitwise_or",
    "op_exclusive_or",
    "op_less_than",
    "op_less_than_or_equal",
    "op_greater_than",
    "op_greater_than_or_equal",
    "op_equality",
    "op_inequality",
    "from_bits",
    "from_int",
    "from_value",
    "from_number",
    "to_number",
    "from_integer",
    "get_range",
    "parse",
    "try_parse",
    "to_string",
    "to_int",
    "long",
]
