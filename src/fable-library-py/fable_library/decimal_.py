from decimal import Decimal, getcontext

from .core import Array, FSharpRef, array, byte, float32, float64, int16, int64, sbyte, uint16, uint32, uint64
from .types import IntegerTypes


getcontext().prec = 29

get_zero = Decimal(0)
get_one = Decimal(1)

get_minus_one = Decimal(-1)
get_max_value = Decimal("79228162514264337593543950335")
get_min_value = Decimal("-79228162514264337593543950335")


def compare(x: Decimal, y: Decimal) -> int:
    return -1 if x < y else 1 if x > y else 0


def equals(a: Decimal, b: Decimal) -> bool:
    return a == b


def abs(x: Decimal) -> Decimal:
    return -x if x < 0 else x


def sign(x: Decimal) -> int:
    return -1 if x < 0 else 1 if x > 0 else 0


def max(x: Decimal, y: Decimal) -> Decimal:
    return x if x > y else y


def min(x: Decimal, y: Decimal) -> Decimal:
    return x if x < y else y


def add(a: Decimal, b: Decimal) -> Decimal:
    return a + b


def subtract(a: Decimal, b: Decimal) -> Decimal:
    return a - b


def multiply(a: Decimal, b: Decimal) -> Decimal:
    return a * b


def divide(a: Decimal, b: Decimal) -> Decimal:
    return a / b


def remainder(a: Decimal, b: Decimal) -> Decimal:
    return a % b


def negate(a: Decimal) -> Decimal:
    return -a


def op_unary_negation(a: Decimal) -> Decimal:
    return -a


def op_unary_plus(a: Decimal) -> Decimal:
    return +a


def op_addition(a: Decimal, b: Decimal) -> Decimal:
    return a + b


def op_subtraction(a: Decimal, b: Decimal) -> Decimal:
    return a - b


def op_multiply(a: Decimal, b: Decimal) -> Decimal:
    return a * b


def op_division(a: Decimal, b: Decimal) -> Decimal:
    return a / b


def op_modulus(a: Decimal, b: Decimal) -> Decimal:
    return a % b


def op_less_than(a: Decimal, b: Decimal) -> bool:
    return a < b


def op_less_than_or_equal(a: Decimal, b: Decimal) -> bool:
    return a <= b


def op_greater_than(a: Decimal, b: Decimal) -> bool:
    return a > b


def op_greater_than_or_equal(a: Decimal, b: Decimal) -> bool:
    return a >= b


def op_equality(a: Decimal, b: Decimal) -> bool:
    return a == b


def op_inequality(a: Decimal, b: Decimal) -> bool:
    return a != b


def from_parts(
    low: IntegerTypes, mid: IntegerTypes, high: IntegerTypes, is_negative: IntegerTypes, scale: IntegerTypes
) -> Decimal:
    sign = -1 if is_negative else 1

    _low, _mid, _high, _scale = int(low), int(mid), int(high), int(scale)

    if _low < 0:
        _low = 0x100000000 + _low

    if _mid < 0:
        _mid = 0xFFFFFFFF00000000 + _mid + 1
    else:
        _mid = _mid << 32

    if _high < 0:
        _high = 0xFFFFFFFF0000000000000000 + _high + 1
    else:
        _high = _high << 64

    value = Decimal((_low + _mid + _high) * sign)
    if scale:
        dscale = Decimal(pow(10, _scale))
        return value / dscale
    return value


def from_ints(low: IntegerTypes, mid: IntegerTypes, high: IntegerTypes, sign_exp: IntegerTypes) -> Decimal:
    _sign_exp = int(sign_exp)
    is_negative = 1 if _sign_exp < 0 else 0
    scale = (_sign_exp >> 16) & 0x7F
    return from_parts(low, mid, high, is_negative, scale)


def from_int_array(bits: Array[int]) -> Decimal:
    return from_ints(bits[0], bits[1], bits[2], bits[3])


def _to_int32(value: int) -> int:
    masked = value & 0xFFFFFFFF
    return masked - 0x100000000 if masked >= 0x80000000 else masked


def get_bits(value: Decimal) -> Array[int]:
    sign, digits, exponent = value.as_tuple()

    if not isinstance(exponent, int):
        raise ValueError(f"The value {value} cannot be represented as a System.Decimal.")

    mantissa = int("".join(str(digit) for digit in digits))
    scale = 0

    if exponent > 0:
        mantissa *= 10**exponent
    else:
        scale = -exponent

    flags = ((scale & 0x7F) << 16) | (0x80000000 if sign else 0)

    return array.Int32Array(
        [
            _to_int32(mantissa),
            _to_int32(mantissa >> 32),
            _to_int32(mantissa >> 64),
            _to_int32(flags),
        ]
    )


def to_string(x: Decimal) -> str:
    return str(x)


def to_number(x: Decimal) -> float:
    return float64(x)


def to_int(x: Decimal) -> int:
    return int(x)


def parse(string: str) -> Decimal:
    return Decimal(string)


def try_parse(string: str, def_value: FSharpRef[Decimal]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


def _from_float(value: float, significant_digits: int) -> Decimal:
    rounded = Decimal(f"{value:.{significant_digits}G}")

    if rounded.is_zero():
        return get_zero

    # `Decimal` keeps the exponent it was parsed with, which would print as 1E+20
    return Decimal(format(rounded, "f"))


def create(value: float | float32 | IntegerTypes | str) -> Decimal:
    match value:
        # Int32 is a plain `int`, which `Decimal` already accepts via the last case
        case sbyte() | byte() | int16() | uint16() | uint32() | int64() | uint64():
            return Decimal(int(value))
        # .NET rounds to 7 significant digits for Single and 15 for Double
        case float32():
            return _from_float(float(value), 7)
        case float():
            return _from_float(value, 15)
        case _:
            return Decimal(value)


__all__ = [
    "Decimal",
    "abs",
    "add",
    "compare",
    "divide",
    "equals",
    "from_int_array",
    "from_ints",
    "from_parts",
    "get_bits",
    "max",
    "min",
    "multiply",
    "negate",
    "op_addition",
    "op_division",
    "op_equality",
    "op_greater_than",
    "op_greater_than_or_equal",
    "op_inequality",
    "op_less_than",
    "op_less_than_or_equal",
    "op_modulus",
    "op_multiply",
    "op_subtraction",
    "op_unary_negation",
    "op_unary_plus",
    "parse",
    "remainder",
    "sign",
    "subtract",
    "to_int",
    "to_number",
    "to_string",
    "try_parse",
]
