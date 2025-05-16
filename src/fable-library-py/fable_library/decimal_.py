from decimal import MAX_EMAX, MIN_EMIN, Decimal, getcontext

from .types import FSharpRef, IntegerTypes, byte, float32, float64, int16, int32, int64, sbyte, uint16, uint32, uint64


getcontext().prec = 29

get_zero = Decimal(0)
get_one = Decimal(1)

get_minus_one = Decimal(-1)
get_max_value = MAX_EMAX
get_min_value = MIN_EMIN


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


def to_string(x: Decimal) -> str:
    return str(x)


def to_number(x: Decimal) -> float64:
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


def create(value: float | float32 | float64 | IntegerTypes | str) -> Decimal:
    match value:
        case sbyte() | byte() | int16() | uint16() | int32() | uint32() | int64() | uint64():
            return Decimal(int(value))
        case float32() | float64():
            return Decimal(float(value))
        case _:
            return Decimal(value)


__all__ = [
    "Decimal",
    "abs",
    "add",
    "compare",
    "divide",
    "equals",
    "from_parts",
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
