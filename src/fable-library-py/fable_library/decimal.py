from decimal import Decimal, MAX_EMAX, MIN_EMIN

from .types import FSharpRef

get_zero = Decimal(0)

get_one = Decimal(1)

get_minus_one = Decimal(-1)

get_max_value = MAX_EMAX

get_min_value = MIN_EMIN


def from_parts(low: int, mid: int, high: int, isNegative: bool, scale: int):
    sign = -1 if isNegative else 1

    if low < 0:
        low = 0x100000000 + low

    if mid < 0:
        mid = 0xFFFFFFFF00000000 + mid + 1
    else:
        mid = mid << 32

    if high < 0:
        high = 0xFFFFFFFF0000000000000000 + high + 1
    else:
        high = high << 64

    value = Decimal((low + mid + high) * sign)
    if scale:
        dscale = Decimal(pow(10, scale))
        return value / dscale
    return value


def op_addition(x: Decimal, y: Decimal):
    return x + y


def to_string(x: Decimal):
    return str(x)


def to_number(x: Decimal):
    return float(x)


def parse(string: str):
    return Decimal(string)


def try_parse(string: str, def_value: FSharpRef[float]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


def equals(a: Decimal, b: Decimal) -> bool:
    return a == b
