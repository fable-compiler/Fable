from decimal import Decimal, MAX_EMAX, MIN_EMIN

get_Zero = Decimal(0)

get_One = Decimal(1)

get_MinusOne = Decimal(-1)

get_MaxValue = MAX_EMAX

get_MinValue = MIN_EMIN


def fromParts(low: int, mid: int, high: int, isNegative: bool, scale: int):
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


def op_Addition(x: Decimal, y: Decimal):
    return x + y


def toString(x):
    return str(x)
