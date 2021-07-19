from decimal import Decimal, MAX_EMAX, MIN_EMIN

get_Zero = Decimal(0)

get_One = Decimal(1)

get_MinusOne = Decimal(-1)

get_MaxValue = MAX_EMAX

get_MinValue = MIN_EMIN


def fromParts(low: int, mid: int, high: int, isNegative: bool, scale: int):
    sign = Decimal(-1) if isNegative else Decimal(1)
    dlow = Decimal(low)
    dmid = Decimal(mid << 32)
    dhigh = Decimal(high << 64)
    dscale = Decimal(pow(10, scale))

    return (dlow + dmid + dhigh) / dscale * sign


def op_Addition(x: Decimal, y: Decimal):
    return x + y
