from decimal import Decimal, MAX_EMAX, MIN_EMIN

get_Zero = Decimal(0)

get_One = Decimal(1)

get_MinusOne = Decimal(-1)

get_MaxValue = MAX_EMAX

get_MinValue = MIN_EMIN


def fromParts(low: int, mid: int, high: int, isNegative: bool, scale: int):
    print("low: ", low)
    print("mid: ", mid)
    print("high: ", high)
    print("scale: ", scale)
    return Decimal()


def op_Addition(x: Decimal, y: Decimal):
    return x + y
