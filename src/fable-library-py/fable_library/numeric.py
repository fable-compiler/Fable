from typing import Optional


def to_fixed(x: float, dp: Optional[int] = None) -> str:
    if dp is not None:
        fmt = "{:.%sf}" % dp
        return fmt.format(x)

    return "{}".format(x)


def to_precision(x: float, sd: Optional[int] = None) -> str:
    if sd is not None:
        fmt = "{:.%se}" % sd
        return fmt.format(x)

    return "{}".format(x)


def to_exponential(x: float, dp: Optional[int] = None) -> str:
    if dp is not None:
        fmt = "{:.%se}" % dp
        return fmt.format(x)

    return "{}".format(x)


def to_hex(x: int) -> str:
    def rshift(val, n):
        sign = 0x100000000 if x < 0x100000000 else 0x10000000000000000
        return val >> n if val >= 0 else (val + sign) >> n

    return "{0:x}".format(rshift(x, 0))


def multiply(x: float, y: float) -> float:
    return x * y
