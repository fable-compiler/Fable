from typing import Optional

from .types import int64


def to_fixed(x: float, dp: Optional[int] = None) -> str:
    x = int(x) if int(x) == x else x

    if dp is not None:
        fmt = "{:.%sf}" % dp
        return fmt.format(x)

    return "{}".format(x)


def to_precision(x: float, sd: Optional[int] = None) -> str:
    x = int(x) if int(x) == x else x

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
    def rshift(val: int, n: int) -> int:
        sign = 0x10000000000000000 if isinstance(val, int64) else 0x100000000
        return val >> n if val >= 0 else (val + sign) >> n

    return "{0:x}".format(rshift(x, 0))


def multiply(x: float, y: float) -> float:
    return x * y


__all__ = ["to_fixed", "to_precision", "to_exponential", "to_hex", "multiply"]
