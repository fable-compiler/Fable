from __future__ import annotations

from .core import int64, uint32, uint64
from .types import IntegerTypes


def to_fixed(x: float, dp: int | None = None) -> str:
    x = int(x) if int(x) == x else x

    if dp is not None:
        fmt = f"{{:.{dp}f}}"
        return fmt.format(x)

    return f"{x}"


def to_precision(x: float, sd: int | None = None) -> str:
    x = int(x) if int(x) == x else x

    if sd is not None:
        fmt = f"{{:.{sd}e}}"
        return fmt.format(x)

    return f"{x}"


def to_exponential(x: float, dp: int | None = None) -> str:
    if dp is not None:
        fmt = f"{{:.{dp}e}}"
        return fmt.format(x)

    return f"{x}"


def to_hex(x: IntegerTypes) -> str:
    # For int64/uint64, convert negative numbers to unsigned 64-bit
    if isinstance(x, int64 | uint64):
        if x < 0:
            x = (1 << 64) + x
        val = uint64(x)
    else:
        val = uint32(x)
    return f"{val:x}"


def multiply(x: float, y: float) -> float:
    return x * y


__all__ = ["multiply", "to_exponential", "to_fixed", "to_hex", "to_precision"]
