from typing import SupportsInt, overload

from .core import FSharpRef, int8, int16, int32
from .core._core import get_range
from .core._core import parse_int32 as parse
from .core._core import try_parse_int32 as try_parse


# Re-export constants for compatibility
AllowHexSpecifier = 0x00000200


def sign(x: int32) -> int32:
    return int32.NEG_ONE if x < int32.ZERO else int32.ONE if x > int32.ZERO else int32.ZERO


@overload
def div_rem[T: SupportsInt](x: T, y: T) -> tuple[T, T]: ...


@overload
def div_rem[T: SupportsInt](x: T, y: T, out: FSharpRef[T]) -> T: ...


def div_rem[T: SupportsInt](x: T, y: T, out: FSharpRef[T] | None = None) -> T | tuple[T, T]:
    # Rust wrapper types already use truncated division and remainder
    q = x // y  # type: ignore[operator]
    r = x % y  # type: ignore[operator]
    if out is None:
        return (q, r)
    out.contents = r
    return q


def op_unary_negation_int8(x: int8) -> int8:
    return -x


def op_unary_negation_int16(x: int16) -> int16:
    return -x


def op_unary_negation_int32(x: int32) -> int32:
    return -x


__all__ = [
    "AllowHexSpecifier",
    "div_rem",
    "get_range",
    "op_unary_negation_int8",
    "op_unary_negation_int16",
    "op_unary_negation_int32",
    "parse",
    "try_parse",
]
