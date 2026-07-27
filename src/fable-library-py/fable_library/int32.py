from typing import SupportsInt, overload

from .core import FSharpRef, Int32, int8, int16, int32
from .core._core import get_range
from .core._core import parse_int32 as parse
from .core._core import try_parse_int32 as try_parse


# Re-export constants for compatibility
AllowHexSpecifier = 0x00000200


def sign(x: int) -> int:
    return -1 if x < 0 else 1 if x > 0 else 0


def to_string(value: int, radix: int = 10) -> str:
    """`Int32.ToString()`, optionally in another radix (2, 8, 10 or 16).

    Non-decimal radixes render the two's complement bit pattern, so this defers to
    the wrapper rather than to Python's `format`, which would render a sign instead.
    """
    if radix == 10:
        return str(value)

    return Int32(value).to_string(radix)


@overload
def div_rem[T: SupportsInt](x: T, y: T) -> tuple[T, T]: ...


@overload
def div_rem[T: SupportsInt](x: T, y: T, out: FSharpRef[T]) -> T: ...


def div_rem[T: SupportsInt](x: T, y: T, out: FSharpRef[T] | None = None) -> T | tuple[T, T]:
    if type(x) is int and type(y) is int:
        # Plain ints are Int32 (or bigint). Python's // floors and % takes the sign of
        # the divisor; .NET truncates toward zero and takes the sign of the dividend.
        q = -((-x) // y) if (x < 0) != (y < 0) else x // y
        r = x - q * y
    else:
        # The Rust wrapper types already use truncated division and remainder
        q = x // y  # type: ignore[operator]  # ty: ignore[unsupported-operator]
        r = x % y  # type: ignore[operator]  # ty: ignore[unsupported-operator]
    if out is None:
        return (q, r)
    out.contents = r
    return q


def op_unary_negation_int8(x: int8) -> int8:
    return -x


def op_unary_negation_int16(x: int16) -> int16:
    return -x


def op_unary_negation_int32(x: int) -> int:
    return int32(-x)


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
