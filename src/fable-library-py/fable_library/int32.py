from typing import SupportsInt, overload

from .core import FSharpRef, Int32, int8, int16, int32
from .core._core import get_range
from .core._core import parse_int32 as _parse_int32
from .core._core import try_parse_int32 as _try_parse_int32


# Re-export constants for compatibility
AllowHexSpecifier = 0x00000200


def sign(x: int) -> int:
    return -1 if x < 0 else 1 if x > 0 else 0


def parse(string: str, style: int, radix: int = 10) -> int:
    """`Int32.Parse`, returning a plain Python `int`.

    The wrapper class's `Int32.parse` hands back an `Int32` object. Int32 is
    represented as a plain `int`, so parsing goes to the free function instead, which
    already validates the result against the 32-bit range and returns a plain int.
    """
    return _parse_int32(string, style, False, 32, radix)


def try_parse(string: str, style: int, def_value: FSharpRef[int]) -> bool:
    """`Int32.TryParse`, storing a plain Python `int` in `def_value`.

    `Int32.try_parse` would store an `Int32` object instead. See `parse` above.
    """
    return _try_parse_int32(string, style, False, 32, def_value)


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
