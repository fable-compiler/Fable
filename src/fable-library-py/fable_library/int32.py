from .core._core import get_range
from .core._core import parse_int32 as parse
from .core._core import try_parse_int32 as try_parse


# Re-export constants for compatibility
AllowHexSpecifier = 0x00000200


def op_unary_negation_int8(x: int) -> int:
    return x if x == -128 else -x


def op_unary_negation_int16(x: int) -> int:
    return x if x == -32768 else -x


def op_unary_negation_int32(x: int) -> int:
    return x if x == -2147483648 else -x


__all__ = [
    "get_range",
    "op_unary_negation_int8",
    "op_unary_negation_int16",
    "op_unary_negation_int32",
    "parse",
    "try_parse",
]
