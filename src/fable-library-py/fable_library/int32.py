from .core import int8, int16, int32
from .core._core import get_range
from .core._core import parse_int32 as parse
from .core._core import try_parse_int32 as try_parse


# Re-export constants for compatibility
AllowHexSpecifier = 0x00000200


def op_unary_negation_int8(x: int8) -> int8:
    return -x


def op_unary_negation_int16(x: int16) -> int16:
    return -x


def op_unary_negation_int32(x: int32) -> int32:
    return -x


__all__ = [
    "AllowHexSpecifier",
    "get_range",
    "op_unary_negation_int8",
    "op_unary_negation_int16",
    "op_unary_negation_int32",
    "parse",
    "try_parse",
]
