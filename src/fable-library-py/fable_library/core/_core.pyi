"""Stub file for the Fable library core module.

This is only needed so that the static type checker can find the types for the extension
methods we have written in Rust. The file will never be used by Python at runtime.
"""

from . import array, floats, ints, option, strings, types
from .array import FSharpArray
from .floats import Float32, Float64
from .ints import (
    ALLOW_HEX_SPECIFIER,
    ALLOW_LEADING_WHITE,
    ALLOW_TRAILING_WHITE,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    get_range,
    get_range_64,
    parse_int32,
    parse_int64,
    try_parse_int32,
    try_parse_int64,
)
from .types import FSharpRef

__all__: list[str] = [
    "ALLOW_HEX_SPECIFIER",
    "ALLOW_LEADING_WHITE",
    "ALLOW_TRAILING_WHITE",
    "FSharpArray",
    "FSharpRef",
    "Float32",
    "Float64",
    "Int8",
    "Int16",
    "Int32",
    "Int64",
    "UInt8",
    "UInt16",
    "UInt32",
    "UInt64",
    "array",
    "floats",
    "get_range",
    "get_range_64",
    "ints",
    "option",
    "parse_int32",
    "parse_int64",
    "strings",
    "try_parse_int32",
    "try_parse_int64",
    "types",
]
