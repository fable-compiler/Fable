"""Stub file for the Fable library core module.

This is only needed so that the static type checker can find the types for the extension
methods we have written in Rust. The file will never be used by Python at runtime.
"""

from . import array, floats, ints, option, types
from .array import FSharpArray
from .floats import Float32, Float64
from .ints import Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64
from .types import FSharpRef

__all__: list[str] = [
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
    "ints",
    "option",
    "types",
]
