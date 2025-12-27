"""Type stubs for fable_library.core module."""

from typing import Literal

from .array import FSharpArray
from .floats import Float32, Float64
from .ints import Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64
from .types import FSharpRef as FSharpRef

Array = FSharpArray

ArrayType = Literal[
    "Int8",
    "UInt8",
    "Int16",
    "UInt16",
    "Int32",
    "UInt32",
    "Int64",
    "UInt64",
    "Float32",
    "Float64",
    "String",
    "Bool",
    "Generic",
]

# Class aliases - simple assignments work for both type annotations and class attribute access
byte = UInt8
sbyte = Int8
uint8 = UInt8
int8 = Int8
int16 = Int16
uint16 = UInt16
int32 = Int32
uint32 = UInt32
int64 = Int64
uint64 = UInt64

# Float aliases
float32 = Float32
float64 = Float64

__all__ = [
    "Array",
    "ArrayType",
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
    "byte",
    "float32",
    "float64",
    "int8",
    "int16",
    "int32",
    "int64",
    "sbyte",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
]
