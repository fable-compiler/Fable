from typing import Literal

from ._core import (
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    array,
    floats,
    option,
    strings,
    types,
)


# Note: We don't use type aliases here because since we use the types as
# functions

Array = array.FSharpArray
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
    "Generic",
]
FSharpRef = types.FSharpRef

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
float32 = floats.Float32
float64 = floats.Float64

__all__: list[str] = [
    "ArrayType",
    "FSharpRef",
    "Int8",
    "Int16",
    "Int32",
    "Int64",
    "UInt8",
    "UInt16",
    "UInt32",
    "UInt64",
    "array",
    "byte",
    "float32",
    "float64",
    "floats",
    "int8",
    "int16",
    "int32",
    "int64",
    "option",
    "sbyte",
    "strings",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
]
