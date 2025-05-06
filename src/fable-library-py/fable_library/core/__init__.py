from typing import Literal, TypeAlias, TypeVar

from ._core import (
    Float32,
    Float64,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    array,
    datetime_offset,
    option,
)


_T = TypeVar("_T")

# Type aliases for the built-in types
Array: TypeAlias = array.FSharpArray[_T]
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

byte: TypeAlias = UInt8
sbyte: TypeAlias = Int8
uint8: TypeAlias = UInt8
int8: TypeAlias = Int8
int16: TypeAlias = Int16
uint16: TypeAlias = UInt16
int32: TypeAlias = Int32
uint32: TypeAlias = UInt32
int64: TypeAlias = Int64
uint64: TypeAlias = UInt64
float32: TypeAlias = Float32
float64: TypeAlias = Float64

__all__: list[str] = [
    "ArrayType",
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
    "byte",
    "datetime_offset",
    "float32",
    "float64",
    "int8",
    "int16",
    "int32",
    "int64",
    "option",
    "sbyte",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
]
