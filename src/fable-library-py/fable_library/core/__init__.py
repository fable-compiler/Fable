from typing import TypeAlias

from ._core import (
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
)


# Type aliases for the built-in types
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

__all__ = [
    "Int8",
    "Int16",
    "Int32",
    "Int64",
    "UInt8",
    "UInt16",
    "UInt32",
    "UInt64",
    "byte",
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
