from typing import TypeAlias

# from ._core import Int64 as int64  # type: ignore
from ._core import (
    Int8,  # type: ignore
    Int16,  # type: ignore
    Int32,  # type: ignore
    UInt8,  # type: ignore
    UInt16,  # type: ignore
    UInt32,  # type: ignore
)


# from ._core import UInt64 as uint64  # type: ignore

# Type aliases for the built-in types
byte: TypeAlias = UInt8
sbyte: TypeAlias = Int8
int16: TypeAlias = Int16
uint16: TypeAlias = UInt16
int32: TypeAlias = Int32
uint32: TypeAlias = UInt32

__all__ = [
    "sbyte",
    "byte",
    "uint16",
    "int16",
    "int32",
    "uint32",
    "Int8",
    "UInt8",
    "Int16",
    "UInt16",
    "Int32",
    "UInt32",
]
