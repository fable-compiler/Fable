from typing import Literal, SupportsInt

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
    "Bool",
    "Generic",
]
FSharpRef = types.FSharpRef

byte = UInt8
sbyte = Int8
uint8 = UInt8
int8 = Int8
int16 = Int16
uint16 = UInt16
uint32 = UInt32
int64 = Int64
uint64 = UInt64
float32 = floats.Float32
float64 = floats.Float64

# System.Int32 is represented as a plain Python `int`, which is arbitrary precision,
# so the compiler normalizes back into 32 bits after operations that can leave the
# range. The `Int32` wrapper class is still exported for interop, but generated code
# no longer constructs it.
#
# These are deliberately pure Python: a call into the Rust extension costs roughly
# twice what the whole helper does, because crossing the boundary is the expensive
# part, not the arithmetic.

_INT32_MIN = -2147483648
_INT32_MAX = 2147483647


def int32(value: SupportsInt = 0, /) -> int:
    """Normalize `value` to a signed 32-bit int, wrapping as .NET does."""
    number: int = value if type(value) is int else int(value)

    if _INT32_MIN <= number <= _INT32_MAX:
        return number

    return ((number + 2147483648) & 4294967295) - 2147483648


def op_remainder_int32(dividend: int, divisor: int, /) -> int:
    """Remainder taking the sign of the dividend, as in .NET.

    Python's `%` takes the sign of the divisor instead, so `-5 % 3` is 1 here where
    .NET gives -2.
    """
    rest = dividend % divisor

    if rest and (rest < 0) != (dividend < 0):
        return rest - divisor

    return rest


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
    "op_remainder_int32",
    "option",
    "sbyte",
    "strings",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
]
