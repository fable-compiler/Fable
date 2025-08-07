"""Stub file for Fable integer types."""

from typing import Any, ClassVar, Protocol, Self, SupportsFloat, SupportsInt, final

class NumericInts(Protocol):
    """Protocol for Integer types"""

    def to_string(self, radix: int = 10) -> str: ...
    def __init__(self, value: SupportsInt | SupportsFloat) -> None: ...
    def __add__(self, other: Any) -> Self: ...
    def __radd__(self, other: Any) -> Self: ...
    def __sub__(self, other: Any) -> Self: ...
    def __rsub__(self, other: Any) -> Any: ...
    def __mul__(self, other: Any) -> Self: ...
    def __rmul__(self, other: Any) -> Any: ...
    def __truediv__(self, other: NumericInts | int | float) -> Self: ...
    def __rtruediv__(self, other: NumericInts | int | float) -> Self: ...
    def __mod__(self, other: Any) -> Self: ...
    def __rmod__(self, other: Any) -> Any: ...
    def __neg__(self) -> Self: ...
    def __pos__(self) -> Self: ...
    def __eq__(self, other: Any) -> bool: ...
    def __ne__(self, other: Any) -> bool: ...
    def __lt__(self, other: Any) -> bool: ...
    def __le__(self, other: Any) -> bool: ...
    def __gt__(self, other: Any) -> bool: ...
    def __ge__(self, other: Any) -> bool: ...
    def __int__(self) -> int: ...
    def __index__(self) -> int: ...
    def __hash__(self) -> int: ...
    def GetHashCode(self) -> Int32: ...
    def __lshift__(self, other: int | NumericInts) -> Self: ...
    def __rlshift__(self, other: int) -> Any: ...
    def __rshift__(self, other: int | NumericInts) -> Self: ...
    def __rrshift__(self, other: int) -> Any: ...
    def __floordiv__(self, other: NumericInts | int | float) -> Self: ...
    def __rfloordiv__(self, other: NumericInts | int | float) -> Self: ...
    def __and__(self, other: int | NumericInts) -> Self: ...
    def __rand__(self, other: int | NumericInts) -> Self: ...
    def __or__(self, other: int | NumericInts) -> Self: ...
    def __ror__(self, other: int | NumericInts) -> Self: ...
    def __xor__(self, other: int | NumericInts) -> Self: ...
    def __rxor__(self, other: int | NumericInts) -> Self: ...
    def __invert__(self) -> Self: ...
    def __abs__(self) -> Self: ...

@final
class UInt8(NumericInts):
    ZERO: ClassVar[UInt8]
    ONE: ClassVar[UInt8]
    TWO: ClassVar[UInt8]
    THREE: ClassVar[UInt8]
    FOUR: ClassVar[UInt8]
    FIVE: ClassVar[UInt8]
    SIX: ClassVar[UInt8]
    SEVEN: ClassVar[UInt8]
    EIGHT: ClassVar[UInt8]
    NINE: ClassVar[UInt8]
    TEN: ClassVar[UInt8]
    SIXTEEN: ClassVar[UInt8]
    THIRTY_TWO: ClassVar[UInt8]
    SIXTY_FOUR: ClassVar[UInt8]
    NEG_ONE: ClassVar[UInt8]

@final
class Int8(NumericInts):
    ZERO: ClassVar[Int8]
    ONE: ClassVar[Int8]
    TWO: ClassVar[Int8]
    THREE: ClassVar[Int8]
    FOUR: ClassVar[Int8]
    FIVE: ClassVar[Int8]
    SIX: ClassVar[Int8]
    SEVEN: ClassVar[Int8]
    EIGHT: ClassVar[Int8]
    NINE: ClassVar[Int8]
    TEN: ClassVar[Int8]
    SIXTEEN: ClassVar[Int8]
    THIRTY_TWO: ClassVar[Int8]
    SIXTY_FOUR: ClassVar[Int8]
    NEG_ONE: ClassVar[Int8]

@final
class UInt16(NumericInts):
    ZERO: ClassVar[UInt16]
    ONE: ClassVar[UInt16]
    TWO: ClassVar[UInt16]
    THREE: ClassVar[UInt16]
    FOUR: ClassVar[UInt16]
    FIVE: ClassVar[UInt16]
    SIX: ClassVar[UInt16]
    SEVEN: ClassVar[UInt16]
    EIGHT: ClassVar[UInt16]
    NINE: ClassVar[UInt16]
    TEN: ClassVar[UInt16]
    SIXTEEN: ClassVar[UInt16]
    THIRTY_TWO: ClassVar[UInt16]
    SIXTY_FOUR: ClassVar[UInt16]
    NEG_ONE: ClassVar[UInt16]

@final
class Int16(NumericInts):
    ZERO: ClassVar[Int16]
    ONE: ClassVar[Int16]
    TWO: ClassVar[Int16]
    THREE: ClassVar[Int16]
    FOUR: ClassVar[Int16]
    FIVE: ClassVar[Int16]
    SIX: ClassVar[Int16]
    SEVEN: ClassVar[Int16]
    EIGHT: ClassVar[Int16]
    NINE: ClassVar[Int16]
    TEN: ClassVar[Int16]
    SIXTEEN: ClassVar[Int16]
    THIRTY_TWO: ClassVar[Int16]
    SIXTY_FOUR: ClassVar[Int16]
    NEG_ONE: ClassVar[Int16]

@final
class UInt32(NumericInts):
    ZERO: ClassVar[UInt32]
    ONE: ClassVar[UInt32]
    TWO: ClassVar[UInt32]
    THREE: ClassVar[UInt32]
    FOUR: ClassVar[UInt32]
    FIVE: ClassVar[UInt32]
    SIX: ClassVar[UInt32]
    SEVEN: ClassVar[UInt32]
    EIGHT: ClassVar[UInt32]
    NINE: ClassVar[UInt32]
    TEN: ClassVar[UInt32]
    SIXTEEN: ClassVar[UInt32]
    THIRTY_TWO: ClassVar[UInt32]
    SIXTY_FOUR: ClassVar[UInt32]
    NEG_ONE: ClassVar[UInt32]

@final
class Int32(NumericInts, int):
    # Note that this is not really a subclass of int
    ZERO: ClassVar[Int32]
    ONE: ClassVar[Int32]
    TWO: ClassVar[Int32]
    THREE: ClassVar[Int32]
    FOUR: ClassVar[Int32]
    FIVE: ClassVar[Int32]
    SIX: ClassVar[Int32]
    SEVEN: ClassVar[Int32]
    EIGHT: ClassVar[Int32]
    NINE: ClassVar[Int32]
    TEN: ClassVar[Int32]
    SIXTEEN: ClassVar[Int32]
    THIRTY_TWO: ClassVar[Int32]
    SIXTY_FOUR: ClassVar[Int32]
    NEG_ONE: ClassVar[Int32]

@final
class UInt64(NumericInts):
    ZERO: ClassVar[UInt64]
    ONE: ClassVar[UInt64]
    TWO: ClassVar[UInt64]
    THREE: ClassVar[UInt64]
    FOUR: ClassVar[UInt64]
    FIVE: ClassVar[UInt64]
    SIX: ClassVar[UInt64]
    SEVEN: ClassVar[UInt64]
    EIGHT: ClassVar[UInt64]
    NINE: ClassVar[UInt64]
    TEN: ClassVar[UInt64]
    SIXTEEN: ClassVar[UInt64]
    THIRTY_TWO: ClassVar[UInt64]
    SIXTY_FOUR: ClassVar[UInt64]
    NEG_ONE: ClassVar[UInt64]

@final
class Int64(NumericInts):
    ZERO: ClassVar[Int64]
    ONE: ClassVar[Int64]
    TWO: ClassVar[Int64]
    THREE: ClassVar[Int64]
    FOUR: ClassVar[Int64]
    FIVE: ClassVar[Int64]
    SIX: ClassVar[Int64]
    SEVEN: ClassVar[Int64]
    EIGHT: ClassVar[Int64]
    NINE: ClassVar[Int64]
    TEN: ClassVar[Int64]
    SIXTEEN: ClassVar[Int64]
    THIRTY_TWO: ClassVar[Int64]
    SIXTY_FOUR: ClassVar[Int64]
    NEG_ONE: ClassVar[Int64]

# Integer parsing functions with F#-compatible semantics
def parse_int32(
    string: str,
    style: int,
    unsigned: bool,
    radix: int = 10,
) -> Int32:
    """
    Parses a string representation of a 32-bit integer with F#-compatible semantics.
    This function matches the behavior of int32.py parse function exactly.

    Args:
        string: The string to parse
        style: NumberStyles flags controlling parsing behavior
        unsigned: Whether to treat the result as unsigned (u32)
        radix: Default radix to use (defaults to 10)

    Returns:
        The parsed integer value as int

    Raises:
        ValueError: If the string is not in a valid format or value is out of range
    """
    ...

def parse_int64(
    string: str,
    style: int,
    unsigned: bool,
    radix: int = 10,
) -> Int64:
    """
    Parses a string representation of a 64-bit integer with F#-compatible semantics.
    This function matches the behavior of long.py parse function exactly.

    Args:
        string: The string to parse
        style: NumberStyles flags controlling parsing behavior
        unsigned: Whether to treat the result as unsigned (u64)
        radix: Default radix to use (defaults to 10)

    Returns:
        The parsed integer value as int

    Raises:
        ValueError: If the string is not in a valid format or value is out of range
    """
    ...

def try_parse_int32(
    string: str,
    style: int,
    unsigned: bool,
    def_value: Any,
    radix: int = 10,
) -> bool:
    """
    Attempts to parse a 32-bit integer with F#-style try semantics.

    Args:
        string: The string to parse
        style: NumberStyles flags controlling parsing behavior
        unsigned: Whether to treat the result as unsigned (u32)
        def_value: Python object reference to store the result on success
        radix: Default radix to use (defaults to 10)

    Returns:
        True if parsing succeeded, False otherwise
    """
    ...

def try_parse_int64(
    string: str,
    style: int,
    unsigned: bool,
    def_value: Any,
    radix: int = 10,
) -> bool:
    """
    Attempts to parse a 64-bit integer with F#-style try semantics.

    Args:
        string: The string to parse
        style: NumberStyles flags controlling parsing behavior
        unsigned: Whether to treat the result as unsigned (u64)
        def_value: Python object reference to store the result on success
        radix: Default radix to use (defaults to 10)

    Returns:
        True if parsing succeeded, False otherwise
    """
    ...

def get_range(unsigned: bool, bitsize: int) -> tuple[int, int]:
    """
    Returns the valid range for a given bit size and signedness.
    This matches the behavior of int32.py get_range function.

    Args:
        unsigned: Whether the range should be for unsigned integers
        bitsize: The bit width (8, 16, or 32)

    Returns:
        A tuple containing (minimum_value, maximum_value) for the specified type
    """
    ...

def get_range_64(unsigned: bool) -> tuple[int, int]:
    """
    Returns the valid range for 64-bit integers.
    This matches the behavior of long.py get_range function.

    Args:
        unsigned: Whether the range should be for unsigned integers

    Returns:
        A tuple containing (minimum_value, maximum_value) for 64-bit integers
    """
    ...

# .NET NumberStyles constants
ALLOW_HEX_SPECIFIER: int
ALLOW_LEADING_WHITE: int
ALLOW_TRAILING_WHITE: int

__all__ = [
    "ALLOW_HEX_SPECIFIER",
    "ALLOW_LEADING_WHITE",
    "ALLOW_TRAILING_WHITE",
    "Int8",
    "Int16",
    "Int32",
    "Int64",
    "UInt8",
    "UInt16",
    "UInt32",
    "UInt64",
    "get_range",
    "get_range_64",
    "parse_int32",
    "parse_int64",
    "try_parse_int32",
    "try_parse_int64",
]
