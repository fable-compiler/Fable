from __future__ import annotations

from typing import SupportsInt

from .array_ import Array
from .core import byte
from .system import (
    ArgumentNullException__ctor_Z721C83C5,
    ArgumentOutOfRangeException__ctor_Z384F8060,
    ArgumentOutOfRangeException__ctor_Z721C83C5,
    FormatException__ctor_Z721C83C5,
)


_HEX_DIGITS = frozenset("0123456789abcdefABCDEF")


def _hex_input(in_array: Array[byte], offset: SupportsInt | None, length: SupportsInt | None) -> bytes:
    if in_array is None:
        raise ArgumentNullException__ctor_Z721C83C5("inArray")
    data = bytes(in_array)
    start = 0 if offset is None else int(offset)
    count = len(data) - start if length is None else int(length)
    if count < 0:
        raise ArgumentOutOfRangeException__ctor_Z384F8060("length", "Non-negative number required.")
    if start < 0:
        raise ArgumentOutOfRangeException__ctor_Z384F8060("offset", "Non-negative number required.")
    if start > len(data) - count:
        raise ArgumentOutOfRangeException__ctor_Z721C83C5("offset")
    return data[start : start + count]


def to_hex_string(in_array: Array[byte], offset: SupportsInt | None = None, length: SupportsInt | None = None) -> str:
    """Convert a byte array, or length bytes of it from offset, to uppercase hex (Convert.ToHexString)."""
    return _hex_input(in_array, offset, length).hex().upper()


def to_hex_string_lower(
    in_array: Array[byte], offset: SupportsInt | None = None, length: SupportsInt | None = None
) -> str:
    """Convert a byte array, or length bytes of it from offset, to lowercase hex (Convert.ToHexStringLower)."""
    return _hex_input(in_array, offset, length).hex()


def from_hex_string(s: str) -> Array[byte]:
    """Convert a hex string of either case to a byte array (Convert.FromHexString)."""
    if s is None:
        raise ArgumentNullException__ctor_Z721C83C5("s")
    if len(s) % 2 != 0:
        raise FormatException__ctor_Z721C83C5(
            "The input is not a valid hex string as its length is not a multiple of 2."
        )
    if not _HEX_DIGITS.issuperset(s):
        raise FormatException__ctor_Z721C83C5("The input is not a valid hex string as it contains a non-hex character.")
    return Array[byte](byte(x) for x in bytes.fromhex(s))
