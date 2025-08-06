from __future__ import annotations

import builtins
from base64 import b64decode, b64encode
from typing import Any

from .core import strings
from .types import Array


# Re-export classes from core.strings
IPrintfFormat = strings.IPrintfFormat
StringComparison = strings.StringComparison

# Re-export functions from core.strings - now with proper F# semantics
printf = strings.printf
continue_print = strings.continue_print
to_console = strings.to_console
to_text = strings.to_text
format_replacement = strings.format_replacement
format = strings.format
initialize = strings.initialize
insert = strings.insert
is_null_or_empty = strings.is_null_or_empty
is_null_or_white_space = strings.is_null_or_white_space
concat = strings.concat
join = strings.join
pad_left = strings.pad_left
pad_right = strings.pad_right
remove = strings.remove
replace = strings.replace
replicate = strings.replicate
get_char_at_index = strings.get_char_at_index
split = strings.split
trim = strings.trim
trim_start = strings.trim_start
trim_end = strings.trim_end
filter = strings.filter
substring = strings.substring
to_char_array2 = strings.to_char_array2
compare = strings.compare
starts_with = strings.starts_with
ends_with = strings.ends_with
index_of = strings.index_of
last_index_of = strings.last_index_of
interpolate = strings.interpolate


# Additional helper functions that might be needed for backward compatibility
def to_console_error(arg: Any) -> None:
    """Print to console error stream."""

    def print_func(x: str) -> None:
        builtins.print(x)  # noqa: T201

    return continue_print(print_func, arg)


def to_fail(arg: Any) -> None:
    """Fail with message."""

    def fail(msg: str) -> None:
        raise Exception(msg)

    return continue_print(fail, arg)


# Utility functions for base64 encoding/decoding
def to_base64string(in_array: Array[int]) -> str:
    """Convert byte array to base64 string."""
    return b64encode(bytes(in_array)).decode("utf8")


def from_base64string(b64encoded: str) -> Array[int]:
    """Convert base64 string to byte array."""
    return Array[int](b64decode(b64encoded))


def join_with_indices(delimiter: str, xs: list[str], startIndex: int, count: int) -> str:
    """Join strings with start index and count."""
    endIndexPlusOne = startIndex + count
    if endIndexPlusOne > len(xs):
        raise ValueError("Index and count must refer to a location within the buffer.")
    return delimiter.join(xs[startIndex:endIndexPlusOne])


def not_supported(name: str) -> None:
    """Raise not supported exception."""
    raise Exception("The environment doesn't support '" + name + "', please use a polyfill.")


# Additional functions that might be needed but not in Rust implementation yet
def compare_to(this: str, other: str) -> int:
    """Compare this string with other string using current culture."""
    return compare(this, other)


def ends_with_exact(string: str, pattern: str) -> bool:
    """Check if string ends with pattern (exact match)."""
    idx = string.rfind(pattern)
    return idx >= 0 and idx == len(string) - len(pattern)


def starts_with_exact(string: str, pattern: str) -> bool:
    """Check if string starts with pattern (exact match)."""
    idx = string.find(pattern)
    return idx == 0


def index_of_any(string: str, any_of: list[str], *args: int) -> int:
    """Find index of any character from the list."""
    if not string:
        return -1

    start_index = args[0] if len(args) > 0 else 0
    if start_index < 0:
        raise ValueError("Start index cannot be negative")

    length = args[1] if len(args) > 1 else len(string) - start_index
    if length < 0:
        raise ValueError("Length cannot be negative")

    if length > len(string) - start_index:
        raise ValueError("Invalid start_index and length")

    search_string = string[start_index : start_index + length]
    any_of_str = "".join(any_of)
    for i, c in enumerate(search_string):
        if c in any_of_str:
            return i + start_index

    return -1


__all__ = [
    "IPrintfFormat",
    "StringComparison",
    "compare",
    "compare_to",
    "concat",
    "continue_print",
    "ends_with",
    "ends_with_exact",
    "filter",
    "format",
    "format_replacement",
    "from_base64string",
    "get_char_at_index",
    "index_of",
    "index_of_any",
    "initialize",
    "insert",
    "interpolate",
    "is_null_or_empty",
    "is_null_or_white_space",
    "join",
    "join_with_indices",
    "last_index_of",
    "not_supported",
    "pad_left",
    "pad_right",
    "printf",
    "remove",
    "replace",
    "replicate",
    "split",
    "starts_with",
    "starts_with_exact",
    "substring",
    "to_base64string",
    "to_char_array2",
    "to_console",
    "to_console_error",
    "to_fail",
    "to_text",
    "trim",
    "trim_end",
    "trim_start",
]
