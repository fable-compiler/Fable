"""Stub file for the DateTimeOffset module.

This is only needed so that the static type checker can find the types for the extension
methods we have written in Rust. The file will never be used by Python at runtime.
"""

from datetime import datetime

# Only include the now function for now
def now() -> datetime: ...

# Commented out functions
# def add(d: datetime, ts: timedelta) -> datetime: ...
# def parse(string: str, detect_utc: bool = False) -> datetime: ...
# def try_parse(string: str, style: int, unsigned: bool, bitsize: int, def_value: FSharpRef[datetime]) -> bool: ...
# @overload
# def create(
#     year: int,
#     month: int,
#     day: int,
#     h: int,
#     m: int,
#     s: int,
#     ms: int,
#     offset: timedelta | None = None,
# ) -> datetime: ...
# @overload
# def create(
#     year: int,
#     month: int,
#     day: int,
#     h: int,
#     m: int,
#     s: int,
#     ms: int,
#     offset: int | None = None,
# ) -> datetime: ...
# def utc_now() -> datetime: ...
# def op_addition(x: datetime, y: timedelta) -> datetime: ...
# @overload
# def op_subtraction(x: datetime, y: datetime) -> int: ...  # Returns TimeSpan as ticks
# @overload
# def op_subtraction(x: datetime, y: int) -> datetime: ...  # TimeSpan as ticks
# def min_value() -> datetime: ...
