"""TimeOnly implementation for Fable Python.

TimeOnly is represented as ticks since midnight (same unit as TimeSpan).
This is consistent with the Python TimeSpan representation which stores ticks.
"""

from __future__ import annotations

from typing import Any, SupportsFloat

from .core import FSharpRef, int32
from .time_span import (
    TimeSpan,
    hours,
    microseconds,
    milliseconds,
    minutes,
    seconds,
    ticks as _ts_ticks,
)
from .util import pad_with_zeros

_TICKS_PER_MILLISECOND = 10000
_TICKS_PER_SECOND = 10000000
_TICKS_PER_MINUTE = 600000000
_TICKS_PER_HOUR = 36000000000
_TICKS_PER_DAY = 864000000000


def create(h: int = 0, m: int = 0, s: int = 0, ms: int = 0) -> TimeSpan:
    """Create a TimeOnly value from hours, minutes, seconds, milliseconds.

    Returns a TimeSpan (ticks since midnight).
    """
    if h < 0 or m < 0 or s < 0 or ms < 0:
        raise Exception("The parameters describe an unrepresentable TimeOnly.")

    return TimeSpan(
        int(h) * _TICKS_PER_HOUR
        + int(m) * _TICKS_PER_MINUTE
        + int(s) * _TICKS_PER_SECOND
        + int(ms) * _TICKS_PER_MILLISECOND
    )


def from_ticks(t: int) -> TimeSpan:
    """Create a TimeOnly from ticks."""
    return TimeSpan(int(t))


def from_time_span(ts: TimeSpan) -> TimeSpan:
    """Create a TimeOnly from a TimeSpan."""
    t = int(ts)
    if t < 0 or t >= _TICKS_PER_DAY:
        raise Exception("The TimeSpan describes an unrepresentable TimeOnly.")
    return TimeSpan(t)


def from_date_time(d: Any) -> TimeSpan:
    """Extract TimeOnly from a DateTime."""
    return create(d.hour, d.minute, d.second, d.microsecond // 1000)


def max_value() -> TimeSpan:
    """23:59:59.9999999"""
    return TimeSpan(_TICKS_PER_DAY - 1)


def min_value() -> TimeSpan:
    return TimeSpan(0)


def hour(t: TimeSpan) -> int32:
    return int32(hours(t))


def minute(t: TimeSpan) -> int32:
    return int32(minutes(t))


def second(t: TimeSpan) -> int32:
    return int32(seconds(t))


def millisecond(t: TimeSpan) -> int32:
    return int32(milliseconds(t))


def ticks(t: TimeSpan) -> int:
    return int(_ts_ticks(t))


def add(t: TimeSpan, ts: TimeSpan, wrapped_days: FSharpRef[int] | None = None) -> TimeSpan:
    """Add a TimeSpan to a TimeOnly, wrapping around midnight."""
    t_val = int(t)
    ts_val = int(ts)

    if wrapped_days is None:
        t2 = (t_val + ts_val) % _TICKS_PER_DAY
        return TimeSpan(t2 if t2 >= 0 else _TICKS_PER_DAY + t2)

    wrapped_days.contents = ts_val // _TICKS_PER_DAY
    new_ticks = t_val + ts_val % _TICKS_PER_DAY

    if new_ticks < 0:
        wrapped_days.contents -= 1
        new_ticks += _TICKS_PER_DAY
    elif new_ticks >= _TICKS_PER_DAY:
        wrapped_days.contents += 1
        new_ticks -= _TICKS_PER_DAY

    return TimeSpan(new_ticks)


def add_hours(t: TimeSpan, h: SupportsFloat) -> TimeSpan:
    return add(t, TimeSpan(int(float(h) * _TICKS_PER_HOUR)))


def add_minutes(t: TimeSpan, m: SupportsFloat) -> TimeSpan:
    return add(t, TimeSpan(int(float(m) * _TICKS_PER_MINUTE)))


def is_between(t: TimeSpan, start: TimeSpan, end: TimeSpan) -> bool:
    t_val, s_val, e_val = int(t), int(start), int(end)
    if s_val <= e_val:
        return s_val <= t_val and e_val > t_val
    else:
        return s_val <= t_val or e_val > t_val


def to_string(t: TimeSpan, format: str = "t", _provider: Any = None) -> str:
    if format not in ("r", "R", "o", "O", "t", "T"):
        raise Exception("Custom formats are not supported")

    h = pad_with_zeros(int(hours(t)), 2)
    m = pad_with_zeros(int(minutes(t)), 2)
    base = f"{h}:{m}"

    if format == "t":
        return base

    s = pad_with_zeros(int(seconds(t)), 2)
    if format in ("o", "O"):
        ms = pad_with_zeros(int(milliseconds(t)), 3)
        return f"{base}:{s}.{ms}0000"
    else:
        return f"{base}:{s}"


def parse(string: str) -> TimeSpan:
    """Parse a time string into a TimeOnly value."""
    import re

    match = re.match(
        r"^\s*([0-1]?\d|2[0-3])\s*:\s*([0-5]?\d)(\s*:\s*([0-5]?\d)(\.(\d+))?)?\s*$",
        string,
    )
    if match and match.group(1) is not None and match.group(2) is not None:
        h = int(match.group(1))
        m = int(match.group(2))
        s = 0
        ms = 0

        if match.group(4) is not None:
            s = int(match.group(4))

        if match.group(6) is not None:
            frac = match.group(6)
            length = len(frac)
            val = int(frac)
            if length == 1:
                ms = val * 100
            elif length == 2:
                ms = val * 10
            elif length == 3:
                ms = val
            elif length == 4:
                ms = val // 10
            elif length == 5:
                ms = val // 100
            elif length == 6:
                ms = val // 1000
            else:
                ms = int(frac[:7]) // 10000

        return create(h, m, s, ms)

    raise Exception(f"String '{string}' was not recognized as a valid TimeOnly.")


def try_parse(v: str, def_value: FSharpRef[TimeSpan]) -> bool:
    try:
        def_value.contents = parse(v)
        return True
    except Exception:
        return False


def op_subtraction(left: TimeSpan, right: TimeSpan) -> TimeSpan:
    return add(left, TimeSpan(-int(right)))


__all__ = [
    "add",
    "add_hours",
    "add_minutes",
    "create",
    "from_date_time",
    "from_ticks",
    "from_time_span",
    "is_between",
    "max_value",
    "min_value",
    "op_subtraction",
    "parse",
    "to_string",
    "try_parse",
]
