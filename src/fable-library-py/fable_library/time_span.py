from __future__ import annotations

from datetime import timedelta
from typing import Any

from .util import pad_left_and_right_with_zeros, pad_with_zeros


# TimeSpan is represented as an int which is the Tick value
# We can recompute everything from this value
class TimeSpan(int):
    pass


def create(
    days: float = 0,
    hours: float | None = None,
    minutes: float | None = None,
    seconds: float | None = None,
    milliseconds: float | None = None,
    microseconds: float | None = None,
) -> TimeSpan:
    match (days, hours, minutes, seconds, milliseconds, microseconds):
        # ticks constructor
        case (_, None, None, None, None, None):
            return TimeSpan(days)
        # hours, minutes, seconds constructor
        case (_, _, _, None, None, None):
            print("hours, minutes, seconds constructor")
            seconds = minutes
            minutes = hours
            hours = days
            days = 0
        # others constructor follows the correct order of arguments
        case _:
            pass

    return TimeSpan(
        days * 864000000000
        + (hours or 0) * 36000000000
        + (minutes or 0) * 600000000
        + (seconds or 0) * 10000000
        + (milliseconds or 0) * 10000
        + (microseconds or 0) * 10
    )


def total_nanoseconds(ts: TimeSpan) -> float:
    # We store timespan as the Tick value so nanoseconds step is 100
    return ts * 100


def total_microseconds(ts: TimeSpan) -> float:
    return ts / 10


def total_milliseconds(ts: TimeSpan) -> float:
    return ts / 10000


def total_seconds(ts: TimeSpan) -> float:
    return ts / 10000000


def total_minutes(ts: TimeSpan) -> float:
    return ts / 600000000


def total_hours(ts: TimeSpan) -> float:
    return ts / 36000000000


def total_days(ts: TimeSpan) -> float:
    return ts / 864000000000

def from_microseconds(micros: float) -> TimeSpan:
    return create(0,0,0,0,0,micros)

def from_milliseconds(msecs: int) -> TimeSpan:
    return create(0,0,0,0,msecs)


def from_ticks(ticks: int) -> TimeSpan:
    return create(ticks)


def from_seconds(s: int) -> TimeSpan:
    return create(0, 0, s)


def from_minutes(m: int) -> TimeSpan:
    return create(0, m, 0)


def from_hours(h: int) -> TimeSpan:
    return create(h, 0, 0)


def from_days(d: int) -> TimeSpan:
    return create(d, 0, 0, 0)


def ticks(ts: TimeSpan) -> int:
    return int(ts)


def microseconds(ts: TimeSpan) -> int:
    return int(ts % 10000 / 10)


def milliseconds(ts: TimeSpan) -> int:
    return int(ts % 10000000 / 10000)


def seconds(ts: TimeSpan) -> int:
    return int(ts % 600000000 / 10000000)


def minutes(ts: TimeSpan) -> int:
    return int(ts % 36000000000 / 600000000)


def hours(ts: TimeSpan) -> int:
    return int(ts % 864000000000 / 36000000000)


def days(ts: TimeSpan) -> int:
    return int(ts / 864000000000)


def negate(ts: TimeSpan) -> TimeSpan:
    return TimeSpan(-ts)


def duration(ts: TimeSpan) -> TimeSpan:
    return TimeSpan(abs(int(ts)))


def add(ts: TimeSpan, other: TimeSpan) -> TimeSpan:
    return TimeSpan(ts + other)


def subtract(ts: TimeSpan, other: TimeSpan) -> TimeSpan:
    return TimeSpan(ts - other)


def multiply(ts: TimeSpan, factor: float) -> TimeSpan:
    # We represents TimeSpan as a Tick which can't be a float
    # This also allows us
    return TimeSpan(int(ts * factor))


def divide(ts: TimeSpan, divisor: float) -> TimeSpan:
    return TimeSpan(int(ts / divisor))


def to_string(ts: TimeSpan, format: str = "c", _provider: Any | None = None) -> str:
    if format not in ["c", "g", "G"]:
        raise ValueError("Custom formats are not supported")

    d = abs(days(ts))
    h = hours(ts)
    m = minutes(ts)
    s = seconds(ts)
    ms = abs(milliseconds(ts))
    sign: str = "-" if ts < 0 else ""

    ms_str = (
        ""
        if ms == 0 and format != "G"
        else "." + pad_left_and_right_with_zeros(ms, 3, 7)
        if format != "g"
        else "." + pad_with_zeros(ms, 3)
    )
    day_str = "" if d == 0 and format != "G" else f"{d}." if format == "c" else f"{d}:"
    hour_str = pad_with_zeros(h, 2) if format != "g" else str(h)

    return f"{sign}{day_str}{hour_str}:{pad_with_zeros(m, 2)}:{pad_with_zeros(s, 2)}{ms_str}"


__all__ = [
    "create",
    "total_microseconds",
    "total_milliseconds",
    "total_seconds",
    "total_minutes",
    "total_hours",
    "total_days",
    "from_ticks",
    "from_microseconds",
    "from_milliseconds",
    "from_hours",
    "from_minutes",
    "from_seconds",
    "negate",
    "duration",
    "total_seconds",
    "total_days",
    "total_minutes",
    "total_hours",
    "add",
    "subtract",
    "divide",
    "multiply",
]
