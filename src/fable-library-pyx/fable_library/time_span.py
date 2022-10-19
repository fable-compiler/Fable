from datetime import timedelta
from typing import Any, Optional

from .util import pad_left_and_right_with_zeros, pad_with_zeros


def total_seconds(ts: timedelta) -> float:
    return ts.total_seconds()


def total_days(ts: timedelta) -> float:
    return total_seconds(ts) / 86400


def total_minutes(ts: timedelta) -> float:
    return total_seconds(ts) / 60


def total_hours(ts: timedelta) -> float:
    return total_seconds(ts) / 3600


def from_milliseconds(msecs: int) -> timedelta:
    return timedelta(milliseconds=msecs)


def from_ticks(ticks: int) -> timedelta:
    return timedelta(microseconds=ticks // 10)


def from_seconds(s: int) -> timedelta:
    return timedelta(seconds=s)


def from_minutes(m: int) -> timedelta:
    return timedelta(minutes=m)


def from_hours(h: int) -> timedelta:
    return timedelta(hours=h)


def from_days(d: int) -> timedelta:
    return timedelta(days=d)


def to_milliseconds(td: timedelta) -> int:
    return int(td.total_seconds() * 1000)


def days(ts: timedelta) -> int:
    return int(total_seconds(ts) / 86400)


def hours(ts: timedelta) -> int:
    return int(abs(total_seconds(ts)) % 86400 / 3600)


def minutes(ts: timedelta) -> int:
    return int(abs(total_seconds(ts)) % 3600 / 60)


def seconds(ts: timedelta) -> int:
    return int(abs(total_seconds(ts)) % 60)


def milliseconds(ts: timedelta) -> int:
    return int(total_seconds(ts) % 1 * 1000)


def ticks(ts: timedelta) -> int:
    return int(total_seconds(ts) * 10000000)


def negate(ts: timedelta) -> timedelta:
    return -ts


def duration(ts: timedelta) -> timedelta:
    if ts < timedelta(0):
        return -ts

    return ts


def add(ts: timedelta, other: timedelta) -> timedelta:
    return ts + other


def subtract(ts: timedelta, other: timedelta) -> timedelta:
    return ts - other


def multiply(ts: timedelta, factor: int) -> timedelta:
    return ts * factor


def divide(ts: timedelta, divisor: int) -> timedelta:
    return ts / divisor


def create(
    d: int = 0,
    h: Optional[int] = None,
    m: Optional[int] = None,
    s: Optional[int] = None,
    ms: Optional[int] = None,
) -> timedelta:
    if h is None and m is None and s is None and ms is None:
        return from_ticks(d)

    elif s is None and ms is None:
        return timedelta(hours=d or 0, minutes=h or 0, seconds=m or 0)

    return timedelta(
        days=d, hours=h or 0, minutes=m or 0, seconds=s or 0, milliseconds=ms or 0
    )


def to_string(ts: timedelta, format: str = "c", _provider: Optional[Any] = None) -> str:
    if format not in ["c", "g", "G"]:
        raise ValueError("Custom formats are not supported")

    d = abs(days(ts))
    h = hours(ts)
    m = minutes(ts)
    s = seconds(ts)
    ms = abs(milliseconds(ts))
    sign: str = "-" if ts < timedelta(0) else ""

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
    "to_milliseconds",
    "from_ticks",
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
