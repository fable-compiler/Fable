from __future__ import annotations

import re
from math import ceil, floor, fmod
from typing import Any

from .types import FloatTypes, FSharpRef, IntegerTypes, float64, int32
from .util import pad_left_and_right_with_zeros, pad_with_zeros


# TimeSpan is represented as an int which is the Tick value
# We can recompute everything from this value
class TimeSpan(int):
    pass


def create(
    days: float64 = float64(0),
    hours: float64 | None = None,
    minutes: float64 | None = None,
    seconds: float64 | None = None,
    milliseconds: float64 | None = None,
    microseconds: float64 | None = None,
) -> TimeSpan:
    match (days, hours, minutes, seconds, milliseconds, microseconds):
        # ticks constructor
        case (_, None, None, None, None, None):
            return TimeSpan(days)
        # hours, minutes, seconds constructor
        case (_, _, _, None, None, None):
            seconds = minutes
            minutes = hours
            hours = days
            days = float64(0)
        # others constructor follows the correct order of arguments
        case _:
            pass

    return TimeSpan(
        float(days) * 864000000000
        + float(hours or 0) * 36000000000
        + float(minutes or 0) * 600000000
        + float(seconds or 0) * 10000000
        + float(milliseconds or 0) * 10000
        + float(microseconds or 0) * 10
    )


def total_nanoseconds(ts: TimeSpan) -> float64:
    # We store timespan as the Tick value so nanoseconds step is 100
    return float64(ts) * 100


def total_microseconds(ts: TimeSpan) -> float64:
    return float64(ts) / 10


def total_milliseconds(ts: TimeSpan) -> float64:
    return float64(ts) / 10000


def total_seconds(ts: TimeSpan) -> float64:
    return float64(ts) / 10000000


def total_minutes(ts: TimeSpan) -> float64:
    return float64(ts) / 600000000


def total_hours(ts: TimeSpan) -> float64:
    return float64(ts) / 36000000000


def total_days(ts: TimeSpan) -> float64:
    return float64(ts) / 864000000000


def from_microseconds(micros: IntegerTypes | FloatTypes) -> TimeSpan:
    return create(float64(0), float64(0), float64(0), float64(0), float64(0), float64(micros))


def from_milliseconds(msecs: IntegerTypes | FloatTypes, mc: int32 | None = None) -> TimeSpan:
    return create(
        float64(0),
        float64(0),
        float64(0),
        float64(0),
        float64(msecs),
        float64(mc) if mc is not None else float64(0),
    )


def from_ticks(ticks: int32) -> TimeSpan:
    return create(float64(ticks))


def from_seconds(s: IntegerTypes | FloatTypes, ms: int32 | None = None, mc: int32 | None = None) -> TimeSpan:
    return create(
        float64(0),
        float64(0),
        float64(0),
        float64(s),
        float64(ms) if ms is not None else float64(0),
        float64(mc) if mc is not None else float64(0),
    )


def from_minutes(
    m: IntegerTypes | FloatTypes, s: int32 | None = None, ms: int32 | None = None, mc: int32 | None = None
) -> TimeSpan:
    return create(
        float64(0),
        float64(0),
        float64(m),
        float64(s) if s is not None else float64(0),
        float64(ms) if ms is not None else float64(0),
        float64(mc) if mc is not None else float64(0),
    )


def from_hours(
    h: IntegerTypes | FloatTypes,
    m: int32 | None = None,
    s: int32 | None = None,
    ms: int32 | None = None,
    mc: int32 | None = None,
) -> TimeSpan:
    return create(
        float64(0),
        float64(h),
        float64(m) if m is not None else float64(0),
        float64(s) if s is not None else float64(0),
        float64(ms) if ms is not None else float64(0),
        float64(mc) if mc is not None else float64(0),
    )


def from_days(
    d: IntegerTypes | FloatTypes,
    h: int32 | None = None,
    m: int32 | None = None,
    s: int32 | None = None,
    ms: int32 | None = None,
    mc: int32 | None = None,
) -> TimeSpan:
    return create(
        float64(d),
        float64(h) if h is not None else float64(0),
        float64(m) if m is not None else float64(0),
        float64(s) if s is not None else float64(0),
        float64(ms) if ms is not None else float64(0),
        float64(mc) if mc is not None else float64(0),
    )


def ticks(ts: TimeSpan) -> int:
    return int(ts)


def microseconds(ts: TimeSpan) -> int:
    return int(fmod(ts, 10000) / 10)


def milliseconds(ts: TimeSpan) -> int:
    return int(fmod(ts, 10000000) / 10000)


def seconds(ts: TimeSpan) -> int:
    return int(fmod(ts, 600000000) / 10000000)


def minutes(ts: TimeSpan) -> int:
    return int(fmod(ts, 36000000000) / 600000000)


def hours(ts: TimeSpan) -> int:
    return int(fmod(ts, 864000000000) / 36000000000)


def days(ts: TimeSpan) -> int:
    res = ts / 864000000000
    return ceil(res) if res < 0 else floor(res)


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
    h = abs(hours(ts))
    m = abs(minutes(ts))
    s = abs(seconds(ts))
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


_time_span_parse_regex = re.compile(
    r"^(-?)((\d+)\.)?(?:0*)([0-9]|0[0-9]|1[0-9]|2[0-3]):(?:0*)([0-5][0-9]|[0-9])(:(?:0*)([0-5][0-9]|[0-9]))?\.?(\d+)?$"
)


# Second argument is to not crash when using provides CultureInfo.InvariantCulture
def parse(string: str, _: Any | None = None) -> TimeSpan:
    first_dot = string.find(".")
    first_colon = string.find(":")
    if first_dot == -1 and first_colon == -1:
        # There is only a day ex: 4
        # parse as int
        try:
            d = int(string)
        except Exception:
            raise Exception(f"String '{string}' was not recognized as a valid TimeSpan.")
        return from_days(d)
    if first_colon > 0:  # process time part
        r = _time_span_parse_regex.match(string)
        if r is not None and r.group(4) is not None and r.group(5) is not None:
            d = 0
            ms = 0
            s = 0
            sign = -1 if r.group(1) == "-" else 1
            h = int(r.group(4))
            m = int(r.group(5))
            if r.group(3) is not None:
                d = int(r.group(3))
            if r.group(7) is not None:
                s = int(r.group(7))
            if r.group(8) is not None:
                # Depending on the number of decimals passed, we need to adapt the numbers
                g_8: str = r.group(8)
                match len(g_8):
                    case 1:
                        ms = int(g_8) * 100
                    case 2:
                        ms = int(g_8) * 10
                    case 3:
                        ms = int(g_8)
                    case 4:
                        ms = int(g_8) / 10
                    case 5:
                        ms = int(g_8) / 100
                    case 6:
                        ms = int(g_8) / 1000
                    case 7:
                        ms = int(g_8) / 10000
                    case _:
                        raise Exception(f"String '{string}' was not recognized as a valid TimeSpan.")
            return multiply(create(float64(d), float64(h), float64(m), float64(s), float64(ms)), sign)
    raise Exception(f"String '{string}' was not recognized as a valid TimeSpan.")


def try_parse(
    string: str, def_value_or_format_provider: FSharpRef[TimeSpan] | Any, def_value: FSharpRef[TimeSpan] | None = None
) -> bool:
    # Find where the out_value is
    out_value: FSharpRef[TimeSpan] = def_value_or_format_provider

    # If we have 3 arguments, it means that the second argument is the format provider
    if def_value is not None:
        out_value = def_value

    try:
        out_value.contents = parse(string)
    except Exception:
        return False

    return True


def to_milliseconds(value: int | TimeSpan) -> float:
    """Convert either an int (milliseconds) or TimeSpan to milliseconds as float."""
    if isinstance(value, TimeSpan):
        return float(total_milliseconds(value))
    else:
        return float(value)


__all__ = [
    "add",
    "create",
    "divide",
    "duration",
    "from_hours",
    "from_microseconds",
    "from_milliseconds",
    "from_minutes",
    "from_seconds",
    "from_ticks",
    "multiply",
    "negate",
    "parse",
    "subtract",
    "to_milliseconds",
    "total_days",
    "total_days",
    "total_hours",
    "total_hours",
    "total_microseconds",
    "total_milliseconds",
    "total_minutes",
    "total_minutes",
    "total_seconds",
    "total_seconds",
    "try_parse",
]
