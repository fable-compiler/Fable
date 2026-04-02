from __future__ import annotations

from datetime import datetime
from typing import Any

from .core import FSharpRef, int32, int64
from .date import days_in_month
from .time_span import TimeSpan


def create(year: int, month: int, day: int) -> datetime:
    """Create a DateOnly value represented as a UTC datetime at midnight."""
    return datetime(year, month, day)


def max_value() -> datetime:
    return datetime(9999, 12, 31)


def min_value() -> datetime:
    return datetime(1, 1, 1)


def day_number(d: datetime) -> int:
    """Return the number of days since 0001-01-01."""
    ticks = _to_ticks(d)
    return int(ticks // 864000000000)


def from_day_number(day_num: int) -> datetime:
    """Create a DateOnly from a day number (days since 0001-01-01)."""
    ticks = day_num * 864000000000
    us = (ticks // 10) + _DOTNET_EPOCH_MICROSECONDS
    from datetime import UTC

    dt = datetime.fromtimestamp(us / 1_000_000, tz=UTC)
    return datetime(dt.year, dt.month, dt.day)


def from_date_time(d: datetime) -> datetime:
    """Create a DateOnly from a DateTime."""
    return datetime(d.year, d.month, d.day)


def year(d: datetime) -> int32:
    return int32(d.year)


def month(d: datetime) -> int32:
    return int32(d.month)


def day(d: datetime) -> int32:
    return int32(d.day)


def day_of_week(d: datetime) -> int:
    return (d.weekday() + 1) % 7


def day_of_year(d: datetime) -> int:
    return d.timetuple().tm_yday


def add_days(d: datetime, v: int) -> datetime:
    from datetime import timedelta

    new_dt = d + timedelta(days=int(v))
    return datetime(new_dt.year, new_dt.month, new_dt.day)


def add_months(d: datetime, v: int) -> datetime:
    from .date import add_months as date_add_months

    result = date_add_months(d, v)
    return datetime(result.year, result.month, result.day)


def add_years(d: datetime, v: int) -> datetime:
    from .date import add_years as date_add_years

    result = date_add_years(d, v)
    return datetime(result.year, result.month, result.day)


def to_date_time(d: datetime, time: TimeSpan, kind: int = 0) -> datetime:
    """Convert DateOnly + TimeOnly to DateTime.

    time is a TimeOnly value (ticks since midnight).
    kind: 0=Unspecified, 1=UTC, 2=Local
    """
    from datetime import UTC as _UTC

    from .singleton_local_time_zone import local_time_zone
    from .time_span import hours, microseconds, milliseconds, minutes, seconds

    h = int(hours(time))
    m = int(minutes(time))
    s = int(seconds(time))
    ms = int(milliseconds(time))
    mc = int(microseconds(time))

    if kind == 1:  # UTC
        return datetime(d.year, d.month, d.day, h, m, s, ms * 1000 + mc, tzinfo=_UTC)
    elif kind == 2:  # Local
        return datetime(d.year, d.month, d.day, h, m, s, ms * 1000 + mc, tzinfo=local_time_zone)
    else:
        return datetime(d.year, d.month, d.day, h, m, s, ms * 1000 + mc)


def to_string(d: datetime, format: str = "d", _provider: Any = None) -> str:
    y = str(d.year).zfill(4)
    m = str(d.month).zfill(2)
    dd = str(d.day).zfill(2)

    if format == "d":
        return f"{m}/{dd}/{y}"
    else:  # "o" or "O"
        return f"{y}-{m}-{dd}"


def parse(string: str) -> datetime:
    """Parse a string into a DateOnly value."""
    import re

    # yyyy-mm-dd or yyyy/mm/dd
    match = re.match(r"^\s*(\d{4})\s*[-/.,]\s*(\d{1,2})\s*[-/.,]\s*(\d{1,2})\s*$", string)
    if match:
        y, m, d = int(match.group(1)), int(match.group(2)), int(match.group(3))
        if 1 <= m <= 12 and 1 <= d <= days_in_month(y, m):
            return create(y, m, d)

    # mm/dd/yyyy
    match = re.match(r"^\s*(\d{1,2})\s*[-/.,]\s*(\d{1,2})\s*[-/.,]\s*(\d{4})\s*$", string)
    if match:
        m, d, y = int(match.group(1)), int(match.group(2)), int(match.group(3))
        if 1 <= m <= 12 and 1 <= d <= days_in_month(y, m):
            return create(y, m, d)

    # mm/dd (current year)
    match = re.match(r"^\s*(\d{1,2})\s*[-/.,]\s*(\d{1,2})\s*$", string)
    if match:
        m, d = int(match.group(1)), int(match.group(2))
        y = datetime.now().year
        if 1 <= m <= 12 and 1 <= d <= days_in_month(y, m):
            return create(y, m, d)

    raise Exception(f"String '{string}' was not recognized as a valid DateOnly.")


def try_parse(v: str, def_value: FSharpRef[datetime]) -> bool:
    try:
        def_value.contents = parse(v)
        return True
    except Exception:
        return False


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

_DOTNET_EPOCH_MICROSECONDS = -62135596800000000


def _to_ticks(d: datetime) -> int:
    """Convert a naive datetime to .NET ticks."""
    from datetime import UTC as _UTC

    utc_dt = datetime(d.year, d.month, d.day, tzinfo=_UTC)
    us = int(utc_dt.timestamp() * 1_000_000)
    return (us - _DOTNET_EPOCH_MICROSECONDS) * 10


__all__ = [
    "add_days",
    "add_months",
    "add_years",
    "create",
    "day",
    "day_number",
    "day_of_week",
    "day_of_year",
    "from_date_time",
    "from_day_number",
    "max_value",
    "min_value",
    "month",
    "parse",
    "to_date_time",
    "to_string",
    "try_parse",
    "year",
]
