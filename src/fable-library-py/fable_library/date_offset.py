from __future__ import annotations

from datetime import UTC, datetime, timedelta, timezone
from typing import Any, SupportsIndex

from . import time_span
from .time_span import TimeSpan
from .types import FSharpRef


class DateTimeOffset(datetime):
    """A datetime subclass with an explicit offset, similar to .NET DateTimeOffset"""

    def __new__(cls, dt: datetime, offset_milliseconds: int = 0):
        # Create new datetime instance using the values from the input datetime
        instance = super().__new__(
            cls, dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second, dt.microsecond, dt.tzinfo
        )
        return instance

    def __init__(self, dt: datetime, offset_milliseconds: int = 0):
        # Store the offset in milliseconds internally
        self.offset_ms = offset_milliseconds

    @property
    def offset(self) -> TimeSpan:
        """Return the offset as a TimeSpan object"""
        return time_span.from_milliseconds(self.offset_ms)

    def __str__(self):
        offset_hours = self.offset_ms // (60 * 60 * 1000)
        offset_minutes = abs(self.offset_ms % (60 * 60 * 1000)) // (60 * 1000)
        sign = "+" if self.offset_ms >= 0 else "-"
        return f"{self.strftime('%Y-%m-%dT%H:%M:%S.%f')}{sign}{abs(offset_hours):02d}:{offset_minutes:02d}"

    def __repr__(self):
        return f"DateTimeOffset({datetime.__repr__(self)}, offset={self.offset_ms})"

    def getTime(self):
        """Get time in milliseconds since epoch (like JavaScript Date.getTime())"""
        return int(self.timestamp() * 1000)

    def replace(
        self,
        year: SupportsIndex | None = None,
        month: SupportsIndex | None = None,
        day: SupportsIndex | None = None,
        hour: SupportsIndex | None = None,
        minute: SupportsIndex | None = None,
        second: SupportsIndex | None = None,
        microsecond: SupportsIndex | None = None,
        tzinfo: Any = True,
        *,
        fold: int | None = None,
        offset: int | None = None,
    ):
        """Override replace to maintain DateTimeOffset type and preserve offset"""
        # Use our custom offset parameter if provided, otherwise preserve existing offset
        new_offset = offset if offset is not None else self.offset_ms

        # Handle None values like the original datetime.replace implementation
        if year is None:
            year = self.year
        if month is None:
            month = self.month
        if day is None:
            day = self.day
        if hour is None:
            hour = self.hour
        if minute is None:
            minute = self.minute
        if second is None:
            second = self.second
        if microsecond is None:
            microsecond = self.microsecond
        if tzinfo is True:  # True is the sentinel for "use current tzinfo"
            tzinfo = self.tzinfo
        if fold is None:
            fold = self.fold

        # Call parent replace with all parameters explicitly
        new_dt = super().replace(
            year=year,
            month=month,
            day=day,
            hour=hour,
            minute=minute,
            second=second,
            microsecond=microsecond,
            tzinfo=tzinfo,
            fold=fold,
        )
        # Return new DateTimeOffset with preserved or updated offset
        return DateTimeOffset(new_dt, new_offset)


def timedelta_total_microseconds(td: timedelta) -> int:
    # timedelta doesn't expose total_microseconds
    # so we need to calculate it ourselves
    return td.days * (24 * 3600) + td.seconds * 10**6 + td.microseconds


def parse(string: str, detectUTC: bool = False) -> DateTimeOffset:
    from dateutil import parser

    parsed_dt = parser.parse(string)

    # Calculate offset in milliseconds
    if parsed_dt.tzinfo is not None:
        offset_td = parsed_dt.utcoffset()
        if offset_td is not None:
            offset_ms = int(offset_td.total_seconds() * 1000)
        else:
            offset_ms = 0
    else:
        # If no timezone info, assume local timezone
        local_dt = parsed_dt.replace(tzinfo=datetime.now().astimezone().tzinfo)
        offset_td = local_dt.utcoffset()
        offset_ms = int(offset_td.total_seconds() * 1000) if offset_td else 0
        parsed_dt = local_dt

    return DateTimeOffset(parsed_dt, offset_ms)


def try_parse(string: str, def_value: FSharpRef[DateTimeOffset]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


def create(
    year: int,
    month: int,
    day: int,
    h: int,
    m: int,
    s: int,
    ms: int | TimeSpan,
    offset: TimeSpan | None = None,
) -> DateTimeOffset:
    python_offset: timedelta | None = None
    offset_ms = 0

    if isinstance(ms, TimeSpan):
        python_offset = timedelta(microseconds=float(time_span.total_microseconds(ms)))
        offset_ms = int(time_span.total_microseconds(ms) / 1000)
        ms = 0

    if python_offset is None:
        dt = datetime(year, month, day, h, m, s, ms)
        if offset is not None:
            offset_ms = int(time_span.total_microseconds(offset) / 1000)
            python_offset = timedelta(microseconds=float(time_span.total_microseconds(offset)))
            dt = dt.replace(tzinfo=timezone(python_offset))
        else:
            # Default to local timezone
            dt = dt.replace(tzinfo=datetime.now().astimezone().tzinfo)
            offset_td = dt.utcoffset()
            offset_ms = int(offset_td.total_seconds() * 1000) if offset_td else 0
    else:
        tzinfo = timezone(python_offset)
        dt = datetime(year, month, day, h, m, s, ms, tzinfo=tzinfo)

    return DateTimeOffset(dt, offset_ms)


def now() -> DateTimeOffset:
    dt = datetime.now().astimezone()
    offset_td = dt.utcoffset()
    offset_ms = int(offset_td.total_seconds() * 1000) if offset_td else 0
    return DateTimeOffset(dt, offset_ms)


def utc_now() -> DateTimeOffset:
    dt = datetime.now(UTC)
    return DateTimeOffset(dt, 0)


def min_value() -> DateTimeOffset:
    dt = datetime.min.replace(tzinfo=UTC)
    return DateTimeOffset(dt, 0)


def year(d: DateTimeOffset) -> int:
    return d.year


def month(d: DateTimeOffset) -> int:
    return d.month


def day(d: DateTimeOffset) -> int:
    return d.day


def hour(d: DateTimeOffset) -> int:
    return d.hour


def minute(d: DateTimeOffset) -> int:
    return d.minute


def second(d: DateTimeOffset) -> int:
    return d.second


def op_subtraction(x: DateTimeOffset, y: DateTimeOffset | TimeSpan) -> DateTimeOffset | TimeSpan:
    if isinstance(y, TimeSpan):
        # Subtract TimeSpan from DateTimeOffset
        new_dt = x - timedelta(microseconds=float(time_span.total_microseconds(y)))
        # Create new DateTimeOffset preserving the offset
        return DateTimeOffset(new_dt, x.offset_ms)

    # When subtracting two DateTimeOffset objects, return TimeSpan
    time_diff = datetime.__sub__(x, y)  # This will be a timedelta
    return time_span.from_microseconds(timedelta_total_microseconds(time_diff))


__all__ = [
    "DateTimeOffset",
    "create",
    "day",
    "hour",
    "min_value",
    "minute",
    "month",
    "now",
    "op_subtraction",
    "parse",
    "second",
    "try_parse",
    "utc_now",
    "year",
]
