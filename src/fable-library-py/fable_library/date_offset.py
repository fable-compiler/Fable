from __future__ import annotations

from datetime import UTC, datetime, timedelta, timezone
from math import fmod
from typing import Any, SupportsFloat, SupportsIndex, SupportsInt, overload

from . import time_span
from .core import FSharpRef, int32, int64
from .time_span import TimeSpan


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

    def __eq__(self, other: object) -> bool:
        """DateTimeOffset equality compares by UTC-normalized instant."""
        if not isinstance(other, DateTimeOffset):
            return NotImplemented
        return _to_utc_ms(self) == _to_utc_ms(other)

    def __ne__(self, other: object) -> bool:
        if not isinstance(other, DateTimeOffset):
            return NotImplemented
        return _to_utc_ms(self) != _to_utc_ms(other)

    def __lt__(self, other: datetime) -> bool:
        if not isinstance(other, DateTimeOffset):
            return NotImplemented
        return _to_utc_ms(self) < _to_utc_ms(other)

    def __le__(self, other: datetime) -> bool:
        if not isinstance(other, DateTimeOffset):
            return NotImplemented
        return _to_utc_ms(self) <= _to_utc_ms(other)

    def __gt__(self, other: datetime) -> bool:
        if not isinstance(other, DateTimeOffset):
            return NotImplemented
        return _to_utc_ms(self) > _to_utc_ms(other)

    def __ge__(self, other: datetime) -> bool:
        if not isinstance(other, DateTimeOffset):
            return NotImplemented
        return _to_utc_ms(self) >= _to_utc_ms(other)

    def __hash__(self) -> int:
        return hash(_to_utc_ms(self))

    @overload
    def __sub__(self, other: datetime) -> timedelta: ...
    @overload
    def __sub__(self, other: timedelta) -> DateTimeOffset: ...

    def __sub__(self, other: timedelta | datetime) -> DateTimeOffset | timedelta:
        if isinstance(other, timedelta) and not isinstance(other, datetime):
            # datetime - timedelta → new DateTimeOffset preserving offset
            plain = datetime(
                self.year, self.month, self.day, self.hour, self.minute, self.second, self.microsecond, self.tzinfo
            )
            new_dt = plain - other
            return DateTimeOffset(new_dt, self.offset_ms)
        # datetime - datetime → timedelta
        plain_self = datetime(
            self.year, self.month, self.day, self.hour, self.minute, self.second, self.microsecond, self.tzinfo
        )
        plain_other = datetime(
            other.year, other.month, other.day, other.hour, other.minute, other.second, other.microsecond, other.tzinfo
        )
        return plain_self - plain_other

    def __add__(self, other: timedelta) -> DateTimeOffset:
        plain = datetime(
            self.year, self.month, self.day, self.hour, self.minute, self.second, self.microsecond, self.tzinfo
        )
        new_dt = plain + other
        return DateTimeOffset(new_dt, self.offset_ms)

    def __radd__(self, other: timedelta) -> DateTimeOffset:
        return self.__add__(other)

    def getTime(self) -> int:
        """Get UTC time in milliseconds since epoch."""
        return _to_utc_ms(self)

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


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# .NET epoch: 0001-01-01T00:00:00Z in microseconds from Unix epoch
_DOTNET_EPOCH_MICROSECONDS = -62135596800000000


def _to_utc_ms(d: DateTimeOffset) -> int:
    """Return UTC instant in milliseconds since Unix epoch."""
    # The datetime stores local time; subtract the offset to get UTC
    local_ms = int(datetime.timestamp(d) * 1000)
    return local_ms


def _ticks_per_millisecond() -> int:
    return 10000


# ---------------------------------------------------------------------------
# Constructors
# ---------------------------------------------------------------------------


def timedelta_total_microseconds(td: timedelta) -> int:
    # timedelta doesn't expose total_microseconds
    # so we need to calculate it ourselves
    return td.days * (24 * 3600) + td.seconds * 10**6 + td.microseconds


def parse(string: str, detectUTC: bool = False) -> DateTimeOffset:
    from dateutil import parser  # Imported here to avoid top-level dependency if not used

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
        dt = datetime(year, month, day, h, m, s, ms * 1000)
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


def from_date(dt: datetime, offset_ts: TimeSpan | None = None) -> DateTimeOffset:
    """Construct DateTimeOffset from a DateTime."""
    if offset_ts is not None:
        offset_ms = int(time_span.total_microseconds(offset_ts) / 1000)
    elif dt.tzinfo is not None and dt.utcoffset() is not None:
        offset_td = dt.utcoffset()
        assert offset_td is not None
        offset_ms = int(offset_td.total_seconds() * 1000)
    else:
        # Unspecified kind — use local offset
        local_dt = dt.astimezone()
        offset_td = local_dt.utcoffset()
        offset_ms = int(offset_td.total_seconds() * 1000) if offset_td else 0
        dt = local_dt

    python_offset = timedelta(milliseconds=offset_ms)
    dt = dt.replace(tzinfo=timezone(python_offset))
    return DateTimeOffset(dt, offset_ms)


def from_ticks(ticks: SupportsInt, offset_ts: TimeSpan) -> DateTimeOffset:
    """Construct DateTimeOffset from ticks and offset."""
    offset_ms = int(time_span.total_microseconds(offset_ts) / 1000)
    # Ticks are 100-nanosecond intervals since 0001-01-01
    # Convert to microseconds from Unix epoch
    us = (int(ticks) // 10) + _DOTNET_EPOCH_MICROSECONDS
    python_offset = timedelta(milliseconds=offset_ms)
    dt = datetime.fromtimestamp(us / 1_000_000, tz=UTC)
    # Shift from UTC to the specified offset
    dt = dt + python_offset
    dt = dt.replace(tzinfo=timezone(python_offset))
    return DateTimeOffset(dt, offset_ms)


def from_date_time(date_only: datetime, time_only: TimeSpan, offset_ts: TimeSpan) -> DateTimeOffset:
    """Construct DateTimeOffset from DateOnly, TimeOnly, and offset TimeSpan."""
    from .time_span import hours, microseconds, milliseconds, minutes, seconds

    h = int(hours(time_only))
    m = int(minutes(time_only))
    s = int(seconds(time_only))
    ms = int(milliseconds(time_only))
    mc = int(microseconds(time_only))

    offset_ms = int(time_span.total_microseconds(offset_ts) / 1000)
    python_offset = timedelta(milliseconds=offset_ms)
    dt = datetime(
        date_only.year, date_only.month, date_only.day, h, m, s, ms * 1000 + mc, tzinfo=timezone(python_offset)
    )
    return DateTimeOffset(dt, offset_ms)


# ---------------------------------------------------------------------------
# Static fields
# ---------------------------------------------------------------------------


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


def max_value() -> DateTimeOffset:
    dt = datetime.max.replace(tzinfo=UTC)
    return DateTimeOffset(dt, 0)


def unix_epoch() -> DateTimeOffset:
    dt = datetime(1970, 1, 1, tzinfo=UTC)
    return DateTimeOffset(dt, 0)


# ---------------------------------------------------------------------------
# Properties (accessors)
# ---------------------------------------------------------------------------


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


def millisecond(d: DateTimeOffset) -> int:
    return d.microsecond // 1000


def microsecond(d: DateTimeOffset) -> int:
    return d.microsecond % 1000


def day_of_week(d: DateTimeOffset) -> int:
    return (d.weekday() + 1) % 7


def day_of_year(d: DateTimeOffset) -> int:
    return d.timetuple().tm_yday


def total_offset_minutes(d: DateTimeOffset) -> int:
    return d.offset_ms // (60 * 1000)


def date(d: DateTimeOffset) -> datetime:
    """Return the date component (time set to midnight) as a DateTime."""
    from . import date as date_mod

    return date_mod.create(d.year, d.month, d.day)


def time_of_day(d: DateTimeOffset) -> TimeSpan:
    """Return the time of day as a TimeSpan."""
    return time_span.create(
        0.0, float(d.hour), float(d.minute), float(d.second), float(d.microsecond // 1000), float(d.microsecond % 1000)
    )


def ticks(d: DateTimeOffset) -> int64:
    """Return ticks of the local date/time (not UTC-adjusted).

    .NET: DateTimeOffset.Ticks returns the local date/time component ticks,
    independent of the offset.
    """
    # Compute ticks from the local date/time fields
    from . import date as date_mod

    local_dt = datetime(d.year, d.month, d.day, d.hour, d.minute, d.second, d.microsecond, tzinfo=UTC)
    us = int(local_dt.timestamp() * 1_000_000)
    return date_mod.unix_epoch_microseconds_to_ticks(int64(us), int64(0))


def get_utc_ticks(d: DateTimeOffset) -> int64:
    """Return UTC ticks."""
    from . import date as date_mod

    utc_ms = _to_utc_ms(d)
    us = utc_ms * 1000
    return date_mod.unix_epoch_microseconds_to_ticks(int64(us), int64(0))


# ---------------------------------------------------------------------------
# Conversion
# ---------------------------------------------------------------------------


def utc_date_time(d: DateTimeOffset) -> datetime:
    """Return UtcDateTime (DateTime with UTC kind).

    Fable Python convention: UTC DateTimes are naive (no tzinfo).
    """
    utc_ms = _to_utc_ms(d)
    return datetime.fromtimestamp(utc_ms / 1000, tz=UTC).replace(tzinfo=None)


def local_date_time(d: DateTimeOffset) -> datetime:
    """Return LocalDateTime (DateTime with Local kind).

    Returns naive datetime in the local time zone to match
    Fable Python's DateTime convention where comparisons must
    work across different DateTimeKind values.
    """
    utc_ms = _to_utc_ms(d)
    utc_dt = datetime.fromtimestamp(utc_ms / 1000, tz=UTC)
    local_dt = utc_dt.astimezone()
    # Strip tzinfo so the datetime is naive (comparable with UTC datetimes)
    return datetime(
        local_dt.year,
        local_dt.month,
        local_dt.day,
        local_dt.hour,
        local_dt.minute,
        local_dt.second,
        local_dt.microsecond,
    )


def to_universal_time(d: DateTimeOffset) -> DateTimeOffset:
    """Convert to UTC DateTimeOffset."""
    utc_ms = _to_utc_ms(d)
    utc_dt = datetime.fromtimestamp(utc_ms / 1000, tz=UTC)
    return DateTimeOffset(utc_dt, 0)


def to_local_time(d: DateTimeOffset) -> DateTimeOffset:
    """Convert to local DateTimeOffset."""
    utc_ms = _to_utc_ms(d)
    utc_dt = datetime.fromtimestamp(utc_ms / 1000, tz=UTC)
    local_dt = utc_dt.astimezone()
    offset_td = local_dt.utcoffset()
    offset_ms = int(offset_td.total_seconds() * 1000) if offset_td else 0
    return DateTimeOffset(local_dt, offset_ms)


def to_offset(d: DateTimeOffset, offset_ts: TimeSpan) -> DateTimeOffset:
    """Convert to a DateTimeOffset with the specified offset."""
    new_offset_ms = int(time_span.total_microseconds(offset_ts) / 1000)
    utc_ms = _to_utc_ms(d)
    new_python_offset = timedelta(milliseconds=new_offset_ms)
    utc_dt = datetime.fromtimestamp(utc_ms / 1000, tz=UTC)
    local_dt = utc_dt + new_python_offset
    local_dt = local_dt.replace(tzinfo=timezone(new_python_offset))
    return DateTimeOffset(local_dt, new_offset_ms)


# ---------------------------------------------------------------------------
# Add methods
# ---------------------------------------------------------------------------


def _add_timedelta(d: DateTimeOffset, td: timedelta) -> DateTimeOffset:
    """Add a timedelta, preserving offset. Returns a new DateTimeOffset."""
    # We can't use datetime.__add__ directly because it tries to construct
    # a DateTimeOffset via __new__ with the wrong signature.
    # Instead, construct a plain datetime, add the timedelta, then wrap.
    plain = datetime(d.year, d.month, d.day, d.hour, d.minute, d.second, d.microsecond, d.tzinfo)
    new_dt = plain + td
    return DateTimeOffset(new_dt, d.offset_ms)


def add(d: DateTimeOffset, ts: TimeSpan) -> DateTimeOffset:
    """Add a TimeSpan to a DateTimeOffset."""
    us = time_span.total_microseconds(ts)
    return _add_timedelta(d, timedelta(microseconds=float(us)))


def add_years(d: DateTimeOffset, v: SupportsInt) -> DateTimeOffset:
    new_month = d.month
    new_year = d.year + int(v)
    _days_in_month = _get_days_in_month(new_year, new_month)
    new_day = min(_days_in_month, d.day)
    return create(new_year, new_month, new_day, d.hour, d.minute, d.second, d.microsecond // 1000, d.offset)


def add_months(d: DateTimeOffset, v: SupportsInt) -> DateTimeOffset:
    new_month = d.month + int(v)
    new_month_ = 0
    year_offset = 0
    if new_month > 12:
        new_month_ = int(fmod(new_month, 12))
        year_offset = new_month // 12
        new_month = new_month_
    elif new_month < 1:
        new_month_ = 12 + int(fmod(new_month, 12))
        year_offset = new_month // 12 + (-1 if new_month_ == 12 else 0)
        new_month = new_month_
    new_year = d.year + year_offset
    _days_in_month = _get_days_in_month(new_year, new_month)
    new_day = min(_days_in_month, d.day)
    return create(new_year, new_month, new_day, d.hour, d.minute, d.second, d.microsecond // 1000, d.offset)


def add_days(d: DateTimeOffset, v: SupportsFloat) -> DateTimeOffset:
    return _add_timedelta(d, timedelta(days=float(v)))


def add_hours(d: DateTimeOffset, v: SupportsFloat) -> DateTimeOffset:
    return _add_timedelta(d, timedelta(hours=float(v)))


def add_minutes(d: DateTimeOffset, v: SupportsFloat) -> DateTimeOffset:
    return _add_timedelta(d, timedelta(minutes=float(v)))


def add_seconds(d: DateTimeOffset, v: SupportsFloat) -> DateTimeOffset:
    return _add_timedelta(d, timedelta(seconds=float(v)))


def add_milliseconds(d: DateTimeOffset, v: SupportsFloat) -> DateTimeOffset:
    return _add_timedelta(d, timedelta(milliseconds=float(v)))


def add_microseconds(d: DateTimeOffset, v: SupportsFloat) -> DateTimeOffset:
    return _add_timedelta(d, timedelta(microseconds=float(v)))


def add_ticks(d: DateTimeOffset, v: SupportsFloat) -> DateTimeOffset:
    ms = float(v) / 10000
    return add_milliseconds(d, ms)


# ---------------------------------------------------------------------------
# Subtract / Operators
# ---------------------------------------------------------------------------


@overload
def subtract(x: DateTimeOffset, y: DateTimeOffset) -> TimeSpan: ...
@overload
def subtract(x: DateTimeOffset, y: TimeSpan) -> DateTimeOffset: ...


def subtract(x: DateTimeOffset, y: DateTimeOffset | TimeSpan) -> DateTimeOffset | TimeSpan:
    if isinstance(y, DateTimeOffset):
        # Subtracting two DateTimeOffsets: compare UTC instants
        diff_ms = _to_utc_ms(x) - _to_utc_ms(y)
        return time_span.from_milliseconds(diff_ms)
    # TimeSpan subtraction (TimeSpan is an int subclass, not timedelta)
    us = time_span.total_microseconds(y)
    return _add_timedelta(x, timedelta(microseconds=float(-us)))


@overload
def op_subtraction(x: DateTimeOffset, y: DateTimeOffset) -> TimeSpan: ...
@overload
def op_subtraction(x: DateTimeOffset, y: TimeSpan) -> DateTimeOffset: ...


def op_subtraction(x: DateTimeOffset, y: DateTimeOffset | TimeSpan) -> DateTimeOffset | TimeSpan:
    return subtract(x, y)


def op_addition(x: DateTimeOffset, y: TimeSpan) -> DateTimeOffset:
    return add(x, y)


# ---------------------------------------------------------------------------
# Comparison / Equality
# ---------------------------------------------------------------------------


def equals(d1: DateTimeOffset, d2: DateTimeOffset) -> bool:
    return _to_utc_ms(d1) == _to_utc_ms(d2)


def equals_exact(d1: DateTimeOffset, d2: DateTimeOffset) -> bool:
    return _to_utc_ms(d1) == _to_utc_ms(d2) and d1.offset_ms == d2.offset_ms


def compare(d1: DateTimeOffset, d2: DateTimeOffset) -> int32:
    a = _to_utc_ms(d1)
    b = _to_utc_ms(d2)
    if a < b:
        return int32(-1)
    if a > b:
        return int32(1)
    return int32(0)


def compare_to(d1: DateTimeOffset, d2: DateTimeOffset) -> int32:
    return compare(d1, d2)


# ---------------------------------------------------------------------------
# Unix time
# ---------------------------------------------------------------------------


def from_unix_time_milliseconds(ms: SupportsFloat) -> DateTimeOffset:
    dt = datetime.fromtimestamp(float(ms) / 1000, tz=UTC)
    return DateTimeOffset(dt, 0)


def from_unix_time_seconds(s: SupportsFloat) -> DateTimeOffset:
    dt = datetime.fromtimestamp(float(s), tz=UTC)
    return DateTimeOffset(dt, 0)


def to_unix_time_milliseconds(d: DateTimeOffset) -> int64:
    return int64(_to_utc_ms(d))


def to_unix_time_seconds(d: DateTimeOffset) -> int64:
    return int64(_to_utc_ms(d) // 1000)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _get_days_in_month(year: int, month: int) -> int:
    if month == 2:
        return 29 if (year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)) else 28
    if month in (4, 6, 9, 11):
        return 30
    return 31


__all__ = [
    "DateTimeOffset",
    "add",
    "add_days",
    "add_hours",
    "add_microseconds",
    "add_milliseconds",
    "add_minutes",
    "add_months",
    "add_seconds",
    "add_ticks",
    "add_years",
    "compare",
    "compare_to",
    "create",
    "date",
    "day",
    "day_of_week",
    "day_of_year",
    "equals",
    "equals_exact",
    "from_date",
    "from_date_time",
    "from_ticks",
    "from_unix_time_milliseconds",
    "from_unix_time_seconds",
    "get_utc_ticks",
    "hour",
    "local_date_time",
    "max_value",
    "microsecond",
    "millisecond",
    "min_value",
    "minute",
    "month",
    "now",
    "op_addition",
    "op_subtraction",
    "parse",
    "second",
    "subtract",
    "ticks",
    "time_of_day",
    "to_local_time",
    "to_offset",
    "to_universal_time",
    "to_unix_time_milliseconds",
    "to_unix_time_seconds",
    "total_offset_minutes",
    "try_parse",
    "unix_epoch",
    "utc_date_time",
    "utc_now",
    "year",
]
