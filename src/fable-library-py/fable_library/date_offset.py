from __future__ import annotations

from datetime import datetime, timedelta, timezone

from . import time_span
from .time_span import TimeSpan
from .types import FSharpRef


def timedelta_total_microseconds(td: timedelta) -> int:
    # timedelta doesn't expose total_microseconds
    # so we need to calculate it ourselves
    return td.days * (24 * 3600) + td.seconds * 10**6 + td.microseconds


def add(d: datetime, ts: timedelta) -> datetime:
    return d + ts


def parse(string: str, detectUTC: bool = False) -> datetime:
    from dateutil import parser

    return parser.parse(string)


def try_parse(string: str, style: int, unsigned: bool, bitsize: int, defValue: FSharpRef[datetime]) -> bool:
    try:
        defValue.contents = parse(string)
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
) -> datetime:
    python_offset: timedelta | None = None
    if isinstance(ms, TimeSpan):
        python_offset = timedelta(microseconds=time_span.total_microseconds(ms))
        ms = 0

    if python_offset is None:
        return datetime(year, month, day, h, m, s, ms)

    tzinfo = timezone(python_offset)
    return datetime(year, month, day, h, m, s, ms, tzinfo=tzinfo)


def now() -> datetime:
    return datetime.now()


def utc_now() -> datetime:
    return datetime.utcnow()


def op_addition(x: datetime, y: timedelta) -> datetime:
    return x + y


def op_subtraction(x: datetime, y: datetime | TimeSpan) -> datetime | TimeSpan:
    if isinstance(y, TimeSpan):
        return x - timedelta(microseconds=time_span.total_microseconds(y))

    return time_span.create(0, 0, 0, 0, 0, timedelta_total_microseconds(x - y))


def min_value() -> datetime:
    return datetime.min


__all__ = ["now", "op_addition", "op_subtraction", "parse", "try_parse", "utc_now"]
