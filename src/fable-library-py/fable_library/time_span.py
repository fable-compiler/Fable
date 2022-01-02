from datetime import timedelta
from typing import Optional


def total_seconds(ts: timedelta) -> float:
    return ts.total_seconds()


def from_milliseconds(msecs: int) -> timedelta:
    return timedelta(milliseconds=msecs)


def from_ticks(ticks: int) -> timedelta:
    return timedelta(microseconds=ticks // 10)


def to_milliseconds(td: timedelta) -> int:
    return int(td.total_seconds() * 1000)


def create(
    d: int = 0, h: Optional[int] = None, m: Optional[int] = None, s: Optional[int] = None, ms: Optional[int] = None
) -> timedelta:
    if h is None and m is None and s is None and ms is None:
        return from_ticks(d)

    return timedelta(days=d, hours=h or 0, minutes=m or 0, seconds=s or 0, milliseconds=ms or 0)
