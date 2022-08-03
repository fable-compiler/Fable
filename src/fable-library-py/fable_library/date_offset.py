from datetime import datetime, timedelta
from typing import Union

from .types import FSharpRef


def add(d: datetime, ts: timedelta) -> datetime:
    return d + ts


def parse(string: str, detectUTC: bool = False) -> datetime:
    from dateutil import parser

    return parser.parse(string)


def try_parse(
    string: str, style: int, unsigned: bool, bitsize: int, defValue: FSharpRef[datetime]
) -> bool:
    try:
        defValue.contents = parse(string)
        return True
    except Exception:
        return False


def now() -> datetime:
    return datetime.now()


def utc_now() -> datetime:
    return datetime.utcnow()


def op_addition(x: datetime, y: timedelta) -> datetime:
    return x + y


def op_subtraction(
    x: datetime, y: Union[datetime, timedelta]
) -> Union[datetime, timedelta]:
    if isinstance(y, timedelta):
        return x - y

    return x - y


def min_value() -> datetime:
    return datetime.min


__all__ = ["now", "op_addition", "op_subtraction", "parse", "try_parse", "utc_now"]
