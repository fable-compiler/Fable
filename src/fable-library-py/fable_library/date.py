from __future__ import annotations

import re
from datetime import datetime, timedelta, timezone
from re import Match
from typing import Any
from math import fmod

from .time_span import TimeSpan, total_microseconds
from .time_span import create as create_time_span
from .types import FSharpRef
from .util import DateKind


FORMAT_REGEXP = re.compile(r"(\w)\1*")


def subtract(x: datetime, y: datetime | TimeSpan) -> datetime | TimeSpan:
    if isinstance(y, datetime):
        delta = x - y
        # ts.microseconds only contains the microseconds provided to the constructor
        # so we need to calculate the total microseconds ourselves
        delta_microseconds = delta.days * (24 * 3600 * 10**6) + delta.seconds * 10**6 + delta.microseconds
        return create_time_span(0, 0, 0, 0, 0, delta_microseconds)

    return x - timedelta(microseconds=total_microseconds(y))


def op_subtraction(x: datetime, y: datetime | TimeSpan) -> datetime | TimeSpan:
    return subtract(x, y)


def create(
    year: int,
    month: int,
    day: int,
    h: int = 0,
    m: int = 0,
    s: int = 0,
    ms: int = 0,
    kind: DateKind | None = None,
) -> datetime:
    if kind == DateKind.UTC:
        date = datetime(
            year=year,
            month=month,
            day=day,
            hour=h,
            minute=m,
            second=s,
            microsecond=ms * 1000,
            tzinfo=timezone.utc,
        )
    else:
        date = datetime(year, month, day, h, m, s, ms * 1000)

    return date


def year(d: datetime) -> int:
    return d.year


def month(d: datetime) -> int:
    return d.month


def day(d: datetime) -> int:
    return d.day


def hour(d: datetime) -> int:
    return d.hour


def minute(d: datetime) -> int:
    return d.minute


def second(d: datetime) -> int:
    return d.second


def millisecond(d: datetime) -> int:
    return d.microsecond // 1000


def microsecond(d: datetime) -> int:
    return d.microsecond


def to_universal_time(d: datetime) -> datetime:
    return d.astimezone(timezone.utc)


def day_of_week(d: datetime) -> int:
    return (d.weekday() + 1) % 7


def day_of_year(d: datetime) -> int:
    return d.timetuple().tm_yday


def date_to_string_with_custom_format(date: datetime, format: str, utc: bool) -> str:
    def match(match: Match[str]) -> str:
        group = match.group()
        m = group[:1]
        rep = None

        if m == "y":
            y = date.astimezone(timezone.utc).year if utc else date.year
            rep = y % 100 if len(group) < 4 else y
        elif m == "M":
            rep = date.astimezone(timezone.utc).month if utc else date.month
        elif m == "H":
            rep = date.astimezone(timezone.utc).hour if utc else date.hour
        elif m == "m":
            rep = date.astimezone(timezone.utc).minute if utc else date.minute
        elif m == "s":
            rep = date.astimezone(timezone.utc).second if utc else date.second
        elif m == "f":
            rep = date.astimezone(timezone.utc).microsecond if utc else date.microsecond
            rep = rep // 1000

        if rep:
            return f"0{rep}" if (rep < 10 and len(group) > 1) else f"{rep}"

        return group

    ret = FORMAT_REGEXP.sub(match, format)
    return ret

    # return format.replace(/(\w)\1*/g, (match) => {
    #     let rep = Number.NaN;
    #     switch (match.substring(0, 1)) {
    #         case "y":
    #             const y = utc ? date.getUTCFullYear() : date.getFullYear();
    #             rep = match.length < 4 ? y % 100 : y;
    #             break;
    #         case "M":
    #             rep = (utc ? date.getUTCMonth() : date.getMonth()) + 1;
    #             break;
    #         case "d":
    #             rep = utc ? date.getUTCDate() : date.getDate();
    #             break;
    #         case "H":
    #             rep = utc ? date.getUTCHours() : date.getHours();
    #             break;
    #         case "h":
    #             const h = utc ? date.getUTCHours() : date.getHours();
    #             rep = h > 12 ? h % 12 : h;
    #             break;
    #         case "m":
    #             rep = utc ? date.getUTCMinutes() : date.getMinutes();
    #             break;
    #         case "s":
    #             rep = utc ? date.getUTCSeconds() : date.getSeconds();
    #             break;
    #         case "f":
    #             rep = utc ? date.getUTCMilliseconds() : date.getMilliseconds();
    #             break;
    #     }
    #     if (Number.isNaN(rep)) {
    #         return match;
    #     }
    #     else {
    #         return (rep < 10 && match.length > 1) ? "0" + rep : "" + rep;
    #     }


# def dateToStringWithOffset(date, format=None):
#     d = new Date(date.getTime() + ((_a = date.offset) !== null && _a !== void 0 ? _a : 0));
#     if (typeof format !== "string") {
#         return d.toISOString().replace(/\.\d+/, "").replace(/[A-Z]|\.\d+/g, " ") + dateOffsetToString(((_b = date.offset) !== null && _b !== void 0 ? _b : 0));
#     }
#     else if (format.length === 1) {
#         switch (format) {
#             case "D":
#             case "d": return dateToHalfUTCString(d, "first");
#             case "T":
#             case "t": return dateToHalfUTCString(d, "second");
#             case "O":
#             case "o": return dateToISOStringWithOffset(d, ((_c = date.offset) !== null && _c !== void 0 ? _c : 0));
#             default: throw new Error("Unrecognized Date print format");
#         }

#     else:
#         return dateToStringWithCustomFormat(d, format, True)


def date_to_string_with_offset(date: datetime, format: str | None = None) -> str:
    match format:
        case None:
            raise NotImplementedError("date_to_string_with_offset")
        case "O" | "o":
            return date.strftime("%Y-%m-%dT%H:%M:%S.%fZ")
        case _ if len(format) == 1:
            raise Exception("Unrecognized Date print format")
        case _:
            return date_to_string_with_custom_format(date, format, True)


def date_to_string_with_kind(date: datetime, format: str | None = None) -> str:
    utc = date.tzinfo == timezone.utc
    if not format:
        return date.isoformat() if utc else str(date)

    elif len(format) == 1:
        if format == "D" or format == "d":
            return dateToHalfUTCString(date, "first") if utc else str(date.date())
        elif format == "T" or format == "t":
            return dateToHalfUTCString(date, "second") if utc else str(date.time())
        elif format == "O" or format == "o":
            return date.astimezone().isoformat(timespec="milliseconds")
        else:
            raise Exception("Unrecognized Date print format")

    else:
        return date_to_string_with_custom_format(date, format, utc)


def to_string(date: datetime, format: str | None = None, provider: Any | None = None) -> str:
    if date.tzinfo:
        return date_to_string_with_offset(date, format)

    return date_to_string_with_kind(date, format)


def now() -> datetime:
    return datetime.now()


def utc_now() -> datetime:
    return datetime.utcnow()


def today() -> datetime:
    return datetime.replace(now(), hour=0, minute=0, second=0, microsecond=0)


def to_local_time(date: datetime) -> datetime:
    return date.astimezone()


def compare(x: datetime, y: datetime) -> int:
    if x == y:
        return 0

    if x < y:
        return -1

    return 1


def equals(x: datetime, y: datetime) -> bool:
    return x == y


def max_value() -> datetime:
    return datetime.max


def min_value() -> datetime:
    return datetime.min


def op_addition(x: datetime, y: TimeSpan) -> datetime:
    return x + timedelta(microseconds=total_microseconds(y))


def add(x: datetime, y: TimeSpan) -> datetime:
    return op_addition(x, y)


def parse(string: str) -> datetime:
    try:
        return datetime.fromisoformat(string).astimezone()
    except ValueError:
        pass

    # For the time-only formats, needs a special treatment
    # because in Python, they are set in 1900-01-01 while
    # in F# they are set in the current date
    hoursFormats = {
        # Matches a time string in 24-hour format "HH:MM:SS"
        r"^\d{1,2}:\d{1,2}:\d{2}$": "%H:%M:%S",
        # Matches a time string in 12-hour format with AM/PM "HH:MM:SS AM" or "HH:MM:SS PM"
        r"^(0?[1-9]|1[0-2]):([0-5][1-9]|0?[0-9]):([0-5][0-9]|0?[0-9]) [AP]M$": "%I:%M:%S %p",
        r"^\d{1,2}:\d{1,2}:\d{2} [AP]M$": "%H:%M:%S %p",
    }

    for pattern, format in hoursFormats.items():
        if re.fullmatch(pattern, string):
            hourDate = datetime.strptime(string, format)

            # If the hour is 0 PM, then in .NET it is 12 PM (python keeps it as 0)
            hourOffset = 12 if hourDate.hour == 0 and string.endswith(" PM") else 0

            return datetime.replace(
                now(),
                hour=hourDate.hour + hourOffset,
                minute=hourDate.minute,
                second=hourDate.second,
                microsecond=0,
            )

    formats = {
        # 9/10/2014 1:50:34 PM
        r"^(0?[1-9]|1[0-2])\/(0?[1-9]|1[0-2])\/\d{4} ([0-9]|(0|1)[0-9]|2[0-4]):([0-5][0-9]|0?[0-9]):([0-5][0-9]|0?[0-9]) [AP]M$": "%m/%d/%Y %I:%M:%S %p",
        # 9/10/2014 1:50:34
        r"^(0?[1-9]|1[0-2])\/(0?[1-9]|1[0-2])\/\d{4} ([0-9]|(0|1)[0-9]|2[0-4]):([0-5][0-9]|0?[0-9]):([0-5][0-9]|0?[0-9])$": "%m/%d/%Y %H:%M:%S",
        # Matches a datetime string in the format "YYYY-MM-DDTHH:MM:SS.ffffff". This format usually parses with
        # `fromisoformat`, but not with Python 3.10
        r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d{7}$": "%Y-%m-%dT%H:%M:%S.%f000",
    }

    for pattern, format in formats.items():
        if re.fullmatch(pattern, string):
            return datetime.strptime(string, format)

    raise ValueError("Unsupported format by Fable: %s" % (string))


def try_parse(string: str, defValue: FSharpRef[datetime]) -> bool:
    try:
        defValue.contents = parse(string)
        return True
    except Exception:
        return False


def date(d: datetime) -> datetime:
    # TODO: Should forward d.kind
    return create(d.year, d.month, d.day)


def days_in_month(year: int, month: int) -> int:
    if month == 2:
        return 29 if year % 4 == 0 and (year % 100 != 0 or year % 400 == 0) else 28

    if month in (4, 6, 9, 11):
        return 30

    return 31


def add_years(d: datetime, v: int) -> datetime:
    new_month = month(d)
    new_year = year(d) + v
    _days_in_month = days_in_month(new_year, new_month)
    new_day = min(_days_in_month, day(d))
    # TODO: Should forward d.kind
    return create(new_year, new_month, new_day, hour(d), minute(d), second(d), millisecond(d))


def add_months(d: datetime, v: int) -> datetime:
    new_month = month(d) + v
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
    new_year = year(d) + year_offset
    _days_in_month = days_in_month(new_year, new_month)
    new_day = min(_days_in_month, day(d))
    # TODO: Should forward d.kind
    return create(new_year, new_month, new_day, hour(d), minute(d), second(d), millisecond(d))


def add_days(d: datetime, v: int) -> datetime:
    return d + timedelta(days=v)


def add_hours(d: datetime, v: int) -> datetime:
    return d + timedelta(hours=v)


def add_minutes(d: datetime, v: int) -> datetime:
    return d + timedelta(minutes=v)


def add_seconds(d: datetime, v: int) -> datetime:
    return d + timedelta(seconds=v)


def add_milliseconds(d: datetime, v: int) -> datetime:
    return d + timedelta(milliseconds=v)


def add_microseconds(d: datetime, v: int) -> datetime:
    return d + timedelta(microseconds=v)


__all__ = [
    "add",
    "op_subtraction",
    "subtract",
    "create",
    "year",
    "month",
    "day",
    "hour",
    "minute",
    "second",
    "millisecond",
    "microsecond",
    "day_of_week",
    "day_of_year",
    "date_to_string_with_custom_format",
    "date_to_string_with_offset",
    "date_to_string_with_kind",
    "to_string",
    "now",
    "utc_now",
    "today",
    "to_local_time",
    "compare",
    "equals",
    "max_value",
    "min_value",
    "op_addition",
    "parse",
    "to_universal_time",
    "try_parse",
    "date",
    "add_years",
    "add_months",
    "add_days",
    "add_hours",
    "add_minutes",
    "add_seconds",
    "add_milliseconds",
    "add_microseconds",
]
