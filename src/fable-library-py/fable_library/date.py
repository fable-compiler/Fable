from __future__ import annotations

import re
from datetime import datetime, timedelta, timezone
from math import fmod
from typing import Any

from .singleton_local_time_zone import local_time_zone
from .time_span import TimeSpan, total_microseconds
from .time_span import create as create_time_span
from .types import FSharpRef
from .util import DateKind


# Some of the code in this file has been adapted from
# https://github.com/microsoft/referencesource/blob/51cf7850defa8a17d815b4700b67116e3fa283c2/mscorlib/system/globalization/datetimeformat.cs

short_days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

long_days = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]

short_months = [
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec",
]

long_months = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
]


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
    mc: int = 0,
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
            microsecond=mc + ms * 1_000,
            tzinfo=timezone.utc,
        )
    else:
        date = datetime(year, month, day, h, m, s, mc + ms * 1_000)
        if kind == DateKind.Local:
            date = date.astimezone(local_time_zone)

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
    return d.microsecond // 1_000


def microsecond(d: datetime) -> int:
    return d.microsecond


def to_universal_time(d: datetime) -> datetime:
    return d.astimezone(timezone.utc)


def day_of_week(d: datetime) -> int:
    return (d.weekday() + 1) % 7


def day_of_year(d: datetime) -> int:
    return d.timetuple().tm_yday


def to_short_date_string(date: datetime) -> str:
    return datetime.strftime(date, "%m/%d/%Y")


def to_long_date_string(date: datetime) -> str:
    return datetime.strftime(date, "%A, %d %B %Y")


def to_short_time_string(date: datetime) -> str:
    return datetime.strftime(date, "%H:%M")


def to_long_time_string(date: datetime) -> str:
    return datetime.strftime(date, "%H:%M:%S")


def parse_repeat_token(format: str, pos: int, pattern_char: str) -> int:
    token_length = 0
    internal_pos = pos
    while internal_pos < len(format) and format[internal_pos] == pattern_char:
        internal_pos += 1
        token_length += 1

    return token_length


# Get the next character at the index of 'pos' in the 'format' string.
# Return value of -1 means 'pos' is already at the end of the 'format' string.
# Otherwise, return value is the int value of the next character.
def parse_next_char(format: str, pos: int) -> int:
    if pos >= len(format) - 1:
        return -1

    return ord(format[pos + 1])


def parse_quoted_string(format: str, pos: int) -> tuple[str, int]:
    begin_pos = pos
    format_length = len(format)
    # Get the character used to quote the string
    quote_char = format[pos]

    result = ""
    found_quote = False

    while pos < format_length:
        pos += 1
        current_char = format[pos]
        if current_char == quote_char:
            found_quote = True
            break
        elif current_char == "\\":
            if pos < format_length:
                pos += 1
                result += format[pos]
            else:
                # This means that '\'is the last character in the format string.
                raise ValueError("Invalid string format")
        else:
            result += current_char

    if not found_quote:
        # We couldn't find the matching quote
        raise ValueError(f"Invalid string format could not find matching quote for {quote_char}")

    return (result, pos - begin_pos + 1)


def date_to_string_with_custom_format(date: datetime, format: str, utc: bool) -> str:
    cursor_pos = 0
    token_length = 0
    result = ""
    localized_date = date.astimezone(timezone.utc) if utc else date

    while cursor_pos < len(format):
        token = format[cursor_pos]

        match token:
            case "d":
                token_length = parse_repeat_token(format, cursor_pos, "d")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        result += str(localized_date.day)
                    case 2:
                        result += localized_date.strftime("%d")
                    case 3:
                        result += short_days[day_of_week(localized_date)]
                    case 4:
                        result += long_days[day_of_week(localized_date)]
                    case _:
                        pass
            case "f":
                token_length = parse_repeat_token(format, cursor_pos, "f")
                cursor_pos += token_length
                match token_length:
                    case 1 | 2 | 3 | 4 | 5 | 6:
                        precision = 10 ** (6 - token_length)
                        result += str(localized_date.microsecond // precision).zfill(token_length)
                    # Python datetime only support precision up to the microsecond
                    # so we need to fill the rest with 0
                    case 7:
                        result += str(localized_date.microsecond).zfill(6).ljust(token_length, "0")
                    case _:
                        pass
            case "F":
                token_length = parse_repeat_token(format, cursor_pos, "F")
                cursor_pos += token_length
                match token_length:
                    case 1 | 2 | 3 | 4 | 5 | 6:
                        precision = 10 ** (6 - token_length)
                        value = localized_date.microsecond // precision
                        if value != 0:
                            result += str(value).zfill(token_length)
                    # Python datetime only support precision up to the microsecond
                    # so we can't go further
                    # We alse need to pad start with 0 if the value is not 0
                    case 7:
                        value = localized_date.microsecond
                        if value != 0:
                            result += str(value).zfill(6).rstrip("0")
                    case _:
                        pass
            case "g":
                token_length = parse_repeat_token(format, cursor_pos, "g")
                cursor_pos += token_length
                match token_length:
                    case 1 | 2:
                        result += "A.D."
                    case _:
                        pass

            case "h":
                token_length = parse_repeat_token(format, cursor_pos, "h")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        result += str(localized_date.hour % 12)
                    case 2:
                        result += localized_date.strftime("%I")
                    case _:
                        pass
            case "H":
                token_length = parse_repeat_token(format, cursor_pos, "H")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        result += str(localized_date.hour)
                    case 2:
                        result += localized_date.strftime("%H")
                    case _:
                        pass
            case "K":
                token_length = parse_repeat_token(format, cursor_pos, "K")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        match kind(date):
                            case DateKind.UTC:
                                result += "Z"
                            case DateKind.Local:
                                # %:z is not a perfect match for the .NET equivalent
                                # but it seems to do the job
                                # If needed we can probably compute the offset manually
                                # and format it ourselves
                                result += date.strftime("%:z")
                            case DateKind.Unspecified:
                                result += ""
                    case _:
                        pass
            case "m":
                token_length = parse_repeat_token(format, cursor_pos, "m")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        result += str(localized_date.minute)
                    case 2:
                        result += localized_date.strftime("%M")
                    case _:
                        pass
            case "M":
                token_length = parse_repeat_token(format, cursor_pos, "M")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        result += str(localized_date.month)
                    case 2:
                        result += localized_date.strftime("%m")
                    case 3:
                        result += short_months[month(localized_date) - 1]
                    case 4:
                        result += long_months[month(localized_date) - 1]
                    case _:
                        pass
            case "s":
                token_length = parse_repeat_token(format, cursor_pos, "s")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        result += str(localized_date.second)
                    case 2:
                        result += localized_date.strftime("%S")
                    case _:
                        pass
            case "t":
                token_length = parse_repeat_token(format, cursor_pos, "t")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        result += localized_date.strftime("%p")[:1]
                    case 2:
                        result += localized_date.strftime("%p")
                    case _:
                        pass
            case "y":
                token_length = parse_repeat_token(format, cursor_pos, "y")
                cursor_pos += token_length
                match token_length:
                    case 1:
                        result += str(localized_date.year % 100)
                    case 2:
                        result += str(localized_date.year)[-2:].zfill(2)
                    case length:
                        result += str(localized_date.year).zfill(length)
            case "z":
                token_length = parse_repeat_token(format, cursor_pos, "z")
                cursor_pos += token_length

                match kind(date):
                    case DateKind.UTC:
                        utc_offet_text = localized_date.strftime("%z")
                    case DateKind.Local:
                        utc_offet_text = localized_date.strftime("%z")
                    case DateKind.Unspecified:
                        utc_offet_text = to_local_time(date).strftime("%z")

                sign = utc_offet_text[:1]
                hours = int(utc_offet_text[1:3])
                minutes = int(utc_offet_text[3:5])

                match token_length:
                    case 1:
                        result += f"{sign}{hours}"
                    case 2:
                        result += f"{sign}{hours:02}"
                    case 3 | _:
                        result += f"{sign}{hours:02}:{minutes:02}"
            case ":":
                cursor_pos += 1
                result += ":"
            case "/":
                cursor_pos += 1
                result += "/"
            case "'" | '"':
                quoted_string, quoted_string_length = parse_quoted_string(format, cursor_pos)
                cursor_pos += quoted_string_length
                result += quoted_string
            case "%":
                next_char = parse_next_char(format, cursor_pos)
                if next_char >= 0 and next_char != ord("%"):
                    cursor_pos += 2
                    result += date_to_string_with_custom_format(date, chr(next_char), utc)
                else:
                    raise Exception("Invalid string format")
            case "\\":
                next_char = parse_next_char(format, cursor_pos)
                if next_char >= 0:
                    result += chr(next_char)
                    cursor_pos += 2
                else:
                    raise Exception("Invalid string format")
            case _:
                cursor_pos += 1
                result += token
                pass

    return result


def date_to_string_with_offset(date: datetime, format: str | None = None) -> str:
    utc = date.tzinfo == timezone.utc

    match format:
        case None:
            raise NotImplementedError("date_to_string_with_offset")
        case "O" | "o":
            return date.strftime("%Y-%m-%dT%H:%M:%S.%fZ")
        case _ if len(format) == 1:
            raise Exception("Unrecognized Date print format")
        case _:
            return date_to_string_with_custom_format(date, format, utc)


def date_to_string_with_kind(date: datetime, format: str | None = None) -> str:
    utc = date.tzinfo == timezone.utc

    if not format:
        return date.isoformat() if utc else str(date)
    elif len(format) == 1:
        if format == "d":
            return to_short_date_string(date)
        elif format == "D":
            return to_long_date_string(date)
        elif format == "T":
            return to_long_time_string(date)
        elif format == "t":
            return to_short_time_string(date)
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
    return date.astimezone().replace(tzinfo=local_time_zone)


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
    }

    for pattern, format in formats.items():
        if re.fullmatch(pattern, string):
            return datetime.strptime(string, format)

    # Matches a datetime string in the format "YYYY-MM-DDTHH:MM:SS.ffffff". This format usually parses with
    # `fromisoformat`, but not with Python 3.10
    iso_format_regex = r"(?P<header>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.)(?P<microseconds>\d{0,7})"

    # In Python %f only supports exactly 6 digits so we need to adapt the string
    # If microseconds has 7 digits, we remove the last one
    # If microseconds has less than 6 digits, we pad it with 0
    def adapt_microseconds(m: re.Match[str]) -> str:
        microseconds = m.group("microseconds")
        header_text = m.group("header")

        if len(microseconds) == 7:
            microseconds = microseconds[:-1]

        return header_text + microseconds.ljust(6, "0")

    if re.fullmatch(iso_format_regex, string):
        adapted_string = re.sub(iso_format_regex, adapt_microseconds, string)
        return datetime.strptime(adapted_string, "%Y-%m-%dT%H:%M:%S.%f")

    raise ValueError("Unsupported format by Fable: %s" % (string))


def try_parse(string: str, def_value: FSharpRef[datetime]) -> bool:
    try:
        def_value.contents = parse(string)
        return True
    except Exception:
        return False


def date(d: datetime) -> datetime:
    return create(d.year, d.month, d.day, 0, 0, 0, 0, 0, kind(d))


def is_leap_year(year: int) -> bool:
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)


def days_in_month(year: int, month: int) -> int:
    if month == 2:
        return 29 if is_leap_year(year) else 28

    if month in (4, 6, 9, 11):
        return 30

    return 31


def add_years(d: datetime, v: int) -> datetime:
    new_month = month(d)
    new_year = year(d) + v
    _days_in_month = days_in_month(new_year, new_month)
    new_day = min(_days_in_month, day(d))
    return create(new_year, new_month, new_day, hour(d), minute(d), second(d), millisecond(d), microsecond(d), kind(d))


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
    return create(new_year, new_month, new_day, hour(d), minute(d), second(d), millisecond(d), microsecond(d), kind(d))


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


def kind(d: datetime) -> DateKind:
    if d.tzinfo == timezone.utc:
        return DateKind.UTC

    elif d.tzinfo is None:
        return DateKind.Unspecified

    # Should we check that tzinfo is local_time_zone?
    return DateKind.Local


def specify_kind(d: datetime, kind: DateKind) -> datetime:
    return create(year(d), month(d), day(d), hour(d), minute(d), second(d), millisecond(d), microsecond(d), kind)


def ticks(d: datetime) -> int:
    # Note: It can happens that Ticks differs a little bit from the .NET implementation
    # because of some rounding/precision issues in Python
    # DateTime(1, 1, 1, 0, 0, 0, 0, 99, DateTimeKind.Utc).Ticks should be 990
    # but Python returns 1040
    # because d.timestamp():
    # - returns     -62135596799.9999
    # - instead of  -62135596800000
    # compute timestamp in microseconds
    return unix_epoch_microseconds_to_ticks(int(d.timestamp() * 1_000_000), date_offset(d) * 1_000)


def unix_epoch_microseconds_to_ticks(us: int, offset: int) -> int:
    return int(((us + 62135596800000000) + offset) * 10)


def ticks_to_unix_epoch_microseconds(ticks: int) -> int:
    return int((ticks - 621355968000000000) // 10)


def date_offset(d: datetime) -> int:
    if d.tzinfo == timezone.utc:
        return 0
    else:
        utc_offset = d.utcoffset()

        # Is it correct to force an offset to local time
        # for DateKind.Unspecified?
        if utc_offset is None:
            forced_utc_offset = d.astimezone().utcoffset()
            assert forced_utc_offset is not None
            return int(forced_utc_offset.total_seconds() * 1_000)
        else:
            return int(utc_offset.total_seconds() * 1_000)

    # return 0 if d.tzinfo == timezone.utc else


def create_from_epoch_microseconds(us: int, kind: DateKind | None = None) -> datetime:
    if kind == DateKind.UTC:
        date = datetime.fromtimestamp(us / 1_000_000, timezone.utc)
    else:
        date = datetime.fromtimestamp(us / 1_000_000)
        if kind == DateKind.Local:
            date = date.astimezone()

    return date


def from_ticks(ticks: int, kind: DateKind | None = None) -> datetime:
    # Better default than Unspecified
    kind = kind or DateKind.Local
    date = create_from_epoch_microseconds(ticks_to_unix_epoch_microseconds(ticks), kind)

    # Ticks are local to offset (in this case, either UTC or Local/Unknown).
    # If kind is anything but UTC, that means that the tick number was not
    # in utc, thus getTime() cannot return UTC, and needs to be shifted.
    if kind != DateKind.UTC:
        date = create_from_epoch_microseconds(int(date.timestamp() * 1_000_000 - date_offset(date) * 1_000), kind)

    return date


__all__ = [
    "subtract",
    "op_subtraction",
    "create",
    "year",
    "month",
    "day",
    "hour",
    "minute",
    "second",
    "millisecond",
    "microsecond",
    "to_universal_time",
    "day_of_week",
    "day_of_year",
    "to_short_date_string",
    "to_long_date_string",
    "to_short_time_string",
    "to_long_time_string",
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
    "add",
    "parse",
    "try_parse",
    "date",
    "is_leap_year",
    "days_in_month",
    "add_years",
    "add_months",
    "add_days",
    "add_hours",
    "add_minutes",
    "add_seconds",
    "add_milliseconds",
    "add_microseconds",
    "kind",
    "specify_kind",
    "ticks",
    "date_offset",
    "from_ticks",
]
