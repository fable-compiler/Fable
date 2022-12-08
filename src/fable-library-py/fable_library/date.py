import re

from datetime import datetime, timedelta, timezone
from typing import Any, Match, Optional

from .types import FSharpRef
from .util import DateKind


formatRegExp = re.compile(r"(\w)\1*")


def op_subtraction(x: datetime, y: datetime) -> timedelta:
    return x - y


def create(
    year: int,
    month: int,
    day: int,
    h: int = 0,
    m: int = 0,
    s: int = 0,
    ms: int = 0,
    kind: Optional[DateKind] = None,
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

    ret = formatRegExp.sub(match, format)
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


def date_to_string_with_offset(date: datetime, format: Optional[str] = None) -> str:
    if format and len(format) == 1:
        return date_to_string_with_custom_format(date, format, True)

    raise NotImplementedError("date_to_string_with_offset")


def date_to_string_with_kind(date: datetime, format: Optional[str] = None) -> str:
    utc = date.tzinfo == timezone.utc
    if not format:
        return date.isoformat() if utc else str(date)

    elif len(format) == 1:
        if format == "D" or format == "d":
            return dateToHalfUTCString(date, "first") if utc else str(date.date())
        elif format == "T" or format == "t":
            return dateToHalfUTCString(date, "second") if utc else str(date.time())
        elif format == "O" or format == "o":
            return dateToISOString(date, utc)
        else:
            raise Exception("Unrecognized Date print format")

    else:
        return date_to_string_with_custom_format(date, format, utc)


def to_string(
    date: datetime, format: Optional[str] = None, provider: Optional[Any] = None
) -> str:
    if date.tzinfo:
        return date_to_string_with_offset(date, format)

    return date_to_string_with_kind(date, format)


def now() -> datetime:
    return datetime.now()


def utc_now() -> datetime:
    return datetime.utcnow()


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


def op_addition(x: datetime, y: timedelta) -> datetime:
    return x + y


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
