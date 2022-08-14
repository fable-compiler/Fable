import locale
import re

from base64 import b64decode, b64encode
from dataclasses import dataclass
from datetime import datetime
from enum import IntEnum
from typing import (
    Any,
    Callable,
    Iterable,
    List,
    Match,
    NoReturn,
    Optional,
    Pattern,
    TypeVar,
    Union,
    cast,
    overload,
)

from .date import to_string as date_to_string
from .numeric import multiply, to_exponential, to_fixed, to_hex, to_precision
from .reg_exp import escape
from .types import to_string


T = TypeVar("T")


_fs_format_regexp: Pattern[str] = re.compile(
    r"(^|[^%])%([0+\- ]*)(\d+)?(?:\.(\d+))?(\w)"
)
_interpolate_regexp: Pattern[str] = re.compile(
    r"(?:(^|[^%])%([0+\- ]*)(\d+)?(?:\.(\d+))?(\w))?%P\(\)"
)
_format_regexp: Pattern[str] = re.compile(
    r"\{(\d+)(,-?\d+)?(?:\:([a-zA-Z])(\d{0,2})|\:(.+?))?\}"
)


IPrintfFormatContinuation = Callable[[Callable[[str], Any]], Callable[[str], Any]]


@dataclass
class IPrintfFormat:
    input: str
    cont: IPrintfFormatContinuation


def printf(input: str) -> IPrintfFormat:
    format: IPrintfFormatContinuation = fs_format(input)
    return IPrintfFormat(input=input, cont=format)


def continue_print(cont: Callable[[str], Any], arg: Union[IPrintfFormat, str]) -> Any:
    """Print continuation."""

    if isinstance(arg, IPrintfFormat):
        ret = arg.cont(cont)
        return ret

    return cont(arg)


def to_console(arg: Union[IPrintfFormat, Any]) -> Any:
    return continue_print(print, arg)


def to_console_error(arg: Union[IPrintfFormat, str]):
    return continue_print(lambda x: print(x), arg)


# Set return to `Any` since `Union[str, Callable]` will make type
# checkers really unhappy.
def to_text(arg: Union[IPrintfFormat, str]) -> Union[str, Callable[..., Any]]:
    cont: Callable[[str], Any] = lambda x: x
    return continue_print(cont, arg)


def to_fail(arg: Union[IPrintfFormat, str]):
    def fail(msg: str):
        raise Exception(msg)

    return continue_print(fail, arg)


def format_replacement(
    rep: Any, flags: Any, padLength: Any, precision: Any, format: Any
):
    sign = ""
    flags = flags or ""
    format = format or ""

    if isinstance(rep, (int, float)):
        if format not in ["x", "X"]:
            if rep < 0:
                rep = rep * -1
                sign = "-"
            else:
                if flags.find(" ") >= 0:
                    sign = " "
                elif flags.find("+") >= 0:
                    sign = "+"

        if format == "x":
            rep = to_hex(cast(int, rep))
        elif format == "X":
            rep = to_hex(cast(int, rep)).upper()
        if format in ("f", "F"):
            precision = int(precision) if precision is not None else 6
            rep = to_fixed(rep, precision)
        elif format in ("g", "G"):
            rep = (
                to_precision(rep, precision)
                if precision is not None
                else to_precision(rep)
            )
        elif format in ("e", "E"):
            rep = (
                to_exponential(rep, precision)
                if precision is not None
                else to_exponential(rep)
            )
        else:  # AOid
            rep = to_string(rep)

    elif isinstance(rep, datetime):
        rep = date_to_string(rep)
    else:
        rep = to_string(rep)

    if padLength is not None:
        padLength = int(padLength)
        zeroFlag = flags.find("0") >= 0  # Use '0' for left padding
        minusFlag = flags.find("-") >= 0  # Right padding
        ch = " " if minusFlag or not zeroFlag else "0"
        if ch == "0":
            rep = pad_left(rep, padLength - len(sign), ch, minusFlag)
            rep = sign + rep
        else:
            rep = pad_left(sign + rep, padLength, ch, minusFlag)

    else:
        rep = sign + rep

    return rep


def interpolate(string: str, values: List[Any]) -> str:
    valIdx = 0
    strIdx = 0
    result = ""
    matches = _interpolate_regexp.finditer(string)
    for match in matches:
        # The first group corresponds to the no-escape char (^|[^%]), the actual pattern starts in the next char
        # Note: we don't use negative lookbehind because some browsers don't support it yet
        matchIndex = match.start() + len(match[1] or "")
        result += string[strIdx:matchIndex].replace("%%", "%")
        [_, flags, padLength, precision, format] = match.groups()
        # print(match.groups())
        result += format_replacement(
            values[valIdx], flags, padLength, precision, format
        )
        valIdx += 1

        strIdx = match.end()

    result += string[strIdx:].replace("%%", "%")
    return result


def format_once(str2: str, rep: Any):
    def match(m: Match[str]):
        prefix, flags, pad_length, precision, format = m.groups()
        once: str = format_replacement(rep, flags, pad_length, precision, format)
        return prefix + once.replace("%", "%%")

    ret = _fs_format_regexp.sub(match, str2, count=1)
    return ret


def create_printer(string: str, cont: Callable[..., Any]):
    def _(*args: Any):
        str_copy: str = string
        for arg in args:
            str_copy = format_once(str_copy, arg)

        if _fs_format_regexp.search(str_copy):
            return create_printer(str_copy, cont)
        return cont(str_copy.replace("%%", "%"))

    return _


def fs_format(str: str) -> Callable[[Callable[..., Any]], Any]:
    def _(cont: Callable[..., Any]):
        if _fs_format_regexp.search(str):
            return create_printer(str, cont)
        return cont(str)

    return _


def format(string: str, *args: Any) -> str:
    if not string and args:
        # Called with culture info
        string = args[0]
        args = args[1:]

    def match(m: Match[str]) -> str:
        idx, padLength, format, precision_, pattern = list(m.groups())
        rep = args[int(idx)]
        if isinstance(rep, (int, float)):
            precision: Optional[int] = None
            try:
                precision: Optional[int] = int(precision_)
            except Exception:
                pass

            if format in ["f", "F"]:
                precision = precision if precision is not None else 2
                rep = to_fixed(rep, precision)

            elif format in ["g", "G"]:
                rep = (
                    to_precision(rep, precision)
                    if precision is not None
                    else to_precision(rep)
                )

            elif format in ["e", "E"]:
                rep = (
                    to_exponential(rep, precision)
                    if precision is not None
                    else to_exponential(rep)
                )

            elif format in ["p", "P"]:
                precision = precision if precision is not None else 2
                rep = to_fixed(multiply(rep, 100), precision) + " %"

            elif format in ["d", "D"]:
                rep = (
                    pad_left(str(rep), precision, "0")
                    if precision is not None
                    else str(rep)
                )

            elif format in ["x", "X"] and isinstance(rep, int):
                rep = (
                    pad_left(to_hex(rep), precision, "0")
                    if precision is not None
                    else to_hex(rep)
                )
                if format == "X":
                    rep = rep.upper()
            elif pattern:
                sign = ""

                def match(m: Match[str]):
                    nonlocal sign, rep

                    intPart, decimalPart = list(m.groups())
                    if rep < 0:
                        rep = multiply(rep, -1)
                        sign = "-"

                    rep = to_fixed(rep, len(decimalPart) - 1 if decimalPart else 0)
                    return pad_left(
                        rep,
                        len(intPart or "")
                        - len(sign)
                        + (len(decimalPart) if decimalPart else 0),
                        "0",
                    )

                rep = re.sub(r"(0+)(\.0+)?", match, pattern)
                rep = sign + rep

        elif isinstance(rep, datetime):
            rep = date_to_string(rep, pattern or format)
        else:
            rep = to_string(rep)

        try:
            padLength = int((padLength or " ")[1:])
            rep = pad_left(str(rep), abs(padLength), " ", padLength < 0)
        except ValueError:
            pass

        return str(rep)

    return _format_regexp.sub(match, string)


def initialize(n: int, f: Callable[[int], str]) -> str:
    if n < 0:
        raise Exception("String length must be non-negative")

    return "".join([f(i) for i in range(n)])


def insert(string: str, startIndex: int, value: str):
    if startIndex < 0 or startIndex > len(string):
        raise ValueError(
            "startIndex is negative or greater than the length of this instance."
        )

    return string[:startIndex] + value + string[startIndex:]


def is_null_or_empty(string: Optional[str]):
    return not isinstance(string, str) or not len(string)


def is_null_or_white_space(string: Optional[Any]) -> bool:
    return not string or not isinstance(string, str) or string.isspace()


def concat(*xs: Iterable[Any]) -> str:
    return "".join(map(str, xs))


def join(delimiter: str, xs: Iterable[Any]) -> str:
    return delimiter.join((str(x) for x in xs))


def join_with_indices(delimiter: str, xs: List[str], startIndex: int, count: int):
    endIndexPlusOne = startIndex + count
    if endIndexPlusOne > len(xs):
        raise ValueError("Index and count must refer to a location within the buffer.")

    return delimiter.join(xs[startIndex:endIndexPlusOne])


def not_supported(name: str) -> NoReturn:
    raise Exception(
        "The environment doesn't support '" + name + "', please use a polyfill."
    )


def to_base64string(in_array: bytes) -> str:
    return b64encode(in_array).decode("utf8")


def from_base64string(b64encoded: str) -> bytes:
    return b64decode(b64encoded)


def pad_left(
    string: str, length: int, ch: Optional[str] = None, isRight: Optional[bool] = False
) -> str:
    ch = ch or " "
    length = length - len(string)
    for _ in range(length):
        string = string + ch if isRight else ch + string

    return string


def pad_right(string: str, len: int, ch: Optional[str] = None) -> str:
    return pad_left(string, len, ch, True)


def remove(string: str, startIndex: int, count: Optional[int] = None):
    if startIndex >= len(string):
        raise ValueError("startIndex must be less than length of string")

    if count and (startIndex + count) > len(string):
        raise ValueError("Index and count must refer to a location within the string.")

    return string[:startIndex] + (
        string[startIndex + count :] if count is not None else ""
    )


def replace(string: str, search: str, replace: str):
    return string.replace(search, replace)


def replicate(n: int, x: str) -> str:
    return initialize(n, lambda _=0: x)


def get_char_at_index(input: str, index: int):
    if index < 0 or index >= len(input):
        raise ValueError("Index was outside the bounds of the array.")

    return input[index]


def split(
    string: str,
    splitters: Union[str, List[str]],
    count: Optional[int] = None,
    removeEmpty: int = 0,
) -> List[str]:
    """Split string

    Returns a string array that contains the substrings in this instance
    that are delimited by elements of a specified string or Unicode
    character array."""

    if count and count < 0:
        raise ValueError("Count cannot be less than zero")

    if count == 0:
        return []

    if isinstance(splitters, str):
        if not removeEmpty:
            return string.split(splitters, count - 1 if count else -1)

        splitters = [splitters]

    splitters = [escape(x) for x in splitters] or [" "]

    i = 0
    splits: List[str] = []
    matches = re.finditer("|".join(splitters), string)
    for m in matches:
        if count is not None and count <= 1:
            break

        split = string[i : m.start()]
        if split or not removeEmpty:
            splits.append(split)

            count = count - 1 if count is not None else count

        i = m.end()

    if (count is None or count and count > 0) and len(string) - i > -1:
        split = string[i:]
        if split or not removeEmpty:
            splits.append(split)

    return splits


def trim(string: str, *chars: str) -> str:
    if not len(chars):
        return string.strip()

    pattern = "[" + escape("".join(chars)) + "]+"
    return re.sub(pattern + "$", "", re.sub("^" + pattern, "", string))


def trim_start(string: str, *chars: str) -> str:
    if not len(chars):
        return string.lstrip()

    return re.sub("^[" + escape("".join(chars)) + "]+", "", string)


def trim_end(string: str, *chars: str) -> str:
    if not len(chars):
        return string.rstrip()

    return re.sub("[" + escape("".join(chars)) + "]+$", "", string)


def filter(pred: Callable[[str], bool], x: str) -> str:
    return "".join(c for c in x if pred(c))


def substring(string: str, startIndex: int, length: Optional[int] = None) -> str:
    if startIndex + (length or 0) > len(string):
        raise ValueError("Invalid startIndex and/or length")

    if length is not None:
        return string[startIndex : startIndex + length]

    return string[startIndex:]


class StringComparison(IntEnum):
    CurrentCulture = 0
    CurrentCultureIgnoreCase = 1
    InvariantCulture = 2
    InvariantCultureIgnoreCase = 3
    Ordinal = 4
    OrdinalIgnoreCase = 5


def cmp(x: str, y: str, ic: Union[bool, StringComparison]) -> int:
    def is_ignore_case(i: Union[bool, StringComparison]) -> bool:
        return (
            i is True
            or i == StringComparison.CurrentCultureIgnoreCase
            or i == StringComparison.InvariantCultureIgnoreCase
            or i == StringComparison.OrdinalIgnoreCase
        )

    def is_ordinal(i: Union[bool, int]) -> bool:
        return i == StringComparison.Ordinal or i == StringComparison.OrdinalIgnoreCase

    if not x:
        return 0 if not y else -1
    if not y:
        return 1  # everything is bigger than None

    if is_ordinal(ic):
        if is_ignore_case(ic):
            x = x.lower()
            y = y.lower()

        return 0 if x == y else -1 if x < y else 1
    elif is_ignore_case(ic):
        x = x.lower()
        y = y.lower()

    return locale.strcoll(x, y)


@overload
def compare(string1: str, string2: str, /) -> int:
    """Compares two specified String objects and returns an integer that
    indicates their relative position in the sort order."""
    ...


@overload
def compare(
    string1: str, string2: str, ignore_case: bool, culture: StringComparison, /
) -> int:
    ...


def compare(*args: Any) -> int:
    """Compares two specified String objects and returns an integer that
    indicates their relative position in the sort order.

    All overloads of the Compare method return a 32-bit signed integer
    indicating the lexical relationship between the two comparands.

    - Less than zero: The first substring precedes the second
        substring in the sort order.
    - Zero: The substrings occur in the same position in the sort
        order, or length is zero.
    - Greater than zero: The first substring follows the second
        substring in the sort order.

    Returns:
        Integer that indicates the relationship of the two substrings
        to each other in the sort order:
    """
    length = len(args)

    if length == 2:
        return cmp(args[0], args[1], False)
    if length == 3:
        return cmp(args[0], args[1], args[2])
    if length == 4:
        return cmp(args[0], args[1], args[2])
    if length == 5:
        return cmp(
            args[0][args[1] : args[4] + 1], args[2][args[3] : args[4] + 1], False
        )
    if length == 6:
        return cmp(
            args[0][args[1] : args[4] + 1], args[2][args[3] : args[4] + 1], args[5]
        )
    if length == 7:
        return cmp(
            args[0][args[1] : args[4] + 1], args[2][args[3] : args[4] + 1], args[5]
        )
    raise Exception("String.compare: Unsupported number of parameters")


def compare_to(this: str, other: str) -> int:
    """Compare this string with other

    Compares this instance with a specified String object and indicates
    whether this instance precedes, follows, or appears in the same
    position in the sort order as the specified string.
    """
    return cmp(this, other, StringComparison.CurrentCulture)


def ends_with(string: str, search: str):
    idx = string.rfind(search)
    return idx >= 0 and idx == len(string) - len(search)


def starts_with(string: str, pattern: str, ic: int):
    if len(string) >= len(pattern):
        return cmp(string[0 : len(pattern)], pattern, True if ic else False) == 0

    return False


def index_of_any(string: str, any_of: List[str], *args: int):
    if not string:
        return -1

    start_index = args[0] if len(args) > 0 else 0
    if start_index < 0:
        raise ValueError("Start index cannot be negative")

    length = args[1] if len(args) > 1 else len(string) - start_index
    if length < 0:
        raise ValueError("Length cannot be negative")

    if length > len(string) - start_index:
        raise ValueError("Invalid start_index and length")

    string = string[start_index:length]
    for c in any_of:
        index = string.find(c)
        if index > -1:
            return index + start_index

    return -1
