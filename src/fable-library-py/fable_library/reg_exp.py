from __future__ import annotations

import re
from collections.abc import Callable, Iterator
from functools import lru_cache
from itertools import islice
from operator import methodcaller
from re import Match, Pattern

from .array_ import Array
from .protocols import IEnumerator
from .types import IntegerTypes
from .util import UNIT, Enumerator, Unit


# Fast path: Python and .NET only differ after an empty match. So if a pattern can never match an
# empty string, re.sub and re.split already give the .NET result and we call them directly.
# Otherwise we must first scan the input to check, which makes Replace and Split about 1.7x slower.
# re._parser is the only way to know if a pattern can match an empty string. It is a private
# CPython module (no typeshed stub), so a future Python can remove it. Then we always scan the
# input: the result stays correct, only slower.
try:
    import re._parser as _parser  # ty: ignore[import-not-found]
except ImportError:
    _parser = None


MatchEvaluator = Callable[[Match[str]], str]


class EmptyMatch:
    """A failed match, like .NET's Match.Empty."""

    def start(self) -> int:
        return 0

    def end(self) -> int:
        return 0

    def __getitem__(self, group: int | str) -> str:
        return ""

    def __bool__(self) -> bool:
        return False

    def __str__(self) -> str:
        return ""


EMPTY_MATCH = EmptyMatch()


class GroupCollection:
    def __init__(self, groups: list[str | None], named_groups: dict[str, str | None]) -> None:
        self.named_groups = named_groups
        self.groups = groups

    def values(self) -> list[str | None]:
        return list(self.groups)

    def __len__(self) -> int:
        return len(self.groups)

    def __getitem__(self, key: int | str) -> str | None:
        if isinstance(key, IntegerTypes):
            return self.groups[key]
        else:
            return self.named_groups.get(key)

    def __iter__(self) -> Iterator[str | None]:
        return iter(self.groups)

    def GetEnumerator(self, __unit: Unit = UNIT) -> IEnumerator[str | None]:
        return Enumerator(iter(self.groups))


class MatchCollection:
    """A collection of Match objects that supports both iteration and len()."""

    def __init__(self, matches: list[Match[str]]) -> None:
        self._matches = matches

    def __len__(self) -> int:
        return len(self._matches)

    def __getitem__(self, index: int) -> Match[str]:
        return self._matches[index]

    def __iter__(self) -> Iterator[Match[str]]:
        return iter(self._matches)

    def GetEnumerator(self, __unit: Unit = UNIT) -> IEnumerator[Match[str]]:
        return Enumerator(iter(self._matches))


def _options_to_flags(options: int) -> int:
    """Convert .NET options to Python flags."""
    if options & ~(1 | 2 | 8 | 16 | 256 | 1024):
        raise Exception(
            "RegexOptions only supports: IgnoreCase, Multiline, Compiled, Singleline, ECMAScript and NonBacktracking"
        )
    flags = 256
    if options & 1:
        flags |= re.IGNORECASE
    if options & 2:
        flags |= re.MULTILINE
    if options & 16:
        flags |= re.S
    return flags


def create(pattern: str, options: int = 0) -> Pattern[str]:
    # raw_pattern = pattern.encode("unicode_escape").decode("utf-8")
    flags = _options_to_flags(options)
    # Convert .NET named group syntax (?<name>...) to Python (?P<name>...).
    # Only replace ?< when followed by a word character (name start), not by
    # = or ! which are part of lookbehind assertions (?<=...) and (?<!...).
    pattern = re.sub(r"\?<(?=[A-Za-z_])", "?P<", pattern)
    return re.compile(pattern, flags=flags)


def create_unicode(pattern: str, options: int = 0) -> Pattern[str]:
    reg = create(pattern, options)
    if options & 256:
        return reg
    return re.compile(reg.pattern, reg.flags & ~re.ASCII)


def escape(string: str) -> str:
    return re.escape(string)


def unescape(string: str) -> str:
    return re.sub(r"\\(.)", r"\1", string)


def match(reg: Pattern[str], input: str, start_at: int = 0) -> Match[str] | None:
    return reg.search(input, pos=start_at)


def next_match(m: Match[str] | None) -> Match[str] | None:
    if m is None:
        return None

    end = m.end()
    if end > m.start():
        return m.re.search(m.string, pos=end)

    # search clamps pos to the end of the string, so the same empty match would be found again
    if end >= len(m.string):
        return None

    return m.re.search(m.string, pos=end + 1)


def match_or_empty(reg: Pattern[str], input: str, start_at: int = 0) -> Match[str] | EmptyMatch:
    return reg.search(input, pos=start_at) or EMPTY_MATCH


def next_match_or_empty(m: Match[str] | EmptyMatch) -> Match[str] | EmptyMatch:
    if isinstance(m, EmptyMatch):
        return m

    return next_match(m) or EMPTY_MATCH


def matches(reg: Pattern[str], input: str, start_at: int = 0) -> MatchCollection:
    return MatchCollection(list(reg.finditer(input, pos=start_at)))


def _iter_matches(reg: Pattern[str], input: str, start_at: int) -> Iterator[Match[str]]:
    m = reg.search(input, pos=start_at)
    while m is not None:
        yield m
        m = next_match(m)


# Python only differs from .NET when a match starts where an empty match was just found
def _has_dotnet_starts(found: list[Match[str]]) -> bool:
    return len(set(map(Match.start, found))) == len(found)


@lru_cache(maxsize=512)
def _never_matches_empty(reg: Pattern[str]) -> bool:
    if _parser is None:
        return False
    try:
        return _parser.parse(reg.pattern, reg.flags).getwidth()[0] > 0
    except Exception:
        return False


def _has_dotnet_matches(reg: Pattern[str], input: str) -> bool:
    if reg.groups == 0 and reg.match("") is None and "" not in reg.findall(input):
        return True

    return _has_dotnet_starts(list(reg.finditer(input)))


def _is_native(reg: Pattern[str], input: str, count: int | None, offset: int) -> bool:
    if offset != 0 or count == 0:
        return False
    if _never_matches_empty(reg):
        return True
    return count is None and _has_dotnet_matches(reg, input)


def _find_matches(reg: Pattern[str], input: str, start_at: int, count: int | None = None) -> list[Match[str]]:
    found = list(islice(reg.finditer(input, start_at), count))
    if _never_matches_empty(reg) or _has_dotnet_starts(found):
        return found
    return list(islice(_iter_matches(reg, input, start_at), count))


def matches_from(reg: Pattern[str], input: str, start_at: int = 0) -> MatchCollection:
    return MatchCollection(_find_matches(reg, input, start_at))


def is_match(reg: Pattern[str], input: str, start_at: int = 0) -> bool:
    return reg.search(input, pos=start_at) is not None


def groups(m: Match[str] | None) -> GroupCollection:
    if m is None:
        return GroupCollection(named_groups={}, groups=[])

    named_groups: dict[str, str | None] = m.groupdict()

    # .NET adds the whole capture as group 0
    groups_: list[str | None] = [m.group(0), *m.groups()]

    return GroupCollection(named_groups=named_groups, groups=groups_)


def match_groups(m: Match[str] | EmptyMatch) -> GroupCollection:
    if isinstance(m, EmptyMatch):
        return GroupCollection(named_groups={}, groups=[None])

    return GroupCollection(named_groups=m.groupdict(), groups=[m.group(0), *m.groups()])


def group_success(g: str | Match[str] | EmptyMatch | None) -> bool:
    return g is not None and g is not EMPTY_MATCH


def group_value(g: str | Match[str] | EmptyMatch | None) -> str:
    if isinstance(g, str):
        return g
    if isinstance(g, Match):
        return g.group(0)
    return ""


def options(reg: Pattern[str]) -> int:
    options = 256  # ECMAScript
    if reg.flags & re.IGNORECASE:
        options |= 1
    if reg.flags & re.MULTILINE:
        options |= 2
    if reg.flags & re.S:
        options |= 16
    return options


def replace(
    reg: str | Pattern[str],
    input: str,
    replacement: str | MatchEvaluator,
    limit: int | None = None,
    offset: int = 0,
) -> str:
    if isinstance(replacement, str):
        # Rewrite placeholders from .NET e.g $1 to \g<1> Python syntax
        replacement = re.sub(pattern=r"\$(\d+)", repl=r"\\g<\g<1>>", string=replacement)

    if isinstance(reg, str):
        return re.sub(input, replacement, reg, count=limit or 0)
    else:
        return input[:offset] + reg.sub(replacement, input[offset:], count=limit or 0)


def split(
    reg: str | Pattern[str],
    input: str,
    limit: int | None = None,
    offset: int = 0,
) -> Array[str]:
    if isinstance(reg, str):
        result = re.split(input, reg, maxsplit=limit or 0)
    else:
        input = input[offset:]
        result = reg.split(input, maxsplit=limit or 0)[:limit]
    return Array[str](result)


_SUBSTITUTION = re.compile(r"\$(?:([0-9]+)|\{([0-9]+)\}|\{(\w+)\}|([$&`'+_]))")
_BEFORE_MATCH = -1
_AFTER_MATCH = -2
_WHOLE_INPUT = -3


def _parse_substitution(reg: Pattern[str], token: Match[str]) -> str | int:
    number = token.group(1) or token.group(2)
    if number is not None:
        group = int(number)
    elif token.group(3) is not None:
        group = reg.groupindex.get(token.group(3))
    else:
        match token.group(4):
            case "$":
                return "$"
            case "`":
                return _BEFORE_MATCH
            case "'":
                return _AFTER_MATCH
            case "_":
                return _WHOLE_INPUT
            case "+":
                return reg.groups
            case _:
                return 0

    return token.group(0) if group is None or group > reg.groups else group


def _parse_replacement(reg: Pattern[str], replacement: str) -> list[str | int]:
    parts: list[str | int] = []
    prev = 0
    for token in _SUBSTITUTION.finditer(replacement):
        parts.append(replacement[prev : token.start()])
        parts.append(_parse_substitution(reg, token))
        prev = token.end()
    parts.append(replacement[prev:])
    return parts


def _expand_part(m: Match[str], part: str | int) -> str:
    if isinstance(part, str):
        return part
    if part >= 0:
        return m.group(part) or ""
    if part == _BEFORE_MATCH:
        return m.string[: m.start()]
    if part == _AFTER_MATCH:
        return m.string[m.end() :]
    return m.string


def _to_python_replacement(reg: Pattern[str], replacement: str) -> str | MatchEvaluator:
    parts = _parse_replacement(reg, replacement) if "$" in replacement else [replacement]
    if all(isinstance(part, str) or part >= 0 for part in parts):
        return "".join(part.replace("\\", "\\\\") if isinstance(part, str) else f"\\g<{part}>" for part in parts)

    return lambda m: "".join(_expand_part(m, part) for part in parts)


def _count_out_of_range() -> Exception:
    # fable_library.system is compiled from System.fs, and src/fable-library-py/tests runs without it
    from .system import ArgumentOutOfRangeException__ctor_Z384F8060  # noqa: PLC0415

    return ArgumentOutOfRangeException__ctor_Z384F8060("count", "Count cannot be less than -1.")


def replace_from(
    reg: Pattern[str],
    input: str,
    replacement: str | MatchEvaluator,
    limit: int | None = None,
    offset: int = 0,
) -> str:
    if limit is not None and limit < -1:
        raise _count_out_of_range()

    if isinstance(replacement, str):
        replacement = _to_python_replacement(reg, replacement)

    count = None if limit is None or limit < 0 else limit
    if _is_native(reg, input, count, offset):
        return reg.sub(replacement, input, count or 0)

    evaluator = methodcaller("expand", replacement) if isinstance(replacement, str) else replacement
    parts: list[str] = []
    prev = 0
    for m in _find_matches(reg, input, offset, count):
        parts.append(input[prev : m.start()])
        parts.append(evaluator(m) or "")
        prev = m.end()
    parts.append(input[prev:])
    return "".join(parts)


def split_from(
    reg: Pattern[str],
    input: str,
    limit: int | None = None,
    offset: int = 0,
) -> Array[str]:
    if limit is not None and limit < 0:
        raise _count_out_of_range()

    count = limit - 1 if limit else None
    if _is_native(reg, input, count, offset):
        parts = reg.split(input, count or 0)
        return Array[str](parts if reg.groups == 0 else [part for part in parts if part is not None])

    parts: list[str] = []
    prev = 0
    for m in _find_matches(reg, input, offset, count):
        parts.append(input[prev : m.start()])
        if reg.groups:
            parts.extend(group for group in m.groups() if group is not None)
        prev = m.end()
    parts.append(input[prev:])
    return Array[str](parts)


def get_item(groups: GroupCollection, index: str | int) -> str | None:
    return groups[index]


def get_group(groups: GroupCollection, index: str | int) -> str | None:
    if isinstance(index, str):
        return groups.named_groups.get(index)

    return groups.groups[index] if 0 <= index < len(groups.groups) else None


__all__ = [
    "EMPTY_MATCH",
    "EmptyMatch",
    "MatchCollection",
    "create",
    "create_unicode",
    "escape",
    "get_group",
    "get_item",
    "group_success",
    "group_value",
    "groups",
    "is_match",
    "match",
    "match_groups",
    "match_or_empty",
    "matches",
    "matches_from",
    "next_match",
    "next_match_or_empty",
    "options",
    "replace",
    "replace_from",
    "split",
    "split_from",
    "unescape",
]
