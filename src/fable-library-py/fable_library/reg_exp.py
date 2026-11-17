from __future__ import annotations

import re
from collections.abc import Callable, Iterator
from re import Match, Pattern

from .array_ import Array
from .protocols import IEnumerator
from .types import IntegerTypes
from .util import UNIT, Enumerator, Unit


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
    "next_match",
    "next_match_or_empty",
    "options",
    "replace",
    "split",
    "unescape",
]
