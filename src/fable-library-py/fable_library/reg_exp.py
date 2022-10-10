import re

from typing import (
    Callable,
    Iterator,
    List,
    Match,
    Optional,
    Pattern,
    Union,
    Dict,
)


MatchEvaluator = Callable[[Match[str]], str]


class GroupCollection:
    def __init__(self, groups: List[str], named_groups: Dict[str, str]) -> None:
        self.named_groups = named_groups
        self.groups = groups

    def values(self) -> List[str]:
        return list(self.groups)

    def __len__(self) -> int:
        return len(self.groups)

    def __getitem__(self, key: Union[int, str]) -> Optional[str]:
        if isinstance(key, int):
            return self.groups[key]
        else:
            return self.named_groups.get(key)

    def __iter__(self) -> Iterator[str]:
        return iter(self.groups)


def _options_to_flags(options: int) -> int:
    """Convert .NET options to Python flags."""
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
    pattern = pattern.replace("?<", "?P<")
    return re.compile(pattern, flags=flags)


def escape(string: str) -> str:
    return re.escape(string)


def unescape(string: str) -> str:
    return re.sub(r"\\(.)", r"\1", string)


def match(
    reg: Union[Pattern[str], str], input: str, start_at: int = 0
) -> Optional[Match[str]]:
    if isinstance(reg, str):
        flags = _options_to_flags(start_at)
        return re.search(input, reg, flags)
    return reg.search(input, pos=start_at)


def matches(reg: Pattern[str], input: str, start_at: int = 0) -> List[Match[str]]:
    if isinstance(reg, str):
        flags = _options_to_flags(start_at)
        input = input.replace("?<", "?P<")
        return list(re.finditer(input, reg, flags=flags))

    return list(reg.finditer(input, pos=start_at))


def is_match(reg: Union[Pattern[str], str], input: str, start_at: int = 0) -> bool:
    if isinstance(reg, str):
        # Note: input is the pattern here
        flags = _options_to_flags(start_at)
        return re.search(input, reg, flags=flags) is not None

    return reg.search(input, pos=start_at) is not None


def groups(m: Match[str]) -> GroupCollection:
    named_groups: Dict[str, str] = m.groupdict()

    # .NET adds the whole capture as group 0
    groups_ = [m.group(0), *m.groups()]

    return GroupCollection(named_groups=named_groups, groups=groups_)


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
    reg: Union[str, Pattern[str]],
    input: str,
    replacement: Union[str, MatchEvaluator],
    limit: Optional[int] = None,
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
    reg: Union[str, Pattern[str]],
    input: str,
    limit: Optional[int] = None,
    offset: int = 0,
) -> List[str]:
    if isinstance(reg, str):
        return re.split(input, reg, maxsplit=limit or 0)

    input = input[offset:]
    return reg.split(input, maxsplit=limit or 0)[:limit]


def get_item(groups: GroupCollection, index: Union[str, int]) -> Optional[str]:
    return groups[index]


__all__ = [
    "escape",
    "is_match",
    "match",
    "matches",
    "create",
    "options",
    "replace",
    "split",
    "unescape",
    "groups",
    "get_item",
]
