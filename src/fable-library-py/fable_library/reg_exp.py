import re

from typing import Any, Callable, List, Match, Optional, Pattern, Union


MatchEvaluator = Callable[[Any], str]


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
        return re.findall(input, reg, flags=flags)

    return reg.findall(input, pos=start_at)


def is_match(reg: Union[Pattern[str], str], input: str, start_at: int = 0) -> bool:
    if isinstance(reg, str):
        # Note: input is the pattern here
        flags = _options_to_flags(start_at)
        return re.search(input, reg, flags=flags) is not None

    return reg.search(input, pos=start_at) is not None


def groups(m: Match[str]) -> List[str]:
    # .NET adds the whole capture as group 0
    g = list(m.groups())
    g.insert(0, m.string)
    return g


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
        print("reg, replacement, input=", reg, replacement, input)
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
    print("reg, input: ", reg, input, limit)
    return reg.split(input, maxsplit=limit or 0)[:limit]


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
]
