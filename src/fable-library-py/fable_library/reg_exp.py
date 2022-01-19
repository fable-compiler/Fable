import re
from typing import Pattern, Union, Optional, Callable, Any, Match, List

MatchEvaluator = Callable[[Any], str]


def _options_to_flags(options: int) -> int:
    """Convert .NET options to Python flags."""
    flags = 256
    if options & 1:
        flags |= re.IGNORECASE
    if options & 2:
        flags |= re.MULTILINE
    return flags


def create(pattern: str, options: int = 0) -> Pattern[str]:
    # raw_pattern = pattern.encode("unicode_escape").decode("utf-8")
    flags = _options_to_flags(options)
    return re.compile(pattern, flags=flags)


def escape(string: str) -> str:
    return re.escape(string)


def unescape(string: str) -> str:
    return re.sub(r"\\(.)", r"\1", string)


def match(reg: Pattern[str], input: str, start_at: int = 0) -> Optional[Match[str]]:
    return reg.search(input, pos=start_at)


def matches(reg: Pattern[str], input: str, start_at: int = 0) -> List[Match[str]]:
    return reg.findall(input, pos=start_at)


def is_match(reg: Union[Pattern[str], str], input: str, start_at: int = 0) -> bool:
    if isinstance(reg, str):
        # Note: input is the pattern here
        flags = _options_to_flags(start_at)
        return re.search(input, reg, flags=flags) is not None

    return reg.search(input, pos=start_at) is not None


def options(reg: Pattern[str]) -> int:
    options = 256  # ECMAScript
    if reg.flags & re.IGNORECASE:
        options |= 1
    if reg.flags & re.MULTILINE:
        options |= 2
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
        return re.sub(reg, replacement, input)
    else:
        return reg.sub(replacement, input)


__all__ = ["escape", "is_match", "create", "replace"]
