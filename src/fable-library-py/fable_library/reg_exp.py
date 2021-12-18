import re
from typing import Pattern, Union


def escape(string: str) -> str:
    return re.escape(string)


def is_match(string: str, pattern: str) -> bool:
    return re.match(pattern, string) is not None


def replace(reg: Union[str, Pattern], input: str, replacement: str) -> str:
    if isinstance(reg, str):
        return re.sub(input, replacement, reg)
    else:
        return reg.sub(input, replacement)


def create(pattern: str, options: int = 0) -> Pattern:
    return re.compile(pattern)


__all__ = ["escape", "is_match", "create", "replace"]
