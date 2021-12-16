import re


def escape(string: str):
    return re.escape(string)


def is_match(string: str, pattern: str) -> bool:
    return re.match(pattern, string) is not None


__all__ = ["escape", "is_match"]
