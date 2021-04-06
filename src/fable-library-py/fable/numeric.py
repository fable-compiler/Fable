from abc import ABC, abstractmethod
from typing import Any, Optional, Union

from .util import IComparable


def to_fixed(x: float, dp: Optional[int] = None) -> str:
    if dp is not None:
        fmt = "{:.%sf}" % dp
        return fmt.format(x)

    return "{}".format(x)


def to_precision(x: float, sd: Optional[int] = None):
    if sd is not None:
        fmt = "{:.%se}" % sd
        return fmt.format(x)

    return "{}".format(x)


def to_exponential(x: float, dp: Optional[int] = None):
    if dp is not None:
        fmt = "{:.%se}" % dp
        return fmt.format(x)

    return "{}".format(x)


def to_hex(x) -> str:
    return "{0:x}".format(x)

def multiply(x: int, y: int):
    return x * y
