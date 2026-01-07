"""F# Exception types for Fable Python runtime."""

from __future__ import annotations

from collections.abc import Iterable
from typing import Any, cast

from .bases import ComparableBase, EquatableBase, HashableBase, StringableBase
from .protocols import IComparable


def _exception_to_string(self: FSharpException) -> str:
    """Convert FSharpException to string representation."""
    if hasattr(self, "__slots__"):
        return "{ " + "\n  ".join(map(lambda slot: slot + " = " + str(getattr(self, slot)), self.__slots__)) + " }"
    else:
        return "{ " + "\n  ".join(map(lambda kv: kv[0] + " = " + str(kv[1]), self.__dict__.items())) + " }"


class ObjectDisposedException(Exception):
    """Exception thrown when accessing a disposed object."""

    def __init__(self) -> None:
        super().__init__("Cannot access a disposed object")


def seq_to_string(self: Iterable[Any]) -> str:
    str = "["

    for count, x in enumerate(self):
        if count == 0:
            str += to_string(x)

        elif count == 100:
            str += "; ..."
            break

        else:
            str += "; " + to_string(x)

    return str + "]"


def to_string(x: object | None, call_stack: int = 0) -> str:
    match x:
        case float() if int(x) == x:
            return str(int(x))
        case bool():
            return str(x).lower()
        case Iterable() if not hasattr(cast(Iterable[Any], x), "__str__"):
            return seq_to_string(cast(Iterable[Any], x))
        case _:
            return str(x)


class FSharpException(StringableBase, EquatableBase, ComparableBase, HashableBase, Exception, IComparable):
    """Base class for F# exceptions.

    Inherits from ABC base classes that provide Python dunder methods:
    - StringableBase: __str__, __repr__ from ToString
    - EquatableBase: __eq__, __ne__ from Equals
    - ComparableBase: __lt__, __le__, __gt__, __ge__ from CompareTo
    - HashableBase: __hash__ from GetHashCode
    """

    def __init__(self) -> None:
        self.Data0: Any = None

    # -------------------------------------------------------------------------
    # String representation (used by StringableBase)
    # -------------------------------------------------------------------------

    def ToString(self) -> str:
        return _exception_to_string(self)

    # -------------------------------------------------------------------------
    # IEquatable - Equality (used by EquatableBase)
    # -------------------------------------------------------------------------

    def Equals(self, other: Any) -> bool:
        if self is other:
            return True

        if other is None:
            return False

        return self.Data0 == other.Data0

    # -------------------------------------------------------------------------
    # Hashable (HashableBase provides __hash__ from GetHashCode)
    # -------------------------------------------------------------------------

    def GetHashCode(self) -> int:
        return hash(self.Data0)

    # -------------------------------------------------------------------------
    # IComparable - Comparison (used by ComparableBase)
    # -------------------------------------------------------------------------

    def CompareTo(self, other: Any) -> int:
        if not isinstance(other, FSharpException):
            return 1  # Non-comparable types sort after
        # Simple comparison for Data0 (avoid circular import with util.compare)
        a, b = self.Data0, other.Data0
        if a is b:
            return 0
        if a is None:
            return -1 if b else 0
        if b is None:
            return 1 if a else 0
        if hasattr(a, "CompareTo") and callable(a.CompareTo):
            return cast(int, a.CompareTo(b))
        return 0 if a == b else (-1 if a < b else 1)


def is_exception(x: Any) -> bool:
    return isinstance(x, Exception)


__all__ = [
    "FSharpException",
    "ObjectDisposedException",
    "is_exception",
    "seq_to_string",
    "to_string",
]
