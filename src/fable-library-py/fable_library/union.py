"""F# Union (Discriminated Union) type for Fable Python runtime."""

from __future__ import annotations

from abc import abstractmethod
from typing import Any

from .array_ import Array
from .bases import ComparableBase, EquatableBase, HashableBase, StringableBase
from .core import int32
from .protocols import IComparable
from .util import compare


class Union(StringableBase, EquatableBase, ComparableBase, HashableBase, IComparable):
    """Base class for F# discriminated unions.

    Inherits from ABC base classes that provide Python dunder methods:
    - StringableBase: __str__, __repr__ from ToString
    - EquatableBase: __eq__, __ne__ from Equals
    - ComparableBase: __lt__, __le__, __gt__, __ge__ from CompareTo
    - HashableBase: __hash__ from GetHashCode
    """

    __slots__: list[str] = ["fields", "tag"]

    tag: int32
    fields: Array[Any]

    def __init__(self) -> None:
        self.fields = Array[Any]()

    @staticmethod
    @abstractmethod
    def cases() -> list[str]: ...

    @property
    def name(self) -> str:
        return self.cases()[self.tag]

    # -------------------------------------------------------------------------
    # String representation (used by StringableBase)
    # -------------------------------------------------------------------------

    def ToString(self) -> str:
        if not len(self.fields):
            return self.name

        def to_string(value: Any) -> str:
            if isinstance(value, str):
                return f'"{value}"'
            return str(value)

        fields = ""
        with_parens = True
        if len(self.fields) == 1:
            field = to_string(self.fields[0])
            with_parens = field.find(" ") >= 0
            fields = field
        else:
            fields = ", ".join(map(to_string, self.fields))

        return self.name + (" (" if with_parens else " ") + fields + (")" if with_parens else "")

    # -------------------------------------------------------------------------
    # IEquatable - Equality (used by EquatableBase)
    # -------------------------------------------------------------------------

    def Equals(self, other: Any) -> bool:
        if self is other:
            return True

        if not isinstance(other, Union):
            return False

        # Different objects are not equal even with same structure
        if self.__class__ != other.__class__:
            return False

        if self.tag == other.tag:
            return self.fields == other.fields

        return False

    # -------------------------------------------------------------------------
    # Hashable (HashableBase provides __hash__ from GetHashCode)
    # -------------------------------------------------------------------------

    def GetHashCode(self) -> int32:
        return int32(hash((self.tag, *self.fields)))

    # -------------------------------------------------------------------------
    # IComparable - Comparison (used by ComparableBase)
    # -------------------------------------------------------------------------

    def CompareTo(self, other: Any) -> int:
        if self.tag == other.tag:
            return compare(self.fields, other.fields)
        return -1 if self.tag < other.tag else 1


__all__ = [
    "Union",
]
