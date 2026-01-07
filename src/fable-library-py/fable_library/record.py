"""F# Record type for Fable Python runtime."""

from __future__ import annotations

from .bases import ComparableBase, EquatableBase, HashableBase, StringableBase
from .core import int32
from .protocols import IComparable
from .util import compare, equals


def record_compare_to(self: Record, other: Record) -> int:
    if self is other:
        return 0

    # Check if the record has a custom __eq__ method (not inherited from Record)
    # If so, use it for equality-based comparison instead of field-by-field
    self_eq_method = getattr(type(self), "__eq__", None)
    record_eq_method = getattr(Record, "__eq__", None)

    if self_eq_method and self_eq_method != record_eq_method:
        # Record has custom equality, use it for comparison
        if self == other:
            return 0
        # For custom equality, we can't determine ordering, so use identity comparison
        return -1 if id(self) < id(other) else 1

    # Default field-by-field comparison for records without custom equality
    if hasattr(self, "__dict__") and self.__dict__:
        for name in self.__dict__.keys():
            result = compare(self.__dict__[name], other.__dict__[name])
            if result != 0:
                return result

    elif hasattr(self, "__slots__") and self.__slots__:
        for name in self.__slots__:
            result = compare(getattr(self, name), getattr(other, name))
            if result != 0:
                return result

    return 0


def record_equals[T](self: T, other: T) -> bool:
    if self is other:
        return True

    if self.__class__ != other.__class__:
        return False

    if isinstance(self, Record) and isinstance(other, Record):
        # Check if the record has a custom __eq__ method (not inherited from Record)
        # If so, use it for equality
        self_eq_method = getattr(type(self), "__eq__", None)
        record_eq_method = getattr(Record, "__eq__", None)

        if self_eq_method and self_eq_method != record_eq_method:
            # Record has custom equality, use it
            return self == other

        # Default field-by-field comparison using equals() for proper nested equality
        if hasattr(self, "__dict__") and self.__dict__:
            for name in self.__dict__.keys():
                if not equals(self.__dict__[name], other.__dict__[name]):
                    return False
        elif hasattr(self, "__slots__") and self.__slots__:
            for name in self.__slots__:
                if not equals(getattr(self, name), getattr(other, name)):
                    return False
        return True

    return self == other


def record_to_string(self: Record) -> str:
    if hasattr(self, "__slots__"):
        return "{ " + "\n  ".join(map(lambda slot: slot + " = " + str(getattr(self, slot)), self.__slots__)) + " }"
    else:
        return "{ " + "\n  ".join(map(lambda kv: kv[0] + " = " + str(kv[1]), self.__dict__.items())) + " }"


def record_get_hashcode(self: Record) -> int:
    slots = type(self).__slots__
    return hash(tuple(getattr(self, fixed_field) for fixed_field in slots))


class Record(StringableBase, EquatableBase, ComparableBase, HashableBase, IComparable):
    """Base class for F# records.

    Inherits from ABC base classes that provide Python dunder methods:
    - StringableBase: __str__, __repr__ from ToString
    - EquatableBase: __eq__, __ne__ from Equals
    - ComparableBase: __lt__, __le__, __gt__, __ge__ from CompareTo
    - HashableBase: __hash__ from GetHashCode
    """

    __slots__: list[str] = []

    # -------------------------------------------------------------------------
    # String representation (used by StringableBase)
    # -------------------------------------------------------------------------

    def ToString(self) -> str:
        return record_to_string(self)

    # -------------------------------------------------------------------------
    # IEquatable - Equality (used by EquatableBase)
    # -------------------------------------------------------------------------

    def Equals(self, other: object) -> bool:
        if not isinstance(other, Record):
            return False
        return record_equals(self, other)

    # -------------------------------------------------------------------------
    # Hashable (HashableBase provides __hash__ from GetHashCode)
    # -------------------------------------------------------------------------

    def GetHashCode(self) -> int32:
        return int32(record_get_hashcode(self))

    # -------------------------------------------------------------------------
    # IComparable - Comparison (used by ComparableBase)
    # -------------------------------------------------------------------------

    def CompareTo(self, other: object) -> int:
        if not isinstance(other, Record):
            raise TypeError(f"Cannot compare Record with {type(other).__name__}")
        return record_compare_to(self, other)


__all__ = [
    "Record",
    "record_compare_to",
    "record_equals",
    "record_get_hashcode",
    "record_to_string",
]
