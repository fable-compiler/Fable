"""ABC base classes for F# types in Python.

These base classes provide Python dunder methods by delegating to F# methods:
- StringableBase: __str__, __repr__ from ToString()
- EquatableBase: __eq__, __ne__ from Equals()
- ComparableBase: __lt__, __le__, __gt__, __ge__ from CompareTo()
- HashableBase: __hash__ from GetHashCode()
- SizedBase: __len__ from Count property

Usage:
    Classes should inherit from these bases and implement the corresponding F# method.
    Use the Py.Stringable, Py.Equatable, Py.Comparable, Py.Hashable, Py.Sized marker
    interfaces in F# to have the compiler automatically add these base classes.
"""

from __future__ import annotations

from abc import ABC, abstractmethod


class StringableBase:
    """Base class for types with ToString.

    Provides Python string protocol (__str__, __repr__) for classes that
    override ToString. Not an ABC - relies on duck typing for ToString.
    """

    __slots__ = ()

    def __str__(self) -> str:
        return self.ToString()  # type: ignore[attr-defined]

    def __repr__(self) -> str:
        return self.ToString()  # type: ignore[attr-defined]


class EquatableBase(ABC):
    """ABC base class for equatable types.

    Provides Python equality protocol (__eq__) for classes that implement
    the IEquatable pattern.
    """

    __slots__ = ()

    @abstractmethod
    def Equals(self, other: object) -> bool:
        """Check equality with another object."""
        raise NotImplementedError

    def __eq__(self, other: object) -> bool:
        return self.Equals(other)

    def __ne__(self, other: object) -> bool:
        return not self.Equals(other)


class ComparableBase(ABC):
    """ABC base class for comparable types.

    Provides Python comparison protocol (__lt__, __le__, __gt__, __ge__)
    for classes that implement the IComparable pattern.
    """

    __slots__ = ()

    @abstractmethod
    def CompareTo(self, other: object) -> int:
        """Compare to another object. Returns <0, 0, or >0."""
        raise NotImplementedError

    def __lt__(self, other: object) -> bool:
        return self.CompareTo(other) < 0

    def __le__(self, other: object) -> bool:
        return self.CompareTo(other) <= 0

    def __gt__(self, other: object) -> bool:
        return self.CompareTo(other) > 0

    def __ge__(self, other: object) -> bool:
        return self.CompareTo(other) >= 0


class HashableBase(ABC):
    """ABC base class for hashable types.

    Provides Python hash protocol (__hash__) for classes that implement
    GetHashCode. Handles the type conversion from int32 to int.
    """

    __slots__ = ()

    @abstractmethod
    def GetHashCode(self) -> int:
        """Get the hash code. Must be implemented by subclasses."""
        raise NotImplementedError

    def __hash__(self) -> int:
        return int(self.GetHashCode())


class SizedBase(ABC):
    """ABC base class for sized types.

    Provides Python __len__ protocol for classes that have a Count property.
    """

    __slots__ = ()

    @property
    @abstractmethod
    def Count(self) -> int:
        """Get the number of items. Must be implemented by subclasses."""
        raise NotImplementedError

    def __len__(self) -> int:
        return self.Count


__all__ = [
    "ComparableBase",
    "EquatableBase",
    "HashableBase",
    "SizedBase",
    "StringableBase",
]
