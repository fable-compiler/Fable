"""ABC base classes for F# types in Python.

These base classes provide Python dunder methods by delegating to F# methods:
- DisposableBase: __enter__, __exit__ from Dispose()
- EnumerableBase: __iter__ from GetEnumerator()
- EnumeratorBase: __iter__, __next__ from MoveNext()/Current
- StringableBase: __str__, __repr__ from ToString()
- EquatableBase: __eq__, __ne__ from Equals()
- ComparableBase: __lt__, __le__, __gt__, __ge__ from CompareTo()
- HashableBase: __hash__ from GetHashCode()
- SizedBase: __len__ from Count property

Note: For Mapping and Set base classes the compiler now generates dunders directly and
inherits from collections.abc classes.

Usage:
    Classes should inherit from these bases and implement the corresponding F# method.
    Use the Py marker interfaces in F# to have the compiler automatically add these base
    classes.
"""

from __future__ import annotations

from abc import ABC, ABCMeta, abstractmethod
from collections.abc import Iterator
from types import TracebackType
from typing import TYPE_CHECKING, Any, Literal, cast


if TYPE_CHECKING:
    from .protocols import IEnumerator


class ObjectDisposedException(Exception):
    """Exception thrown when accessing a disposed object."""

    def __init__(self) -> None:
        super().__init__("Cannot access a disposed object")


class DisposableBase(ABC, metaclass=ABCMeta):
    """ABC base class for IDisposable types.

    Provides context manager support (__enter__/__exit__) for classes that
    implement the IDisposable pattern. Inherit from this class to get
    automatic context manager support.
    """

    __slots__ = ()

    @abstractmethod
    def Dispose(self) -> None:
        """Dispose of resources. Must be implemented by subclasses."""
        raise NotImplementedError

    def __enter__(self) -> Any:
        """Enter context management."""
        return self

    def __exit__(
        self,
        exctype: type[BaseException] | None,
        excinst: BaseException | None,
        exctb: TracebackType | None,
    ) -> Literal[False]:
        """Exit context management."""
        self.Dispose()
        return False


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


class EnumeratorBase[T](ABC):
    """ABC base class for IEnumerator<T> types.

    Provides Python iterator protocol support (__iter__/__next__) for classes
    that implement the IEnumerator pattern.

    Note: This does NOT include Dispose. For types that need context manager
    support, use Py.ContextManager marker or add DisposableBase separately.
    """

    __slots__ = ()

    @abstractmethod
    def System_Collections_Generic_IEnumerator_1_get_Current(self) -> T:
        """Get the current element (generic IEnumerator<T>.Current)."""
        raise NotImplementedError

    def System_Collections_IEnumerator_get_Current(self) -> Any:
        """Get the current element (non-generic IEnumerator.Current)."""
        return self.System_Collections_Generic_IEnumerator_1_get_Current()

    @abstractmethod
    def System_Collections_IEnumerator_MoveNext(self) -> bool:
        """Move to the next element. Returns False if no more elements."""
        raise NotImplementedError

    @abstractmethod
    def System_Collections_IEnumerator_Reset(self) -> None:
        """Reset the enumerator."""
        raise NotImplementedError

    def __iter__(self) -> Iterator[T]:
        return self

    def __next__(self) -> T:
        if not self.System_Collections_IEnumerator_MoveNext():
            raise StopIteration
        return self.System_Collections_Generic_IEnumerator_1_get_Current()


class EnumerableBase[T](ABC):
    """ABC base class for IEnumerable<T> types.

    Provides Python iterable protocol support (__iter__) for classes that
    implement the IEnumerable pattern.
    """

    __slots__ = ()

    @abstractmethod
    def GetEnumerator(self) -> IEnumerator[T]:
        """Get an enumerator for the collection."""
        raise NotImplementedError

    def __iter__(self) -> Iterator[T]:
        # At runtime, GetEnumerator() returns an object that also inherits from
        # EnumeratorBase (added by the compiler), which provides __iter__ and __next__.
        # The cast tells the type checker this is safe.
        return cast(Iterator[T], self.GetEnumerator())


__all__ = [
    "ComparableBase",
    "DisposableBase",
    "EnumerableBase",
    "EnumeratorBase",
    "EquatableBase",
    "HashableBase",
    "ObjectDisposedException",
    "SizedBase",
    "StringableBase",
]
