"""Protocol definitions for Fable Python runtime.

This module contains PURE protocol (interface) definitions used by the Fable
runtime library. These are for type hints and structural subtyping only.

For classes that need to inherit with implementations (context managers, etc.),
use the ABC base classes in bases.py (ContextManagerBase, IteratorBase, etc.).

Protocols are defined here to avoid circular imports between modules like
util.py and core/array.pyi.
"""

from __future__ import annotations

from abc import abstractmethod
from collections.abc import Iterable
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


if TYPE_CHECKING:
    from .core import int32


# =============================================================================
# Basic Protocols
# =============================================================================


class SupportsLessThan(Protocol):
    @abstractmethod
    def __lt__(self, __other: Any) -> bool:
        raise NotImplementedError


class IEquatable(Protocol):
    """Protocol for equatable types (pure protocol for type hints).

    Note: Does NOT include __eq__, __hash__. For Python equality support,
    use Py.Equatable and Py.Hashable marker interfaces which add
    EquatableBase and HashableBase.
    """

    def Equals(self, other: Any, /) -> bool: ...
    def GetHashCode(self) -> int: ...


class HashCode(Protocol):
    def GetHashCode(self) -> int32: ...


# =============================================================================
# Comparable Protocols
# =============================================================================


class IComparable(IEquatable, Protocol):
    """Protocol for comparable types (pure protocol for type hints).

    Note: Does NOT include __lt__, __le__, __gt__, __ge__. For Python
    comparison support, use Py.Comparable marker interface which adds
    ComparableBase.
    """

    @abstractmethod
    def CompareTo(self, other: Any) -> int:
        raise NotImplementedError


class IComparable_1[T_in](IEquatable, Protocol):
    """Protocol for generic comparable types (pure protocol for type hints).

    Note: Does NOT include __lt__, __le__, __gt__, __ge__. For Python
    comparison support, use Py.Comparable marker interface which adds
    ComparableBase.
    """

    @abstractmethod
    def CompareTo(self, other: T_in) -> int:
        raise NotImplementedError


# =============================================================================
# Comparer Protocols
# =============================================================================


class IComparer(Protocol):
    """Defines a method that a type implements to compare two objects.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.icomparer
    """

    @abstractmethod
    def Compare(self, x: Any, y: Any, /) -> int32:
        raise NotImplementedError


class IComparer_1[T_in](Protocol):
    """Defines a method that a type implements to compare two objects.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.icomparer-1
    """

    @abstractmethod
    def Compare(self, x: T_in, y: T_in, /) -> int32:
        raise NotImplementedError


# =============================================================================
# Equality Comparer Protocols
# =============================================================================


class IEqualityComparer(Protocol):
    """Defines methods to support the comparison of objects for equality.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.iequalitycomparer
    """

    @abstractmethod
    def Equals(self, x: Any, y: Any, /) -> bool:
        raise NotImplementedError

    @abstractmethod
    def GetHashCode(self, x: Any, /) -> int32:
        raise NotImplementedError


class IEqualityComparer_1[T_in](Protocol):
    """Defines methods to support the comparison of objects for equality.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.iequalitycomparer-1
    """

    @abstractmethod
    def Equals(self, x: T_in, y: T_in, /) -> bool:
        raise NotImplementedError

    @abstractmethod
    def GetHashCode(self, x: T_in, /) -> int32:
        raise NotImplementedError


# =============================================================================
# Structural Comparison Protocols
# =============================================================================


class IStructuralEquatable(Protocol):
    """Protocol for structural equality comparison.

    Note: Does NOT include __hash__. For Python hash support, use
    Py.Hashable marker interface which adds HashableBase.
    """

    @abstractmethod
    def Equals(self, other: Any, comparer: IEqualityComparer) -> bool:
        raise NotImplementedError

    @abstractmethod
    def GetHashCode(self, comparer: IEqualityComparer) -> int32:
        raise NotImplementedError


class IStructuralComparable(Protocol):
    """Protocol for structural comparison."""

    @abstractmethod
    def CompareTo(self, other: Any, comparer: IComparer) -> int32:
        raise NotImplementedError


# =============================================================================
# Generic Adder/Averager Protocols (for Array.sum, Array.average, etc.)
# =============================================================================


class IGenericAdder[T](Protocol):
    """Protocol for types that support addition with a zero value."""

    def GetZero(self, __unit: Any = ...) -> T: ...
    def Add(self, x: T, y: T, /) -> T: ...


class IGenericAverager[T](Protocol):
    """Protocol for types that support averaging (add, divide by count)."""

    def GetZero(self, __unit: Any = ...) -> T: ...
    def Add(self, x: T, y: T, /) -> T: ...
    def DivideByInt(self, x: T, i: int32, /) -> T: ...


# =============================================================================
# Disposable Protocol
# =============================================================================


@runtime_checkable
class IDisposable(Protocol):
    """IDisposable protocol (pure protocol for type hints).

    This is a pure protocol for structural subtyping. For classes that need
    to inherit with context manager implementations, use ContextManagerBase
    from bases.py instead.

    Note: This protocol is @runtime_checkable to support isinstance() checks
    in utility functions like is_disposable().

    Note: Does NOT include __enter__, __exit__. For Python context manager
    support, use Py.ContextManager marker interface which adds ContextManagerBase.
    """

    __slots__ = ()

    def Dispose(self) -> None: ...


# =============================================================================
# Enumerator Protocol
# =============================================================================


class IEnumerator[T](Protocol):
    """Protocol for enumerators (pure protocol for type hints).

    This is a pure protocol for structural subtyping. For classes that need
    to inherit with iterator/context manager implementations, use IteratorBase
    and ContextManagerBase from bases.py instead.

    Note: Does NOT include __iter__, __next__, __enter__, __exit__.
    For Python iterator/context manager support, use Py marker interfaces.
    """

    __slots__ = ()

    # Generic IEnumerator<T>.Current
    def System_Collections_Generic_IEnumerator_1_get_Current(self) -> T: ...

    # Non-generic IEnumerator.Current
    def System_Collections_IEnumerator_get_Current(self) -> Any: ...

    def System_Collections_IEnumerator_MoveNext(self) -> bool: ...
    def System_Collections_IEnumerator_Reset(self) -> None: ...
    def Dispose(self) -> None: ...


# =============================================================================
# Enumerable Protocols
# =============================================================================

# Note: UNIT is not imported here to avoid circular dependency.
# The __unit parameter uses Any default which works the same way.


class IEnumerable(Protocol):
    """Protocol for enumerable collections (pure protocol for type hints).

    Note: Does NOT include __iter__. For Python iteration support, use
    Py.Iterable marker interface which adds IterableBase.
    """

    __slots__ = ()

    def GetEnumerator(self, __unit: Any = None) -> IEnumerator[Any]: ...


class IEnumerable_1[T](Protocol):
    """Protocol for generic enumerable collections (pure protocol for type hints).

    Note: Does NOT include __iter__. For Python iteration support, use
    Py.Iterable marker interface which adds IterableBase.
    """

    __slots__ = ()

    def GetEnumerator(self, __unit: Any = None) -> IEnumerator[T]: ...


# =============================================================================
# Collection Protocols
# =============================================================================


class ICollection[T](IEnumerable_1[T], Protocol): ...


class IDictionary[Key, Value](ICollection[tuple[Key, Value]], Protocol):
    @abstractmethod
    def keys(self) -> IEnumerable_1[Key]: ...

    def values(self) -> IEnumerable_1[Value]: ...


# =============================================================================
# .NET Collection Protocols (F# interface contracts - NO dunders)
# =============================================================================
# These protocols define F# interface method signatures. They do NOT include
# Python dunder methods. For Python interop (len, in, [], etc.), use the
# corresponding ABC base classes from bases.py via Py marker interfaces.


class IReadOnlyCollection_1[T](IEnumerable_1[T], Protocol):
    """Protocol for IReadOnlyCollection<T> (.NET).

    Provides read-only access to a collection with a count.
    For Python __len__ support, use Py.Sized marker → SizedBase.
    """

    @property
    def Count(self) -> int:
        """Get the number of items in the collection."""
        ...


class IReadOnlySet_1[T](IReadOnlyCollection_1[T], Protocol):
    """Protocol for IReadOnlySet<T> (.NET 5+).

    Provides read-only set operations.
    For Python __contains__/__iter__/__len__, use Py.Set marker → SetBase.
    """

    def Contains(self, item: T, /) -> bool:
        """Check if item exists in the set."""
        ...

    def IsProperSubsetOf(self, other: IEnumerable_1[T], /) -> bool:
        """Check if this set is a proper subset of other."""
        ...

    def IsProperSupersetOf(self, other: IEnumerable_1[T], /) -> bool:
        """Check if this set is a proper superset of other."""
        ...

    def IsSubsetOf(self, other: IEnumerable_1[T], /) -> bool:
        """Check if this set is a subset of other."""
        ...

    def IsSupersetOf(self, other: IEnumerable_1[T], /) -> bool:
        """Check if this set is a superset of other."""
        ...

    def Overlaps(self, other: IEnumerable_1[T], /) -> bool:
        """Check if this set overlaps with other."""
        ...

    def SetEquals(self, other: IEnumerable_1[T], /) -> bool:
        """Check if this set equals other."""
        ...


class ISet_1[T](IReadOnlySet_1[T], Protocol):
    """Protocol for ISet<T> (.NET).

    Provides mutable set operations.
    For Python add/discard/remove/pop/clear, use Py.MutableSet marker → MutableSetBase.
    """

    def Add(self, item: T, /) -> bool:
        """Add item to set. Returns True if item was added."""
        ...

    def Remove(self, item: T, /) -> bool:
        """Remove item from set. Returns True if item was removed."""
        ...

    def Clear(self) -> None:
        """Remove all items from set."""
        ...

    def ExceptWith(self, other: IEnumerable_1[T], /) -> None:
        """Remove all items in other from this set."""
        ...

    def IntersectWith(self, other: IEnumerable_1[T], /) -> None:
        """Keep only items that are in both this set and other."""
        ...

    def SymmetricExceptWith(self, other: IEnumerable_1[T], /) -> None:
        """Keep only items that are in this set or other, but not both."""
        ...

    def UnionWith(self, other: IEnumerable_1[T], /) -> None:
        """Add all items from other to this set."""
        ...


class IReadOnlyDictionary_2[K, V](IReadOnlyCollection_1[tuple[K, V]], Protocol):
    """Protocol for IReadOnlyDictionary<K, V> (.NET).

    Provides read-only dictionary access.
    For Python __getitem__/__contains__/__iter__/__len__/keys/values/items/get,
    use Py.Mapping marker → MappingBase.
    """

    def ContainsKey(self, key: K, /) -> bool:
        """Check if key exists in dictionary."""
        ...

    def TryGetValue(self, key: K, /) -> tuple[bool, V]:
        """Try to get value for key. Returns (found, value)."""
        ...

    def get_Item(self, key: K, /) -> V:
        """Get value for key. Raises KeyError if not found."""
        ...

    @property
    def Keys(self) -> IEnumerable_1[K]:
        """Get enumerable of keys."""
        ...

    @property
    def Values(self) -> IEnumerable_1[V]:
        """Get enumerable of values."""
        ...


class IDictionary_2[K, V](IReadOnlyDictionary_2[K, V], Protocol):
    """Protocol for IDictionary<K, V> (.NET).

    Provides mutable dictionary operations.
    For Python __setitem__/__delitem__/clear/pop/popitem/setdefault,
    use Py.MutableMapping marker → MutableMappingBase.
    """

    def Add(self, key: K, value: V, /) -> None:
        """Add key-value pair. Raises if key exists."""
        ...

    def Remove(self, key: K, /) -> bool:
        """Remove key. Returns True if key was removed."""
        ...

    def Clear(self) -> None:
        """Remove all items."""
        ...

    def set_Item(self, key: K, value: V, /) -> None:
        """Set value for key (add or update)."""
        ...


# =============================================================================
# Convenience Aliases (for common usage patterns)
# =============================================================================


class IReadOnlyDictionary[K, V](Protocol):
    """Minimal protocol for read-only dictionary access.

    This is a simplified protocol for functions that just need keys() and item access.
    For the full .NET IReadOnlyDictionary, use IReadOnlyDictionary_2.
    """

    def keys(self, __unit: Any = None) -> Iterable[K]:
        """Return an iterable of keys."""
        ...

    def __getitem__(self, key: K, /) -> V:
        """Get the value for a key."""
        ...
