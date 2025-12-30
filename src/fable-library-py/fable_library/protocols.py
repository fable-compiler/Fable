"""Protocol definitions for Fable Python runtime.

This module contains PURE protocol (interface) definitions used by the Fable
runtime library. These are for type hints and structural subtyping only.

For classes that need to inherit with implementations (context managers, etc.),
use the ABC base classes in util.py (DisposableBase, EnumeratorBase, etc.).

Protocols are defined here to avoid circular imports between modules like
util.py and core/array.pyi.
"""

from __future__ import annotations

from abc import abstractmethod
from collections.abc import Iterable, Iterator
from types import TracebackType
from typing import TYPE_CHECKING, Any, Literal, Protocol, Self, runtime_checkable


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
    def __eq__(self, other: Any) -> bool: ...
    def __hash__(self) -> int: ...


class HashCode(Protocol):
    def GetHashCode(self) -> int32: ...


# =============================================================================
# Comparable Protocols
# =============================================================================


class IComparable(IEquatable, Protocol):
    @abstractmethod
    def __cmp__(self, __other: Any) -> int:
        raise NotImplementedError

    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        raise NotImplementedError


class IComparable_1[T_in](IEquatable, Protocol):
    @abstractmethod
    def __cmp__(self, __other: T_in) -> int:
        raise NotImplementedError

    @abstractmethod
    def __lt__(self, other: Any) -> bool:
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
    @abstractmethod
    def Equals(self, other: Any, comparer: IEqualityComparer) -> bool:
        raise NotImplementedError

    @abstractmethod
    def __hash__(self) -> int32:
        raise NotImplementedError


class IStructuralComparable(Protocol):
    @abstractmethod
    def __cmp__(self, other: Any, comparer: IComparer) -> int32:
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
    to inherit with context manager implementations, use DisposableBase from
    util.py instead.

    Note: This protocol is @runtime_checkable to support isinstance() checks
    in utility functions like is_disposable().
    """

    __slots__ = ()

    def Dispose(self) -> None: ...

    def __enter__(self) -> Self: ...

    def __exit__(
        self,
        exctype: type[BaseException] | None,
        excinst: BaseException | None,
        exctb: TracebackType | None,
    ) -> Literal[False]: ...


# =============================================================================
# Enumerator Protocol
# =============================================================================


class IEnumerator[T](IDisposable, Protocol):
    """Protocol for enumerators (pure protocol for type hints).

    This is a pure protocol for structural subtyping. For classes that need
    to inherit with iterator/context manager implementations, use EnumeratorBase
    from util.py instead.
    """

    __slots__ = ()

    # Generic IEnumerator<T>.Current
    def System_Collections_Generic_IEnumerator_1_get_Current(self) -> T: ...

    # Non-generic IEnumerator.Current
    def System_Collections_IEnumerator_get_Current(self) -> Any: ...

    def System_Collections_IEnumerator_MoveNext(self) -> bool: ...
    def System_Collections_IEnumerator_Reset(self) -> None: ...

    def __iter__(self) -> Iterator[T]: ...
    def __next__(self) -> T: ...


# =============================================================================
# Enumerable Protocols
# =============================================================================

# Note: UNIT is not imported here to avoid circular dependency.
# The __unit parameter uses Any default which works the same way.


class IEnumerable(Iterable[Any], Protocol):
    """Protocol for enumerable collections (pure protocol for type hints)."""

    __slots__ = ()

    def GetEnumerator(self, __unit: Any = None) -> IEnumerator[Any]: ...

    def __iter__(self) -> Iterator[Any]: ...


class IEnumerable_1[T](Iterable[T], Protocol):
    """Protocol for generic enumerable collections (pure protocol for type hints)."""

    __slots__ = ()

    def GetEnumerator(self, __unit: Any = None) -> IEnumerator[T]: ...

    def __iter__(self) -> Iterator[T]: ...


# =============================================================================
# Collection Protocols
# =============================================================================


class ICollection[T](IEnumerable_1[T], Protocol): ...


class IDictionary[Key, Value](ICollection[tuple[Key, Value]], Protocol):
    @abstractmethod
    def keys(self) -> IEnumerable_1[Key]: ...

    def values(self) -> IEnumerable_1[Value]: ...


# =============================================================================
# Set and Map Protocols (JS-style interfaces)
# =============================================================================


class ISet[T](Protocol):
    """Protocol for set-like objects (Python-idiomatic version of JS Set interface).

    The Fable compiler transforms JS-style methods to Python idioms:
    - has(k) -> __contains__(k)  (enables `x in set` syntax)
    - delete(k) -> __delitem__(k)  (enables `del set[k]` syntax)

    Parameters are positional-only (/) to avoid name mismatch errors when
    implementing classes use different parameter names (e.g., 'k' vs 'value').
    """

    @property
    def size(self) -> int:
        """Return the number of elements in the set."""
        ...

    def add(self, value: T = ..., /) -> ISet[T]:
        """Add a value to the set. Returns self for chaining."""
        ...

    def clear(self) -> None:
        """Remove all elements from the set."""
        ...

    def __delitem__(self, value: T = ..., /) -> bool:
        """Remove a value from the set. Returns True if value was present."""
        ...

    def __contains__(self, value: T = ..., /) -> bool:
        """Check if value is in the set (Python `in` operator)."""
        ...

    def values(self) -> IEnumerable_1[T]:
        """Return an enumerable of values."""
        ...


class IMap[K, V](Protocol):
    """Protocol for map-like objects (Python-idiomatic version of JS Map interface).

    The Fable compiler transforms JS-style methods to Python idioms:
    - has(k) -> __contains__(k)  (enables `k in map` syntax)
    - delete(k) -> __delitem__(k)  (enables `del map[k]` syntax)
    - get(k) -> __getitem__(k)  (enables `map[k]` syntax)
    - set(k, v) -> __setitem__(k, v)  (enables `map[k] = v` syntax)

    Parameters are positional-only (/) to avoid name mismatch errors.
    """

    @property
    def size(self) -> int:
        """Return the number of key-value pairs in the map."""
        ...

    def clear(self) -> None:
        """Remove all key-value pairs from the map."""
        ...

    def __delitem__(self, key: K = ..., /) -> bool:
        """Remove a key-value pair. Returns True if key was present."""
        ...

    def __getitem__(self, key: K = ..., /) -> V | None:
        """Get the value for a key, or None if not present."""
        ...

    def __contains__(self, key: K = ..., /) -> bool:
        """Check if key is in the map."""
        ...

    def __setitem__(self, key: K, value: V, /) -> IMap[K, V]:
        """Set a key-value pair. Returns self for chaining."""
        ...

    def keys(self) -> IEnumerable_1[K]:
        """Return an enumerable of keys."""
        ...

    def values(self) -> IEnumerable_1[V]:
        """Return an enumerable of values."""
        ...


class IReadOnlyDictionary[K, V](Protocol):
    """Minimal protocol for read-only dictionary access.

    This matches what FSharpMap provides: keys() and __getitem__.
    Used by try_get_value and similar functions.
    """

    def keys(self, __unit: Any = None) -> Iterable[K]:
        """Return an iterable of keys."""
        ...

    def __getitem__(self, key: K, /) -> V:
        """Get the value for a key."""
        ...
