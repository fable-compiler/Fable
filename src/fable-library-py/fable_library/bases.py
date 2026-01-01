"""ABC base classes for F# types in Python.

These base classes provide Python dunder methods by delegating to F# methods:
- ContextManagerBase: __enter__, __exit__ from Dispose()
- IterableBase: __iter__ from GetEnumerator()
- IteratorBase: __iter__, __next__ from MoveNext()/Current
- StringableBase: __str__, __repr__ from ToString()
- EquatableBase: __eq__, __ne__ from Equals()
- ComparableBase: __lt__, __le__, __gt__, __ge__ from CompareTo()
- HashableBase: __hash__ from GetHashCode()
- SizedBase: __len__ from Count property

Usage:
    Classes should inherit from these bases and implement the corresponding F# method.
    Use the Py.ContextManager, Py.Stringable, Py.Equatable, Py.Comparable, Py.Hashable, Py.Sized
    marker interfaces in F# to have the compiler automatically add these base classes.
"""

from __future__ import annotations

from abc import ABC, ABCMeta, abstractmethod
from collections.abc import Iterator
from types import TracebackType
from typing import TYPE_CHECKING, Any, Literal, Self, cast


if TYPE_CHECKING:
    from .protocols import IEnumerator


class ObjectDisposedException(Exception):
    """Exception thrown when accessing a disposed object."""

    def __init__(self) -> None:
        super().__init__("Cannot access a disposed object")


class ContextManagerBase(ABC, metaclass=ABCMeta):
    """ABC base class for context managers (disposable objects).

    Provides context manager support (__enter__/__exit__) for classes that
    implement the IDisposable pattern. Inherit from this class to get
    automatic context manager support.
    """

    __slots__ = ()

    @abstractmethod
    def Dispose(self) -> None:
        """Dispose of resources. Must be implemented by subclasses."""
        raise NotImplementedError

    def __enter__(self) -> Self:
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


# Alias for backward compatibility
DisposableBase = ContextManagerBase


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


class IteratorBase[T](ABC):
    """ABC base class for iterators.

    Provides Python iterator protocol support (__iter__/__next__) for classes
    that implement the IEnumerator pattern.

    Note: This does NOT include Dispose. For types that need context manager
    support, use Py.ContextManager marker or add ContextManagerBase separately.
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


class IterableBase[T](ABC):
    """ABC base class for iterable collections.

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
        # IteratorBase (added by the compiler), which provides __iter__ and __next__.
        # The cast tells the type checker this is safe.
        return cast(Iterator[T], self.GetEnumerator())


# =============================================================================
# Mapping and Set Base Classes
# =============================================================================
# These provide Python's collections.abc protocols by delegating to F# methods.
# Use Py.Mapping, Py.MutableMapping, Py.Set, Py.MutableSet marker interfaces
# to have the compiler automatically add these base classes.


class MappingBase[K, V](ABC):
    """ABC base class for read-only dictionary-like types.

    Provides Python Mapping protocol (__getitem__, __contains__, __len__,
    __iter__, keys, values, items, get) by delegating to F# methods.

    Required F# methods:
    - get_Item(key) -> value (for __getitem__)
    - ContainsKey(key) -> bool (for __contains__)
    - Count property -> int (for __len__)
    - GetEnumerator() -> IEnumerator<KeyValuePair> (for __iter__)
    """

    __slots__ = ()

    @abstractmethod
    def get_Item(self, key: K, /) -> V:
        """Get value by key. Must be implemented by subclasses."""
        raise NotImplementedError

    @abstractmethod
    def ContainsKey(self, key: K, /) -> bool:
        """Check if key exists. Must be implemented by subclasses."""
        raise NotImplementedError

    @property
    @abstractmethod
    def Count(self) -> int:
        """Get number of items. Must be implemented by subclasses."""
        raise NotImplementedError

    @abstractmethod
    def GetEnumerator(self) -> IEnumerator[tuple[K, V]]:
        """Get enumerator over key-value pairs. Must be implemented by subclasses."""
        raise NotImplementedError

    # Python Mapping protocol - delegated to F# methods

    def __getitem__(self, key: K) -> V:
        """Get value by key (enables map[key] syntax)."""
        return self.get_Item(key)

    def __contains__(self, key: object) -> bool:
        """Check if key exists (enables 'key in map' syntax)."""
        return self.ContainsKey(key)  # type: ignore[arg-type]

    def __len__(self) -> int:
        """Get number of items (enables len(map))."""
        return int(self.Count)

    def __iter__(self) -> Iterator[K]:
        """Iterate over keys (enables 'for key in map')."""
        enumerator = self.GetEnumerator()
        # The enumerator yields (key, value) tuples
        return (k for k, _ in cast(Iterator[tuple[K, V]], enumerator))

    def keys(self) -> Iterator[K]:
        """Return an iterator over keys."""
        return iter(self)

    def values(self) -> Iterator[V]:
        """Return an iterator over values."""
        enumerator = self.GetEnumerator()
        return (v for _, v in cast(Iterator[tuple[K, V]], enumerator))

    def items(self) -> Iterator[tuple[K, V]]:
        """Return an iterator over (key, value) pairs."""
        return cast(Iterator[tuple[K, V]], self.GetEnumerator())

    def get(self, key: K, default: V | None = None) -> V | None:
        """Get value by key, or default if not found."""
        if self.ContainsKey(key):
            return self.get_Item(key)
        return default


class MutableMappingBase[K, V](MappingBase[K, V]):
    """ABC base class for mutable dictionary-like types.

    Extends MappingBase with mutation methods (__setitem__, __delitem__,
    clear, pop, popitem, setdefault, update).

    Additional required F# methods:
    - set_Item(key, value) (for __setitem__)
    - Remove(key) -> bool (for __delitem__)
    - Clear() (for clear)
    """

    __slots__ = ()

    @abstractmethod
    def set_Item(self, key: K, value: V, /) -> None:
        """Set value by key. Must be implemented by subclasses."""
        raise NotImplementedError

    @abstractmethod
    def Remove(self, key: K, /) -> bool:
        """Remove key. Returns True if key was present. Must be implemented by subclasses."""
        raise NotImplementedError

    @abstractmethod
    def Clear(self) -> None:
        """Remove all items. Must be implemented by subclasses."""
        raise NotImplementedError

    # Python MutableMapping protocol - delegated to F# methods

    def __setitem__(self, key: K, value: V) -> None:
        """Set value by key (enables map[key] = value syntax)."""
        self.set_Item(key, value)

    def __delitem__(self, key: K) -> None:
        """Remove key (enables 'del map[key]' syntax)."""
        if not self.Remove(key):
            raise KeyError(key)

    def clear(self) -> None:
        """Remove all items."""
        self.Clear()

    def pop(self, key: K, *args: V) -> V:
        """Remove and return value for key, or default if not found."""
        if len(args) > 1:
            raise TypeError(f"pop expected at most 2 arguments, got {1 + len(args)}")
        try:
            value = self.get_Item(key)
            self.Remove(key)
            return value
        except KeyError:
            if args:
                return args[0]
            raise

    def popitem(self) -> tuple[K, V]:
        """Remove and return an arbitrary (key, value) pair."""
        try:
            key = next(iter(self))
        except StopIteration:
            raise KeyError("dictionary is empty") from None
        value = self.get_Item(key)
        self.Remove(key)
        return (key, value)

    def setdefault(self, key: K, default: V) -> V:
        """Get value for key, setting it to default if not present."""
        if self.ContainsKey(key):
            return self.get_Item(key)
        self.set_Item(key, default)
        return default


class SetBase[T](ABC):
    """ABC base class for read-only set-like types.

    Provides Python Set protocol (__contains__, __len__, __iter__)
    by delegating to F# methods.

    Required F# methods:
    - Contains(item) -> bool (for __contains__)
    - Count property -> int (for __len__)
    - GetEnumerator() -> IEnumerator<T> (for __iter__)
    """

    __slots__ = ()

    @abstractmethod
    def Contains(self, item: T, /) -> bool:
        """Check if item exists. Must be implemented by subclasses."""
        raise NotImplementedError

    @property
    @abstractmethod
    def Count(self) -> int:
        """Get number of items. Must be implemented by subclasses."""
        raise NotImplementedError

    @abstractmethod
    def GetEnumerator(self) -> IEnumerator[T]:
        """Get enumerator. Must be implemented by subclasses."""
        raise NotImplementedError

    # Python Set protocol - delegated to F# methods

    def __contains__(self, item: object) -> bool:
        """Check if item exists (enables 'item in set' syntax)."""
        return self.Contains(item)  # type: ignore[arg-type]

    def __len__(self) -> int:
        """Get number of items (enables len(set))."""
        return int(self.Count)

    def __iter__(self) -> Iterator[T]:
        """Iterate over items (enables 'for item in set')."""
        return cast(Iterator[T], self.GetEnumerator())


class MutableSetBase[T](SetBase[T]):
    """ABC base class for mutable set-like types.

    Extends SetBase with mutation methods (add, discard, remove, pop, clear).

    Additional required F# methods:
    - Add(item) -> bool (for add)
    - Remove(item) -> bool (for discard/remove)
    - Clear() (for clear)
    """

    __slots__ = ()

    @abstractmethod
    def Add(self, item: T, /) -> bool:
        """Add item. Returns True if item was added. Must be implemented by subclasses."""
        raise NotImplementedError

    @abstractmethod
    def Remove(self, item: T, /) -> bool:
        """Remove item. Returns True if item was present. Must be implemented by subclasses."""
        raise NotImplementedError

    @abstractmethod
    def Clear(self) -> None:
        """Remove all items. Must be implemented by subclasses."""
        raise NotImplementedError

    # Python MutableSet protocol - delegated to F# methods

    def add(self, item: T) -> None:
        """Add item to set."""
        self.Add(item)

    def discard(self, item: T) -> None:
        """Remove item if present (no error if not found)."""
        self.Remove(item)

    def remove(self, item: T) -> None:
        """Remove item (raises KeyError if not found)."""
        if not self.Remove(item):
            raise KeyError(item)

    def pop(self) -> T:
        """Remove and return an arbitrary item."""
        try:
            item = next(iter(self))
        except StopIteration:
            raise KeyError("set is empty") from None
        self.Remove(item)
        return item

    def clear(self) -> None:
        """Remove all items."""
        self.Clear()


__all__ = [
    "ComparableBase",
    "ContextManagerBase",
    "DisposableBase",
    "EquatableBase",
    "HashableBase",
    "IterableBase",
    "IteratorBase",
    "MappingBase",
    "MutableMappingBase",
    "MutableSetBase",
    "ObjectDisposedException",
    "SetBase",
    "SizedBase",
    "StringableBase",
]
