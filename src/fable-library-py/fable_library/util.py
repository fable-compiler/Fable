from __future__ import annotations

import builtins
import functools
import platform
import random
import re
from abc import ABC, ABCMeta, abstractmethod
from collections.abc import (
    Callable,
    Iterable,
    Iterator,
    Sequence,
    Sized,
)
from contextlib import AbstractContextManager
from enum import IntEnum
from threading import RLock
from types import TracebackType
from typing import (
    Any,
    ClassVar,
    Final,
    Literal,
    Protocol,
    Self,
    TypeGuard,
    cast,
    overload,
)
from urllib.parse import quote, unquote

from .array_ import Array
from .bases import (
    ComparableBase,
    DisposableBase,
    EnumerableBase,
    EnumeratorBase,
    EquatableBase,
    HashableBase,
    SizedBase,
    StringableBase,
)
from .core import float32, float64, int32

# Re-export protocols for backward compatibility
from .exceptions import ObjectDisposedException
from .protocols import (
    HashCode,
    IDisposable,
    IEnumerable_1,
    IEnumerator,
    SupportsLessThan,
)
from .types import UNIT, Unit


class nullable:
    """Returns a tuple of None values with the specified types.

    Used for pattern matching variable initialization where variables need
    to be typed but initialized to None. This is a type-safe alternative to
    `cast(T, None)` for multiple variables.

    Example:
        (pattern_matching_result, m, n) = nullable[int, int, int]()

    The type checker sees the tuple as `tuple[int, int, int]` even though
    all values are None at runtime.
    """

    _params: tuple[type, ...] | None = None

    def __class_getitem__(cls, params: Any) -> type[nullable]:
        # Create a new class that remembers the params
        class _Nullable(nullable):
            _params = params if isinstance(params, tuple) else (params,)

        return _Nullable

    def __new__(cls) -> Any:
        if cls._params is None:
            raise TypeError("Must specify type parameters: nullable[T1, T2, ...]()")
        return tuple(None for _ in cls._params)


# =============================================================================
# Disposable Classes
# =============================================================================
# These classes provide disposable/context manager implementations.


class AnonymousDisposable(DisposableBase):
    """A disposable that calls a provided action when disposed."""

    __slots__ = "_action", "_is_disposed", "_lock"

    def __init__(self, action: Callable[[], None]) -> None:
        self._is_disposed = False
        self._action = action
        self._lock = RLock()

    def Dispose(self) -> None:
        """Performs the task of cleaning up resources."""
        dispose = False
        with self._lock:
            if not self._is_disposed:
                dispose = True
                self._is_disposed = True

        if dispose:
            self._action()

    def __enter__(self) -> Self:
        if self._is_disposed:
            raise ObjectDisposedException()
        return self


class Disposable[T: IDisposable](DisposableBase):
    """Context manager wrapper for IDisposable objects.

    Wraps any object with a Dispose() method to provide Python context manager
    support. The __enter__ method returns the wrapped value (not self), so the
    `as` target receives the original IDisposable object.

    Example:
        with Disposable(some_enumerator) as e:
            # e is the enumerator, not the wrapper
            while e.MoveNext():
                print(e.Current)
        # e.Dispose() is called automatically

    Also provides a static factory method for creating disposables from actions,
    following the System.Reactive Disposable pattern.
    """

    __slots__ = ("_value",)

    def __init__(self, value: T) -> None:
        self._value = value

    def __enter__(self) -> T:
        """Enter context management, returning the wrapped value."""
        return self._value

    def __exit__(
        self,
        exctype: type[BaseException] | None,
        excinst: BaseException | None,
        exctb: TracebackType | None,
    ) -> Literal[False]:
        """Exit context management, calling Dispose on the wrapped value."""
        self.Dispose()
        return False

    def Dispose(self) -> None:
        """Dispose the wrapped value."""
        dispose = getattr(self._value, "Dispose", None)
        if dispose is not None:
            dispose()

    @staticmethod
    def create(action: Callable[[], None]) -> AnonymousDisposable:
        """Create a disposable from an action. Will call action when disposed."""
        return AnonymousDisposable(action)


def returns[T, **P](targettype: Callable[..., T]) -> Callable[[Callable[P, Any]], Callable[P, T]]:
    def decorator(func: Callable[P, Any]) -> Callable[P, T]:
        @functools.wraps(func)
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
            result = func(*args, **kwargs)
            return targettype(result)

        return wrapper

    return decorator


class DateKind:
    Unspecified: Final[int32] = int32(0)
    UTC: Final[int32] = int32(1)
    Local: Final[int32] = int32(2)


def equals(a: Any, b: Any) -> bool:
    match (a, b):
        case (a, b) if a is b:
            return True
        # Don't test (None, None) here, because a is b already covers that
        # case (None, None):
        #     return True
        case (None, _):
            return False
        case (_, None):
            return False
        case (a, b) if isinstance(a, Array):
            return equal_arrays(a, b)
        case (a, b) if hasattr(a, "Equals") and callable(a.Equals):
            # Call Equals method for classes with override Equals (F# style)
            return bool(a.Equals(b))
        case _:
            return a == b


def is_comparable(x: Any) -> bool:
    return hasattr(x, "CompareTo") and callable(x.CompareTo)


def is_equatable(x: Any) -> bool:
    return (hasattr(x, "Equals") and callable(x.Equals)) or (hasattr(x, "__eq__") and callable(x.__eq__))


def is_iterable(x: object) -> bool:
    return isinstance(x, Iterable)


@returns(int32)
def compare_dicts(x: dict[str, Any], y: dict[str, Any]) -> int:
    """Compare Python dicts with string keys.

    Python cannot do this natively.
    """
    x_keys = x.keys()
    y_keys = y.keys()

    if len(x_keys) != len(y_keys):
        return -1 if len(x_keys) < len(y_keys) else 1

    x_keys_ = sorted(x_keys)
    y_keys_ = sorted(y_keys)

    j = 0
    for i, key in enumerate(x_keys_):
        if key != y_keys_[i]:
            return -1 if key < y_keys_[i] else 1
        else:
            j = compare(x[key], y[key])
            if j != 0:
                return j

    return 0


@returns(int32)
def compare_arrays(xs: Sequence[Any] | None, ys: Sequence[Any] | None) -> int:
    if xs is None:
        return 0 if ys is None else 1

    if ys is None:
        return -1

    if len(xs) != len(ys):
        return -1 if len(xs) < len(ys) else 1

    j = 0
    for i, x in enumerate(xs):
        j = compare(x, ys[i])
        if j != 0:
            return j

    return 0


@returns(int32)
def compare(a: Any, b: Any) -> int:
    match (a, b):
        case (a, b) if a is b:
            return 0
        case (None, b):
            return -1 if b else 0
        case (a, None):
            return 1 if a else 0
        case (a, b) if is_comparable(a):
            return a.CompareTo(b)
        case (a, b) if isinstance(a, dict):
            return compare_dicts(cast(dict[str, Any], a), b)
        case (a, b) if isinstance(a, list):
            return compare_arrays(cast(list[Any], a), b)
        case (a, b) if is_equatable(a) and a == b:
            return 0
        case (a, b) if hasattr(a, "__lt__") and callable(a.__lt__) and a < b:
            return -1
        case _:
            return 1


def equal_arrays_with[T](xs: Sequence[T] | None, ys: Sequence[T] | None, eq: Callable[[T, T], bool]) -> bool:
    if xs is None:
        return ys is None

    if ys is None:
        return False

    if len(xs) != len(ys):
        return False

    for i, x in enumerate(xs):
        if not eq(x, ys[i]):
            return False

    return True


def equal_arrays[T](x: Sequence[T], y: Sequence[T]) -> bool:
    return equal_arrays_with(x, y, equals)


def compare_primitives[TSupportsLessThan: SupportsLessThan](x: TSupportsLessThan, y: TSupportsLessThan) -> int32:
    return int32(0 if x == y else (-1 if x < y else 1))


def min[T](comparer: Callable[[T, T], int], x: T, y: T) -> T:
    return x if comparer(x, y) < 0 else y


def max[T](comparer: Callable[[T, T], int], x: T, y: T) -> T:
    return x if comparer(x, y) > 0 else y


def clamp[T](comparer: Callable[[T, T], int], value: T, min: T, max: T):
    return min if (comparer(value, min) < 0) else max if comparer(value, max) > 0 else value


def assert_equal(actual: Any, expected: Any, msg: str | None = None) -> None:
    if not equals(actual, expected):
        raise Exception(msg or f"Expected: {expected} - Actual: {actual}")


def assert_not_equal[T](actual: T, expected: T, msg: str | None = None) -> None:
    if equals(actual, expected):
        raise Exception(msg or f"Expected: {expected} - Actual: {actual}")


MAX_LOCKS = 1024


def lock[T](lock_obj: Any, fn: Callable[[], T]) -> T:
    @functools.lru_cache(maxsize=MAX_LOCKS)
    def get_lock(n: int) -> RLock:
        return RLock()

    lock = get_lock(id(lock_obj))
    with lock:
        return fn()


class Lazy[T]:
    def __init__(self, factory: Callable[[], T]):
        self.factory = factory
        self.is_value_created: bool = False
        self.created_value: T | None = None

    @property
    def Value(self) -> T:
        if not self.is_value_created:
            self.created_value = self.factory()
            self.is_value_created = True

        assert self.created_value is not None
        return self.created_value

    @property
    def IsValueCreated(self):
        return self.is_value_created


def lazy_from_value[T](v: T) -> Lazy[T]:
    return Lazy(lambda: v)


def pad_with_zeros(i: int, length: int) -> str:
    string = str(i)
    while len(string) < length:
        string = "0" + string
    return string


def pad_left_and_right_with_zeros(i: int, length_left: int, length_right: int) -> str:
    string = str(i)
    while len(string) < length_left:
        string = "0" + string
    while len(string) < length_right:
        string = string + "0"
    return string


class Atom[T](Protocol):
    @overload
    def __call__(self) -> T: ...
    @overload
    def __call__(self, value: T, /) -> None: ...
    def __call__(self, *value: T) -> T | None: ...


def create_atom[T](value: T) -> Atom[T]:
    atom = value

    @overload
    def wrapper() -> T: ...
    @overload
    def wrapper(value: T, /) -> None: ...
    def wrapper(*value: T) -> T | None:
        nonlocal atom

        if not value:
            return atom

        atom = value[0]
        return None

    return wrapper


def create_obj(fields: list[tuple[Any, Any]]):
    return dict(fields)


def tohex(val: int, nbits: int | None = None) -> str:
    if nbits:
        val = (val + (1 << nbits)) % (1 << nbits)
    return f"{val:x}"


def int_to_string(i: int, radix: int = 10, bitsize: int | None = None) -> str:
    i = int(i)  # Translate core pyo3 integers to Python integers

    if radix == 10:
        return f"{i:d}"
    if radix == 16:
        return tohex(i, bitsize)
    if radix == 2:
        return f"{i:b}"
    if radix == 8:
        return f"{i:o}"
    return str(i)


def count(it: IEnumerable_1[Any] | Iterable[Any]) -> int32:
    it = to_iterable(it)
    if isinstance(it, Sized):
        return int32(len(it))

    count = int32.ZERO
    for _ in it:
        count += 1

    return count


def clear(col: dict[Any, Any] | list[Any] | None) -> None:
    if isinstance(col, list | dict):
        col.clear()


class Enumerator[T](EnumeratorBase[T], DisposableBase, IEnumerator[T]):
    """Concrete enumerator that wraps a Python iterator."""

    __slots__ = "current", "iter"

    def __init__(self, iter: Iterator[T]) -> None:
        self.iter = iter
        self.current: T

    def System_Collections_Generic_IEnumerator_1_get_Current(self) -> T:
        return self.current

    def System_Collections_IEnumerator_get_Current(self) -> Any:
        """Non-generic IEnumerator.Current implementation."""
        return self.current

    def System_Collections_IEnumerator_MoveNext(self) -> bool:
        try:
            cur = next(self.iter)
            self.current = cur
            return True
        except StopIteration:
            return False

    def System_Collections_IEnumerator_Reset(self) -> None:
        raise Exception("Python iterators cannot be reset")

    def Dispose(self) -> None:
        return

    def __next__(self) -> T:
        return next(self.iter)

    def __iter__(self) -> Iterator[T]:
        return self


class Enumerable[T](EnumerableBase[T]):
    """Concrete enumerable that wraps a Python iterable."""

    __slots__ = "xs"

    def __init__(self, xs: Iterable[T]) -> None:
        self.xs = xs

    def GetEnumerator(self, __unit: Unit = UNIT) -> IEnumerator[T]:
        return Enumerator(iter(self.xs))

    def __iter__(self) -> Iterator[T]:
        return iter(self.xs)


def to_enumerable[T](e: Iterable[T]) -> IEnumerable_1[T]:
    return Enumerable(e)


def get_enumerator(o: IEnumerable_1[Any] | Iterable[Any]) -> Enumerator:
    attr = getattr(o, "GetEnumerator", None)
    if attr:
        return attr()

    if isinstance(o, dict):
        # Dictionaries should produce tuples
        return Enumerator(iter(cast(Any, o.items())))

    return Enumerator(iter(cast(Any, o)))


def is_array_like(x: Any) -> TypeGuard[Array]:
    # Match FSharpArray (Rust) which has .length property
    # Also match tuples for F# tuple pattern matching
    # Python lists should fall through to the iterator path (no .length)
    return isinstance(x, Array | tuple)


def is_disposable(x: Any) -> TypeGuard[IDisposable]:
    return x is not None and isinstance(x, IDisposable)


def dispose(x: IDisposable | AbstractContextManager[Any]) -> None:
    """Helper to dispose objects.

    Also tries to call `__exit__` if the object turns out to be a Python resource manager.
    For more info see: https://www.python.org/dev/peps/pep-0310/
    """
    if isinstance(x, IDisposable):
        x.Dispose()
    elif hasattr(x, "__exit__"):
        x.__exit__(None, None, None)


def is_hashable(x: Any) -> TypeGuard[HashCode]:
    return hasattr(x, "GetHashCode")


def is_hashable_py(x: Any) -> bool:
    return hasattr(x, "__hash__") and callable(x.__hash__)


def to_iterator[T](en: IEnumerator[T]) -> Iterator[T]:
    """Convert an IEnumerator to a Python Iterator with proper disposal.

    This wraps an F# IEnumerator to make it a proper Python iterator
    that can be used with for loops and other Python iteration protocols.

    The generator's finally block ensures Dispose() is called when:
    - Iteration completes normally
    - A break/return exits the loop early
    - An exception is raised during iteration
    - The iterator is garbage collected
    """
    try:
        while en.System_Collections_IEnumerator_MoveNext():
            yield en.System_Collections_IEnumerator_get_Current()
    finally:
        if hasattr(en, "Dispose"):
            en.Dispose()


def to_iterable[T](items: Iterable[T] | IEnumerable_1[T]) -> Iterable[T]:
    """Convert IEnumerable_1 to Iterable if needed.

    Prefers Python Iterable protocol (more efficient), falls back to GetEnumerator.
    """
    if isinstance(items, Iterable):
        # Has Python iterator protocol, use it directly
        yield from items
    elif hasattr(items, "GetEnumerator"):
        # It's an IEnumerable_1 without __iter__, iterate via the enumerator
        en = items.GetEnumerator()
        try:
            while en.System_Collections_IEnumerator_MoveNext():
                yield en.System_Collections_Generic_IEnumerator_1_get_Current()
        finally:
            if hasattr(en, "Dispose"):
                en.Dispose()
    else:
        raise TypeError(f"Expected Iterable or IEnumerable_1, got {type(items)}")


class ObjectRef:
    id_map: ClassVar = dict[int, int]()
    count: ClassVar = 0

    @staticmethod
    def id(o: Any) -> int:
        _id = id(o)
        if _id not in ObjectRef.id_map:
            ObjectRef.count += 1
            ObjectRef.id_map[_id] = ObjectRef.count

        return ObjectRef.id_map[_id]


def safe_hash(x: Any) -> int32:
    return (
        int32.ZERO
        if x is None
        else int32(hash(x))
        if is_hashable_py(x)
        else x.GetHashCode()
        if is_hashable(x)
        else number_hash(ObjectRef.id(x))
    )


def string_hash(s: str) -> int32:
    h = 5381
    for c in s:
        h = (h * 33) ^ ord(c)

    return int32(h)


def number_hash(x: Any) -> int32:
    return x.GetHashCode() if hasattr(x, "GetHashCode") else int32(hash(x))


def identity_hash(x: Any) -> int32:
    if x is None:
        return int32(0)

    if is_hashable(x):
        return x.GetHashCode()

    if is_hashable_py(x):
        return int32(hash(x))

    return number_hash(ObjectRef.id(x))


def combine_hash_codes(hashes: list[int32]) -> int32:
    if not hashes:
        return int32(0)

    return functools.reduce(lambda h1, h2: ((h1 << 5) + h1) ^ h2, hashes)


def structural_hash(x: Any) -> int32:
    return int32(hash(x))


def array_hash(xs: Iterable[object]) -> int32:
    hashes: list[int32] = []
    for x in xs:
        hashes.append(structural_hash(x))

    return combine_hash_codes(hashes)


def physical_hash(x: Any) -> int32:
    return number_hash(ObjectRef.id(x))


@overload
def round(value: float32, digits: int = 0) -> float32: ...


@overload
def round(value: float64, digits: int = 0) -> float64: ...


def round(value: float64 | float32, digits: int = 0) -> float64 | float32:
    return value.round(digits)


def randint(a: int32, b: int32) -> int32:
    return int32(random.randint(int(a), int(b) - 1))


def unescape_data_string(s: str) -> str:
    # https://stackoverflow.com/a/4458580/524236
    return unquote(re.sub(r"\+", "%20", s))


def escape_data_string(s: str) -> str:
    return quote(s, safe="")


def escape_uri_string(s: str) -> str:
    return quote(s, safe="&?:/!=")


def ignore(a: Any = None) -> None:
    """Ignore argument, returns None."""
    return


def copy_to_array[T](src: Array[T], srci: int, trg: Array[T], trgi: int, cnt: int) -> None:
    for i in builtins.range(0, cnt, 1):
        trg[trgi + i] = src[srci + i]


class PlatformID(IntEnum):
    Win32S = 0
    Win32Windows = 1
    Win32NT = 2
    WinCE = 3
    Unix = 4
    Xbox = 5
    MacOSX = 6
    Other = 7


def get_platform() -> PlatformID:
    name = platform.platform(terse=True).lower()

    if name.startswith("windows"):
        return PlatformID.Win32NT
    elif name.startswith("linux"):
        return PlatformID.Unix
    elif name.startswith("macos"):
        return PlatformID.MacOSX

    return PlatformID.Other


class StaticPropertyBase[T](ABC):
    """Base class for static property descriptors."""

    __slots__ = "name", "setter_func"

    def __init__(self, setter_func: Callable[[T], None] | None = None) -> None:
        self.setter_func = setter_func
        self.name: str | None = None  # Will be set by __set_name__ if available

    @abstractmethod
    def __get__(self, instance: Any, owner: Any) -> T:
        """Get the property value."""
        pass

    def __set__(self, instance: Any, value: T) -> None:
        """Set the property value."""
        if self.setter_func:
            self.setter_func(value)
        self._set_value(value)

    @abstractmethod
    def _set_value(self, value: T) -> None:
        """Internal method to store the value."""
        pass

    def __set_name__(self, owner: type, name: str) -> None:
        """Called when the descriptor is assigned to a class attribute"""
        self.name = name


class StaticProperty[T](StaticPropertyBase[T]):
    """Static property descriptor for direct values with caching.

    This descriptor caches the value and is suitable for static properties
    that hold a single value that can be updated.
    """

    __slots__ = "_initialized", "value"

    def __init__(self, initial_value: T, setter_func: Callable[[T], None] | None = None) -> None:
        super().__init__(setter_func)
        self.value: T = initial_value
        self._initialized: bool = True

    def __get__(self, instance: Any, owner: Any) -> T:
        return self.value

    def _set_value(self, value: T) -> None:
        """Cache the value."""
        self.value = value


class StaticLazyProperty[T](StaticPropertyBase[T]):
    """Static property descriptor for factory-based lazy initialization.

    This descriptor calls the factory function each time the property is accessed,
    making it suitable for computed properties or properties that should always
    return fresh values.
    """

    __slots__ = ("factory",)

    def __init__(self, factory: Callable[[], T], setter_func: Callable[[T], None] | None = None) -> None:
        super().__init__(setter_func)
        self.factory: Callable[[], T] = factory

    def __get__(self, instance: Any, owner: Any) -> T:
        return self.factory()

    def _set_value(self, value: T) -> None:
        """Factory-based properties don't cache values."""
        pass  # The factory handles value retrieval


class StaticPropertyMeta(ABCMeta):
    """Metaclass that enables StaticProperty descriptors to work with class-level
    assignment.

    Note: We inherit from type(Protocol) to be compatible when combined with classes
    that inherit from Protocol (like IDisposable) or ABC.
    """

    def __setattr__(cls, name: str, value: Any) -> None:
        # Check if the attribute exists and is a StaticPropertyBase
        # Use dict lookup instead of getattr to avoid triggering descriptors
        if name in getattr(cls, "__dict__", {}):
            existing_attr = cls.__dict__[name]
            if isinstance(existing_attr, StaticPropertyBase):
                # Call the descriptor's __set__ method instead of replacing it
                attr = cast(StaticPropertyBase[Any], existing_attr)
                attr.__set__(cls, value)
                return

        # Check parent classes for the attribute
        for base in cls.__mro__[1:]:  # Skip self
            if hasattr(base, "__dict__") and name in base.__dict__:
                existing_attr = base.__dict__[name]
                if isinstance(existing_attr, StaticPropertyBase):
                    attr = cast(StaticPropertyBase[Any], existing_attr)
                    attr.__set__(cls, value)
                    return

        # Normal attribute assignment
        super().__setattr__(name, value)


def range(start: int, stop: int, step: int = 1) -> Iterable[int32]:
    """Range function that returns an iterable of int32 values.

    This function handles the difference between F# and Python range semantics:
    - F# ranges are inclusive (include the end value)
    - Python ranges are exclusive (exclude the end value)

    The function automatically adjusts the stop value to match F# semantics.
    """
    # Adjust stop value to be inclusive (F# semantics) by adding step direction
    if step > 0:
        # For positive step, we want to include the stop value
        adjusted_stop = stop + 1
    else:
        # For negative step, we want to include the stop value
        adjusted_stop = stop - 1

    return (int32(x) for x in builtins.range(start, adjusted_stop, step))


__all__ = [
    "UNIT",
    "AnonymousDisposable",
    "ComparableBase",
    "Disposable",
    "DisposableBase",
    "EnumerableBase",
    "EnumeratorBase",
    "EquatableBase",
    "HashableBase",
    "IDisposable",
    "ObjectDisposedException",
    "ObjectRef",
    "PlatformID",
    "SizedBase",
    "StaticLazyProperty",
    "StaticProperty",
    "StaticPropertyBase",
    "StaticPropertyMeta",
    "StringableBase",
    "Unit",
    "array_hash",
    "copy_to_array",
    "escape_data_string",
    "escape_uri_string",
    "get_platform",
    "identity_hash",
    "ignore",
    "nullable",
    "number_hash",
    "physical_hash",
    "randint",
    "range",
    "round",
    "to_iterable",
    "unescape_data_string",
]
