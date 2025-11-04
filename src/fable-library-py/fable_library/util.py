from __future__ import annotations

import builtins
import functools
import platform
import random
import re
import weakref
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
    Literal,
    Protocol,
    TypeGuard,
    cast,
)
from urllib.parse import quote, unquote

from .array_ import Array
from .core import float64, int32


class SupportsLessThan(Protocol):
    @abstractmethod
    def __lt__(self, __other: Any) -> bool:
        raise NotImplementedError


def returns[T, **P](targettype: Callable[..., T]) -> Callable[[Callable[P, Any]], Callable[P, T]]:
    def decorator(func: Callable[P, Any]) -> Callable[P, T]:
        @functools.wraps(func)
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
            result = func(*args, **kwargs)
            return targettype(result)

        return wrapper

    return decorator


class ObjectDisposedException(Exception):
    def __init__(self):
        super().__init__("Cannot access a disposed object")


class IDisposable(ABC):
    """IDisposable interface.

    Note: IDisposable is currently not a protocol since it also
    implements resource management and thus cannot use static subtyping
    and needs to be inherited from.
    """

    __slots__ = ()

    @abstractmethod
    def Dispose(self) -> None: ...

    def __enter__(self):
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

    @staticmethod
    def create(action: Callable[[], None]):
        """Create disposable from action. Will call action when
        disposed."""
        return AnonymousDisposable(action)


# Disposable is now a type parameter in function definitions


class AnonymousDisposable(IDisposable):
    __slots__ = "_action", "_is_disposed", "_lock"

    def __init__(self, action: Callable[[], None]):
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

    def __enter__(self):
        if self._is_disposed:
            raise ObjectDisposedException()
        return self


class IEquatable(Protocol):
    def __eq__(self, other: Any) -> bool: ...

    def __hash__(self) -> int: ...


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


class IComparer(Protocol):
    """Defines a method that a type implements to compare two objects.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.icomparer-1
    """

    @abstractmethod
    def Compare[T_in](self, x: Any = None, y: Any = None) -> int32:
        raise NotImplementedError


class IComparer_1[T_in](Protocol):
    """Defines a method that a type implements to compare two objects.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.icomparer-1
    """

    @abstractmethod
    def Compare(self, x: T_in, y: T_in, /) -> int32:
        raise NotImplementedError


class IEqualityComparer(Protocol):
    @abstractmethod
    def Equals(self, x: Any = None, y: Any = None, /) -> bool:
        raise NotImplementedError

    @abstractmethod
    def GetHashCode(self, /, x: Any = None) -> int32:
        raise NotImplementedError


class IEqualityComparer_1[T_in](Protocol):
    @abstractmethod
    def Equals(self, /, x: T_in, y: T_in) -> bool:
        raise NotImplementedError

    @abstractmethod
    def GetHashCode(self) -> int32:
        raise NotImplementedError


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


class ISet[T](Protocol):
    """Protocol for set-like objects that support add/has operations."""

    @abstractmethod
    def add(self, value: T) -> Any:
        """Add a value to the set."""
        raise NotImplementedError

    @abstractmethod
    def __contains__(self, value: T) -> bool:
        """Check if value is in the set."""
        raise NotImplementedError


class DateKind(IntEnum):
    Unspecified = 0
    UTC = 1
    Local = 2


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
        case (a, b) if is_array_like(a):
            return equal_arrays(a, b)
        case _:
            return a == b


def is_comparable(x: Any) -> bool:
    return hasattr(x, "__cmp__") and callable(x.__cmp__)


def is_equatable(x: Any) -> bool:
    return hasattr(x, "__eq__") and callable(x.__eq__)


def is_iterable(x: Any) -> bool:
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
def compare_arrays(xs: list[Any] | None, ys: list[Any] | None) -> int:
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
            return a.__cmp__(b)
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
    def __call__(self, *value: T) -> T | None: ...


def create_atom[T](value: T) -> Atom[T]:
    atom = value

    def wrapper(*value: T) -> T | None:
        nonlocal atom

        if len(value) == 0:
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


def count(col: Iterable[Any]) -> int32:
    if isinstance(col, Sized):
        return int32(len(col))

    count = int32.ZERO
    for _ in col:
        count += 1

    return count


def clear(col: dict[Any, Any] | list[Any] | None) -> None:
    if isinstance(col, list | dict):
        col.clear()


class IEnumerator[T](Iterator[T], IDisposable):
    __slots__ = ()

    def Current(self) -> T:
        return self.System_Collections_Generic_IEnumerator_1_get_Current()

    def MoveNext(self) -> bool:
        return self.System_Collections_IEnumerator_MoveNext()

    def Reset(self) -> None:
        return self.System_Collections_IEnumerator_Reset()

    @abstractmethod
    def System_Collections_Generic_IEnumerator_1_get_Current(self) -> T: ...

    def System_Collections_IEnumerator_get_Current(self) -> Any:
        return self.System_Collections_Generic_IEnumerator_1_get_Current()

    @abstractmethod
    def System_Collections_IEnumerator_MoveNext(self) -> bool: ...

    @abstractmethod
    def System_Collections_IEnumerator_Reset(self) -> None: ...

    def __iter__(self) -> Iterator[T]:
        return self

    def __next__(self) -> T:
        if not self.MoveNext():
            raise StopIteration
        return self.Current()


class IEnumerable(Iterable[Any], Protocol):
    __slots__ = ()

    @abstractmethod
    def GetEnumerator(self) -> IEnumerator[Any]: ...

    def __iter__(self) -> Iterator[Any]:
        return self.GetEnumerator()


class IEnumerable_1[T](Iterable[T], Protocol):
    __slots__ = ()

    @abstractmethod
    def GetEnumerator(self) -> IEnumerator[T]: ...

    def __iter__(self) -> Iterator[T]:
        return self.GetEnumerator()


class ICollection[T](IEnumerable_1[T], Protocol): ...


class IDictionary[Key, Value](ICollection[tuple[Key, Value]], Protocol):
    @abstractmethod
    def keys(self) -> IEnumerable_1[Key]: ...

    def values(self) -> IEnumerable_1[Value]: ...


class Enumerator[T](IEnumerator[T]):
    __slots__ = "current", "iter"

    def __init__(self, iter: Iterator[T]) -> None:
        self.iter = iter
        self.current: T

    def System_Collections_Generic_IEnumerator_1_get_Current(self) -> T:
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


class Enumerable[T](IEnumerable_1[T]):
    __slots__ = "xs"

    def __init__(self, xs: Iterable[T]) -> None:
        self.xs = xs

    def GetEnumerator(self) -> IEnumerator[T]:
        return Enumerator(iter(self.xs))

    def __iter__(self) -> Iterator[T]:
        return iter(self.xs)


def to_enumerable[T](e: Iterable[T]) -> IEnumerable_1[T]:
    return Enumerable(e)


def get_enumerator(o: Iterable[Any]) -> Enumerator[Any]:
    attr = getattr(o, "GetEnumerator", None)
    if attr:
        return attr()
    elif isinstance(o, dict):
        # Dictionaries should produce tuples
        return Enumerator(iter(cast(Any, o.items())))
    else:
        return Enumerator(iter(o))


_curried = weakref.WeakKeyDictionary[Any, Any]()


def uncurry2[T1, T2, TResult](f: Callable[[T1], Callable[[T2], TResult]]) -> Callable[[T1, T2], TResult]:
    def f2(a1: T1, a2: T2) -> TResult:
        return f(a1)(a2)

    _curried[f2] = f
    return f2


def curry2[T1, T2, TResult](f: Callable[[T1, T2], TResult]) -> Callable[[T1], Callable[[T2], TResult]]:
    f2 = _curried.get(f)
    if f2 is None:
        return lambda a1: lambda a2: f(a1, a2)
    else:
        return f2


def uncurry3[T1, T2, T3, TResult](
    f: Callable[[T1], Callable[[T2], Callable[[T3], TResult]]],
) -> Callable[[T1, T2, T3], TResult]:
    def f2(a1: T1, a2: T2, a3: T3) -> TResult:
        return f(a1)(a2)(a3)

    _curried[f2] = f
    return f2


def curry3[T1, T2, T3, TResult](
    f: Callable[[T1, T2, T3], TResult],
) -> Callable[[T1], Callable[[T2], Callable[[T3], TResult]]]:
    f2 = _curried.get(f)
    if f2 is None:
        return lambda a1: lambda a2: lambda a3: f(a1, a2, a3)
    else:
        return f2


def uncurry4[T1, T2, T3, T4, TResult](
    f: Callable[[T1], Callable[[T2], Callable[[T3], Callable[[T4], TResult]]]],
) -> Callable[[T1, T2, T3, T4], TResult]:
    def f2(a1: T1, a2: T2, a3: T3, a4: T4) -> TResult:
        return f(a1)(a2)(a3)(a4)

    _curried[f2] = f
    return f2


def curry4[T1, T2, T3, T4, TResult](
    f: Callable[[T1, T2, T3, T4], TResult],
) -> Callable[[T1], Callable[[T2], Callable[[T3], Callable[[T4], TResult]]]]:
    f2 = _curried.get(f)
    if f2 is None:
        return lambda a1: lambda a2: lambda a3: lambda a4: f(a1, a2, a3, a4)
    else:
        return f2


def uncurry5[T1, T2, T3, T4, T5, TResult](
    f: Callable[
        [T1],
        Callable[[T2], Callable[[T3], Callable[[T4], Callable[[T5], TResult]]]],
    ],
) -> Callable[[T1, T2, T3, T4, T5], TResult]:
    def f2(a1: T1, a2: T2, a3: T3, a4: T4, a5: T5) -> TResult:
        return f(a1)(a2)(a3)(a4)(a5)

    _curried[f2] = f
    return f2


def curry5[T1, T2, T3, T4, T5, TResult](
    f: Callable[[T1, T2, T3, T4, T5], TResult],
) -> Callable[[T1], Callable[[T2], Callable[[T3], Callable[[T4], Callable[[T5], TResult]]]]]:
    f2 = _curried.get(f)
    if f2 is None:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: f(a1, a2, a3, a4, a5)
    else:
        return f2


def uncurry6[T1, T2, T3, T4, T5, T6, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[[T3], Callable[[T4], Callable[[T5], Callable[[T6], TResult]]]],
        ],
    ],
) -> Callable[[T1, T2, T3, T4, T5, T6], TResult]:
    def f2(a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6) -> TResult:
        return f(a1)(a2)(a3)(a4)(a5)(a6)

    _curried[f2] = f
    return f2


def curry6[T1, T2, T3, T4, T5, T6, TResult](
    f: Callable[[T1, T2, T3, T4, T5, T6], TResult],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[[T3], Callable[[T4], Callable[[T5], Callable[[T6], TResult]]]],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: f(a1, a2, a3, a4, a5, a6)
    else:
        return f2


def uncurry7[T1, T2, T3, T4, T5, T6, T7, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[[T4], Callable[[T5], Callable[[T6], Callable[[T7], TResult]]]],
            ],
        ],
    ],
) -> Callable[[T1, T2, T3, T4, T5, T6, T7], TResult]:
    def f2(a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7) -> TResult:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)

    _curried[f2] = f
    return f2


def curry7[T1, T2, T3, T4, T5, T6, T7, TResult](
    f: Callable[[T1, T2, T3, T4, T5, T6, T7], TResult],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[[T4], Callable[[T5], Callable[[T6], Callable[[T7], TResult]]]],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: f(
            a1, a2, a3, a4, a5, a6, a7
        )
    else:
        return f2


def uncurry8[T1, T2, T3, T4, T5, T6, T7, T8, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[[T6], Callable[[T7], Callable[[T8], TResult]]],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[[T1, T2, T3, T4, T5, T6, T7, T8], TResult]:
    def f2(a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8) -> TResult:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)

    _curried[f2] = f
    return f2


def curry8[T1, T2, T3, T4, T5, T6, T7, T8, TResult](
    f: Callable[[T1, T2, T3, T4, T5, T6, T7, T8], TResult],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[[T5], Callable[[T6], Callable[[T7], Callable[[T8], TResult]]]],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: f(
            a1, a2, a3, a4, a5, a6, a7, a8
        )
    else:
        return f2


def uncurry9[T1, T2, T3, T4, T5, T6, T7, T8, T9, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[[T7], Callable[[T8], Callable[[T9], TResult]]],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9], TResult]:
    def f2(a1: T1, a2: T2, a3: T3, a4: T4, a5: T5, a6: T6, a7: T7, a8: T8, a9: T9) -> TResult:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)

    _curried[f2] = f
    return f2


def curry9[T1, T2, T3, T4, T5, T6, T7, T8, T9, TResult](
    f: Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9], TResult],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[[T7], Callable[[T8], Callable[[T9], TResult]]],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: f(
            a1, a2, a3, a4, a5, a6, a7, a8, a9
        )
    else:
        return f2


def uncurry10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[[T8], Callable[[T9], Callable[[T10], TResult]]],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], TResult]:
    def f2(
        a1: T1,
        a2: T2,
        a3: T3,
        a4: T4,
        a5: T5,
        a6: T6,
        a7: T7,
        a8: T8,
        a9: T9,
        a10: T10,
    ) -> TResult:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)

    _curried[f2] = f
    return f2


def curry10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TResult](
    f: Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], TResult],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[[T8], Callable[[T9], Callable[[T10], TResult]]],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: f(
                a1, a2, a3, a4, a5, a6, a7, a8, a9, a10
            )
        )
    else:
        return f2


def uncurry11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[[T10], Callable[[T11], TResult]],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], TResult]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)

    _curried[f2] = f
    return f2


def curry11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult](
    f: Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], TResult],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[[T9], Callable[[T10], Callable[[T11], TResult]]],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: f(
                a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11
            )
        )
    else:
        return f2


def uncurry12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[[T11], Callable[[T12], TResult]],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], TResult]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)

    _curried[f2] = f
    return f2


def curry12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, TResult](
    f: Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], TResult],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[[T11], Callable[[T12], TResult]],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: f(
                a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12
            )
        )
    else:
        return f2


def uncurry13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[
                                                [T11],
                                                Callable[[T12], Callable[[T13], TResult]],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], TResult]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
        a13: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)

    _curried[f2] = f
    return f2


def curry13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, TResult](
    f: Callable[[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], TResult],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[
                                            [T11],
                                            Callable[[T12], Callable[[T13], TResult]],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: lambda a13: f(
                a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13
            )
        )
    else:
        return f2


def uncurry14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[
                                                [T11],
                                                Callable[
                                                    [T12],
                                                    Callable[
                                                        [T13],
                                                        Callable[[T14], TResult],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[
    [
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
    ],
    TResult,
]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
        a13: Any,
        a14: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)

    _curried[f2] = f
    return f2


def curry14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, TResult](
    f: Callable[
        [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14],
        TResult,
    ],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[
                                            [T11],
                                            Callable[
                                                [T12],
                                                Callable[[T13], Callable[[T14], TResult]],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: lambda a13: lambda a14: f(
                a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14
            )
        )
    else:
        return f2


def uncurry15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[
                                                [T11],
                                                Callable[
                                                    [T12],
                                                    Callable[
                                                        [T13],
                                                        Callable[
                                                            [T14],
                                                            Callable[[T15], TResult],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[
    [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15],
    TResult,
]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
        a13: Any,
        a14: Any,
        a15: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)

    _curried[f2] = f
    return f2


def curry15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, TResult](
    f: Callable[
        [
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
        ],
        TResult,
    ],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[
                                            [T11],
                                            Callable[
                                                [T12],
                                                Callable[
                                                    [T13],
                                                    Callable[
                                                        [T14],
                                                        Callable[[T15], TResult],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: lambda a13: lambda a14: lambda a15: f(
                a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15
            )
        )
    else:
        return f2


def uncurry16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[
                                                [T11],
                                                Callable[
                                                    [T12],
                                                    Callable[
                                                        [T13],
                                                        Callable[
                                                            [T14],
                                                            Callable[
                                                                [T15],
                                                                Callable[[T16], TResult],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[
    [
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
    ],
    TResult,
]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
        a13: Any,
        a14: Any,
        a15: Any,
        a16: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)

    _curried[f2] = f
    return f2


def curry16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, TResult](
    f: Callable[
        [
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
        ],
        TResult,
    ],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[
                                            [T11],
                                            Callable[
                                                [T12],
                                                Callable[
                                                    [T13],
                                                    Callable[
                                                        [T14],
                                                        Callable[
                                                            [T15],
                                                            Callable[[T16], TResult],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: lambda a13: lambda a14: lambda a15: lambda a16: f(
                a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16
            )
        )
    else:
        return f2


def uncurry17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[
                                                [T11],
                                                Callable[
                                                    [T12],
                                                    Callable[
                                                        [T13],
                                                        Callable[
                                                            [T14],
                                                            Callable[
                                                                [T15],
                                                                Callable[
                                                                    [T16],
                                                                    Callable[[T17], TResult],
                                                                ],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[
    [
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
        T17,
    ],
    TResult,
]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
        a13: Any,
        a14: Any,
        a15: Any,
        a16: Any,
        a17: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)

    _curried[f2] = f
    return f2


def curry17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, TResult](
    f: Callable[
        [
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
        ],
        TResult,
    ],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[
                                            [T11],
                                            Callable[
                                                [T12],
                                                Callable[
                                                    [T13],
                                                    Callable[
                                                        [T14],
                                                        Callable[
                                                            [T15],
                                                            Callable[
                                                                [T16],
                                                                Callable[[T17], TResult],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: lambda a13: lambda a14: lambda a15: lambda a16: lambda a17: f(
                a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17
            )
        )
    else:
        return f2


def uncurry18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[
                                                [T11],
                                                Callable[
                                                    [T12],
                                                    Callable[
                                                        [T13],
                                                        Callable[
                                                            [T14],
                                                            Callable[
                                                                [T15],
                                                                Callable[
                                                                    [T16],
                                                                    Callable[
                                                                        [T17],
                                                                        Callable[
                                                                            [T18],
                                                                            TResult,
                                                                        ],
                                                                    ],
                                                                ],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[
    [
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
        T17,
        T18,
    ],
    TResult,
]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
        a13: Any,
        a14: Any,
        a15: Any,
        a16: Any,
        a17: Any,
        a18: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)

    _curried[f2] = f
    return f2


def curry18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, TResult](
    f: Callable[
        [
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
        ],
        TResult,
    ],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[
                                            [T11],
                                            Callable[
                                                [T12],
                                                Callable[
                                                    [T13],
                                                    Callable[
                                                        [T14],
                                                        Callable[
                                                            [T15],
                                                            Callable[
                                                                [T16],
                                                                Callable[
                                                                    [T17],
                                                                    Callable[[T18], TResult],
                                                                ],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: lambda a13: lambda a14: lambda a15: lambda a16: lambda a17: lambda a18: f(
                a1,
                a2,
                a3,
                a4,
                a5,
                a6,
                a7,
                a8,
                a9,
                a10,
                a11,
                a12,
                a13,
                a14,
                a15,
                a16,
                a17,
                a18,
            )
        )
    else:
        return f2


def uncurry19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[
                                                [T11],
                                                Callable[
                                                    [T12],
                                                    Callable[
                                                        [T13],
                                                        Callable[
                                                            [T14],
                                                            Callable[
                                                                [T15],
                                                                Callable[
                                                                    [T16],
                                                                    Callable[
                                                                        [T17],
                                                                        Callable[
                                                                            [T18],
                                                                            Callable[
                                                                                [T19],
                                                                                TResult,
                                                                            ],
                                                                        ],
                                                                    ],
                                                                ],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[
    [
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
        T17,
        T18,
        T19,
    ],
    TResult,
]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
        a13: Any,
        a14: Any,
        a15: Any,
        a16: Any,
        a17: Any,
        a18: Any,
        a19: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19)

    _curried[f2] = f
    return f2


def curry19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, TResult](
    f: Callable[
        [
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
        ],
        TResult,
    ],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[
                                            [T11],
                                            Callable[
                                                [T12],
                                                Callable[
                                                    [T13],
                                                    Callable[
                                                        [T14],
                                                        Callable[
                                                            [T15],
                                                            Callable[
                                                                [T16],
                                                                Callable[
                                                                    [T17],
                                                                    Callable[
                                                                        [T18],
                                                                        Callable[
                                                                            [T19],
                                                                            TResult,
                                                                        ],
                                                                    ],
                                                                ],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: lambda a13: lambda a14: lambda a15: lambda a16: lambda a17: lambda a18: lambda a19: f(
                a1,
                a2,
                a3,
                a4,
                a5,
                a6,
                a7,
                a8,
                a9,
                a10,
                a11,
                a12,
                a13,
                a14,
                a15,
                a16,
                a17,
                a18,
                a19,
            )
        )
    else:
        return f2


def uncurry20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, TResult](
    f: Callable[
        [T1],
        Callable[
            [T2],
            Callable[
                [T3],
                Callable[
                    [T4],
                    Callable[
                        [T5],
                        Callable[
                            [T6],
                            Callable[
                                [T7],
                                Callable[
                                    [T8],
                                    Callable[
                                        [T9],
                                        Callable[
                                            [T10],
                                            Callable[
                                                [T11],
                                                Callable[
                                                    [T12],
                                                    Callable[
                                                        [T13],
                                                        Callable[
                                                            [T14],
                                                            Callable[
                                                                [T15],
                                                                Callable[
                                                                    [T16],
                                                                    Callable[
                                                                        [T17],
                                                                        Callable[
                                                                            [T18],
                                                                            Callable[
                                                                                [T19],
                                                                                Callable[
                                                                                    [T20],
                                                                                    TResult,
                                                                                ],
                                                                            ],
                                                                        ],
                                                                    ],
                                                                ],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
) -> Callable[
    [
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        T16,
        T17,
        T18,
        T19,
        T20,
    ],
    TResult,
]:
    def f2(
        a1: Any,
        a2: Any,
        a3: Any,
        a4: Any,
        a5: Any,
        a6: Any,
        a7: Any,
        a8: Any,
        a9: Any,
        a10: Any,
        a11: Any,
        a12: Any,
        a13: Any,
        a14: Any,
        a15: Any,
        a16: Any,
        a17: Any,
        a18: Any,
        a19: Any,
        a20: Any,
    ) -> Any:
        return f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(a17)(a18)(a19)(a20)

    _curried[f2] = f
    return f2


def curry20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, TResult](
    f: Callable[
        [
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
        ],
        TResult,
    ],
) -> Callable[
    [T1],
    Callable[
        [T2],
        Callable[
            [T3],
            Callable[
                [T4],
                Callable[
                    [T5],
                    Callable[
                        [T6],
                        Callable[
                            [T7],
                            Callable[
                                [T8],
                                Callable[
                                    [T9],
                                    Callable[
                                        [T10],
                                        Callable[
                                            [T11],
                                            Callable[
                                                [T12],
                                                Callable[
                                                    [T13],
                                                    Callable[
                                                        [T14],
                                                        Callable[
                                                            [T15],
                                                            Callable[
                                                                [T16],
                                                                Callable[
                                                                    [T17],
                                                                    Callable[
                                                                        [T18],
                                                                        Callable[
                                                                            [T19],
                                                                            Callable[
                                                                                [T20],
                                                                                TResult,
                                                                            ],
                                                                        ],
                                                                    ],
                                                                ],
                                                            ],
                                                        ],
                                                    ],
                                                ],
                                            ],
                                        ],
                                    ],
                                ],
                            ],
                        ],
                    ],
                ],
            ],
        ],
    ],
]:
    f2 = _curried.get(f)
    if f2 is None:
        return (
            lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: lambda a9: lambda a10: lambda a11: lambda a12: lambda a13: lambda a14: lambda a15: lambda a16: lambda a17: lambda a18: lambda a19: lambda a20: f(
                a1,
                a2,
                a3,
                a4,
                a5,
                a6,
                a7,
                a8,
                a9,
                a10,
                a11,
                a12,
                a13,
                a14,
                a15,
                a16,
                a17,
                a18,
                a19,
                a20,
            )
        )
    else:
        return f2


def is_array_like(x: Any) -> bool:
    return isinstance(x, Array | list | tuple | set | bytes | bytearray)


def is_disposable(x: Any) -> bool:
    return x is not None and isinstance(x, IDisposable)


def dispose(x: IDisposable | AbstractContextManager[Any]) -> None:
    """Helper to dispose objects.

    Also tries to call `__exit__` if the object turns out to be a Python resource manager.
    For more info see: https://www.python.org/dev/peps/pep-0310/
    """
    try:
        x.Dispose()  # type: ignore
    except AttributeError as ex:
        try:
            x.__exit__(None, None, None)
        except AttributeError:
            raise ex


class HashCode(Protocol):
    def GetHashCode(self) -> int32: ...


def is_hashable(x: Any) -> TypeGuard[HashCode]:
    return hasattr(x, "GetHashCode")


def is_hashable_py(x: Any) -> bool:
    return hasattr(x, "__hash__") and callable(x.__hash__)


def to_iterator[T](en: IEnumerator[T]) -> IEnumerator[T]:
    class Iterator:
        def __iter__(self):
            return self

        def __next__(self):
            has_next = en.System_Collections_IEnumerator_MoveNext()
            if not has_next:
                raise StopIteration
            return en.System_Collections_IEnumerator_get_Current()

    return Enumerator(Iterator())


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


def array_hash(xs: list[Any]) -> int32:
    hashes: list[int32] = []
    for x in xs:
        hashes.append(structural_hash(x))

    return combine_hash_codes(hashes)


def physical_hash(x: Any) -> int32:
    return number_hash(ObjectRef.id(x))


def round(value: float64, digits: int = 0) -> float64:
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

    Note: We inherit from ABCMeta to be compatible when combined with classes that also
    inherit from ABC, such as IDisposable.
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

    return map(lambda x: int32(x), builtins.range(start, adjusted_stop, step))


__all__ = [
    "ObjectRef",
    "PlatformID",
    "StaticLazyProperty",
    "StaticProperty",
    "StaticPropertyBase",
    "StaticPropertyMeta",
    "array_hash",
    "copy_to_array",
    "curry2",
    "curry3",
    "curry4",
    "curry5",
    "curry6",
    "curry7",
    "curry8",
    "curry9",
    "curry10",
    "escape_data_string",
    "escape_uri_string",
    "get_platform",
    "identity_hash",
    "ignore",
    "number_hash",
    "physical_hash",
    "randint",
    "range",
    "round",
    "uncurry2",
    "uncurry3",
    "uncurry4",
    "uncurry5",
    "uncurry6",
    "uncurry7",
    "uncurry8",
    "uncurry9",
    "uncurry10",
    "unescape_data_string",
]
