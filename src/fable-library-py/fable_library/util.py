import builtins
import functools
import math
import re
from abc import ABC, abstractmethod
from enum import Enum
from threading import RLock
from types import TracebackType
from typing import (
    Any,
    Callable,
    ContextManager,
    Dict,
    Generic,
    Iterable,
    Iterator,
    List,
    Optional,
    Protocol,
    Sized,
    Tuple,
    Type,
    TypeVar,
    Union,
    cast,
)
from urllib.parse import quote, unquote


class SupportsLessThan(Protocol):
    @abstractmethod
    def __lt__(self, __other: Any) -> bool:
        raise NotImplementedError


_T = TypeVar("_T")
_TSupportsLessThan = TypeVar("_TSupportsLessThan", bound=SupportsLessThan)


class ObjectDisposedException(Exception):
    def __init__(self):
        super().__init__("Cannot access a disposed object")


class IDisposable:
    @abstractmethod
    def Dispose(self) -> None:
        ...

    def __enter__(self):
        """Enter context management."""
        return self

    def __exit__(
        self,
        exctype: Optional[Type[BaseException]],
        excinst: Optional[BaseException],
        exctb: Optional[TracebackType],
    ) -> bool:
        """Exit context management."""

        self.Dispose()
        return False

    @staticmethod
    def create(action: Callable[[], None]):
        """Create disposable from action. Will call action when
        disposed."""
        return AnonymousDisposable(action)


Disposable = TypeVar("Disposable", bound=IDisposable)


class AnonymousDisposable(IDisposable):
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


class IEquatable(ABC):
    def GetHashCode(self):
        return hash(self)

    @abstractmethod
    def __eq__(self, other: Any) -> bool:
        return NotImplemented

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class IComparable(IEquatable):
    def CompareTo(self, other: Any) -> int:
        if self < other:
            return -1
        elif self == other:
            return 0
        return 1

    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        raise NotImplementedError


class IComparer(Generic[_T]):
    """Defines a method that a type implements to compare two objects.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.icomparer-1
    """

    @abstractmethod
    def Compare(self, y: _T) -> int:
        ...


class IEqualityComparer(Generic[_T]):
    def Equals(self, y: _T) -> bool:
        return self == y

    def GetHashCode(self) -> int:
        return hash(self)

    @abstractmethod
    def __eq__(self, other: Any) -> bool:
        return NotImplemented

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class DateKind(Enum):
    Unspecified = 0
    UTC = 1
    Local = 2


def equals(a: Any, b: Any) -> bool:
    return a == b


def is_comparable(x: Any) -> bool:
    return hasattr(x, "CompareTo") and callable(x.CompareTo)


def compare_dicts(x: Dict[str, Any], y: Dict[str, Any]) -> int:
    """Compare Python dicts with string keys.

    Python cannot do this natively.
    """
    x_keys = x.keys()
    y_keys = y.keys()

    if len(x_keys) != len(y_keys):
        return -1 if len(x_keys) < len(y_keys) else 1

    x_keys = sorted(x_keys)
    y_keys = sorted(y_keys)

    j = 0
    for i, key in enumerate(x_keys):
        if key != y_keys[i]:
            return -1 if key < y_keys[i] else 1
        else:
            j = compare(x[key], y[key])
            if j != 0:
                return j

    return 0


def compare_arrays(xs: List[Any], ys: List[Any]):
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


def compare(a: Any, b: Any) -> int:
    if a is b:
        return 0

    if a is None:
        return -1 if b else 0

    if b is None:
        return 1 if a else 0

    if is_comparable(a):
        return a.CompareTo(b)

    if isinstance(a, dict):
        return compare_dicts(cast(Dict[str, Any], a), b)

    if isinstance(a, List):
        return compare_arrays(cast(List[Any], a), b)

    # TODO: the last tests here are not reliable since the methods may rise NotImplementedError
    if hasattr(a, "__eq__") and callable(a.__eq__) and a == b:
        return 0

    if hasattr(a, "__lt__") and callable(a.__lt__) and a < b:
        return -1

    return 1


def equal_arrays_with(xs: List[_T], ys: List[_T], eq: Callable[[_T, _T], bool]) -> bool:
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


def equal_arrays(x: List[_T], y: List[_T]) -> bool:
    return equal_arrays_with(x, y, equals)


def compare_primitives(x: _TSupportsLessThan, y: _TSupportsLessThan) -> int:
    return 0 if x == y else (-1 if x < y else 1)


def min(comparer: Callable[[_T, _T], int], x: _T, y: _T) -> _T:
    return x if comparer(x, y) < 0 else y


def max(comparer: Callable[[_T, _T], int], x: _T, y: _T) -> _T:
    return x if comparer(x, y) > 0 else y


def clamp(comparer: Callable[[_T, _T], int], value: _T, min: _T, max: _T):
    # return (comparer(value, min) < 0) ? min : (comparer(value, max) > 0) ? max : value;
    return min if (comparer(value, min) < 0) else max if comparer(value, max) > 0 else value


def assert_equal(actual: Any, expected: Any, msg: Optional[str] = None) -> None:
    if actual != expected:
        raise Exception(msg or f"Expected: ${expected} - Actual: ${actual}")


def assert_not_equal(actual: _T, expected: _T, msg: Optional[str] = None) -> None:
    if actual == expected:
        raise Exception(msg or f"Expected: ${expected} - Actual: ${actual}")


class Lazy(Generic[_T]):
    def __init__(self, factory: Callable[[], _T]):
        self.factory = factory
        self.is_value_created: bool = False
        self.created_value: Optional[_T] = None

    @property
    def Value(self) -> _T:
        if not self.is_value_created:
            self.created_value = self.factory()
            self.is_value_created = True

        assert self.created_value is not None
        return self.created_value

    @property
    def IsValueCreated(self):
        return self.is_value_created


def lazy_from_value(v: _T) -> Lazy[_T]:
    return Lazy(lambda: v)


def create_atom(value: Optional[_T] = None) -> Callable[[Optional[_T], Optional[Callable[[Any], None]]], _T]:
    atom = value

    def _(value: Optional[_T] = None, isSetter: Optional[Callable[[_T], None]] = None):
        nonlocal atom

        if not isSetter:
            return atom
        else:
            atom = value
            return None

    return _


def create_obj(fields: List[Tuple[Any, Any]]):
    return dict(fields)


def tohex(val: int, nbits: Optional[int] = None) -> str:
    if nbits:
        val = (val + (1 << nbits)) % (1 << nbits)
    return "{:x}".format(val)


def int_to_string(i: int, radix: int = 10, bitsize: Optional[int] = None) -> str:
    if radix == 10:
        return "{:d}".format(i)
    if radix == 16:
        return tohex(i, bitsize)
    if radix == 2:
        return "{:b}".format(i)
    if radix == 8:
        return "{:o}".format(i)
    return str(i)


def int8_to_string(i: int, radix: int = 10, bitsize: Optional[int] = None) -> str:
    return int_to_string(i, radix, 8)


def int16_to_string(i: int, radix: int = 10, bitsize: Optional[int] = None) -> str:
    return int_to_string(i, radix, 16)


def int32_to_string(i: int, radix: int = 10, bitsize: Optional[int] = None) -> str:
    return int_to_string(i, radix, 32)


def int64_to_string(i: int, radix: int = 10, bitsize: Optional[int] = None) -> str:
    return int_to_string(i, radix, 64)


def count(col: Iterable[Any]) -> int:
    if isinstance(col, Sized):
        return len(col)

    count = 0
    for _ in col:
        count += 1

    return count


def clear(col: Optional[List[Any]]) -> None:
    if isinstance(col, List):
        col.clear()


class IEnumerator(Iterator[_T], IDisposable):
    def Current(self) -> _T:
        return self.System_Collections_Generic_IEnumerator_00601_get_Current()

    def MoveNext(self) -> bool:
        return self.System_Collections_IEnumerator_MoveNext()

    def Reset(self) -> None:
        return self.System_Collections_IEnumerator_Reset()

    @abstractmethod
    def System_Collections_Generic_IEnumerator_00601_get_Current(self) -> _T:
        ...

    def System_Collections_IEnumerator_get_Current(self) -> Any:
        return self.System_Collections_Generic_IEnumerator_00601_get_Current()

    @abstractmethod
    def System_Collections_IEnumerator_MoveNext(self) -> bool:
        ...

    @abstractmethod
    def System_Collections_IEnumerator_Reset(self) -> None:
        ...

    def __next__(self) -> _T:
        if not self.MoveNext():
            raise StopIteration
        return self.Current()


class IEnumerable(Iterable[_T]):
    @abstractmethod
    def GetEnumerator(self) -> IEnumerator[_T]:
        ...

    def __iter__(self) -> Iterator[_T]:
        return self.GetEnumerator()


class ICollection(IEnumerable[_T]):
    pass


class Enumerator(IEnumerator[_T]):
    def __init__(self, iter: Iterator[_T]) -> None:
        self.iter = iter
        self.current = None

    def System_Collections_Generic_IEnumerator_00601_get_Current(self):
        if self.current is not None:
            return self.current
        return None

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


def get_enumerator(o: Iterable[_T]) -> Enumerator[_T]:
    attr = getattr(o, "GetEnumerator", None)
    if attr:
        return attr()
    else:
        return Enumerator(iter(o))


CURRIED_KEY = "__CURRIED__"


def uncurry(arity: int, f: Callable[..., Callable[..., Any]]) -> Callable[..., Any]:
    # f may be a function option with None value
    if f is None:
        return f

    fns = {
        2: lambda a1, a2: f(a1)(a2),
        3: lambda a1, a2, a3: f(a1)(a2)(a3),
        4: lambda a1, a2, a3, a4: f(a1)(a2)(a3)(a4),
        5: lambda a1, a2, a3, a4, a5: f(a1)(a2)(a3)(a4)(a5),
        6: lambda a1, a2, a3, a4, a5, a6: f(a1)(a2)(a3)(a4)(a5)(a6),
        7: lambda a1, a2, a3, a4, a5, a6, a7: f(a1)(a2)(a3)(a4)(a5)(a6)(a7),
        8: lambda a1, a2, a3, a4, a5, a6, a7, a8: f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8),
    }

    try:
        uncurried_fn = cast(Callable[..., Any], fns[arity])
    except Exception:
        raise Exception(f"Uncurrying to more than 8-arity is not supported: {arity}")

    setattr(f, CURRIED_KEY, f)
    return uncurried_fn


def curry(arity: int, fn: Callable[..., Any]) -> Callable[..., Callable[..., Any]]:
    if fn is None or arity == 1:
        return fn

    if hasattr(fn, CURRIED_KEY):
        return getattr(fn, CURRIED_KEY)

    if arity == 2:
        return lambda a1: lambda a2: fn(a1, a2)
    elif arity == 3:
        return lambda a1: lambda a2: lambda a3: fn(a1, a2, a3)
    elif arity == 4:
        return lambda a1: lambda a2: lambda a3: lambda a4: fn(a1, a2, a3, a4)
    elif arity == 5:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: fn(a1, a2, a3, a4, a5)
    elif arity == 6:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: fn(a1, a2, a3, a4, a5, a6)
    elif arity == 7:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: fn(
            a1, a2, a3, a4, a5, a6, a7
        )
    elif arity == 8:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: fn(
            a1, a2, a3, a4, a5, a6, a7, a8
        )
    else:
        raise Exception("Currying to more than 8-arity is not supported: %d" % arity)


def partial_apply(arity: int, fn: Callable[..., Any], args: List[Any]) -> Callable[..., Any]:
    if not fn:
        raise ValueError("partially_apply: Missing function")

    if hasattr(fn, CURRIED_KEY):
        fn = getattr(fn, CURRIED_KEY)

        for arg in args:
            fn = fn(arg)

        return fn

    if arity == 1:
        return lambda a1: fn(*args, a1)
    if arity == 2:
        return lambda a1: lambda a2: fn(*args, a1, a2)
    if arity == 3:
        return lambda a1: lambda a2: lambda a3: fn(*args, a1, a2, a3)
    if arity == 4:
        return lambda a1: lambda a2: lambda a3: lambda a4: fn(*args, a1, a2, a3, a4)
    if arity == 5:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: fn(*args, a1, a2, a3, a4, a5)
    if arity == 6:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: fn(*args, a1, a2, a3, a4, a5, a6)
    if arity == 7:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: fn(
            *args, a1, a2, a3, a4, a5, a6, a7
        )
    if arity == 8:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: fn(
            *args, a1, a2, a3, a4, a5, a6, a7, a8
        )
    raise ValueError(f"Partially applying to more than 8-arity is not supported: {arity}")


def is_array_like(x: Any) -> bool:
    return hasattr(x, "__len__") and callable(x.__len__)


def is_disposable(x: Any) -> bool:
    return x is not None and isinstance(x, IDisposable)


def dispose(x: Union[Disposable, ContextManager[Any]]) -> None:
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


def is_hashable(x: Any) -> bool:
    return hasattr(x, "GetHashCode")


def is_hashable_py(x: Any) -> bool:
    return hasattr(x, "__hash__") and callable(x.__hash__)


def to_iterator(en: IEnumerator[_T]) -> Iterator[_T]:
    class Iterator:
        def __iter__(self):
            return self

        def __next__(self):
            has_next = getattr(en, "System_Collections_IEnumerator_MoveNext")()
            if not has_next:
                raise StopIteration
            return getattr(en, "System_Collections_IEnumerator_get_Current")()

    return Iterator()


class ObjectRef:
    id_map: Dict[int, int] = dict()
    count = 0

    @staticmethod
    def id(o: Any) -> int:
        _id = id(o)
        if not _id in ObjectRef.id_map:
            count = ObjectRef.count + 1
            ObjectRef.id_map[_id] = count

        return ObjectRef.id_map[_id]


def safe_hash(x: Any) -> int:
    return 0 if x is None else x.GetHashCode() if is_hashable(x) else number_hash(ObjectRef.id(x))


def string_hash(s: str) -> int:
    h = 5381
    for c in s:
        h = (h * 33) ^ ord(c)

    return h


def number_hash(x: int) -> int:
    return x * 2654435761 | 0


def identity_hash(x: Any) -> int:
    if x is None:
        return 0

    if is_hashable(x):
        return x.GetHashCode()

    if is_hashable_py(x):
        return hash(x)

    return physical_hash(x)


def combine_hash_codes(hashes: List[int]) -> int:
    if not hashes:
        return 0

    return functools.reduce(lambda h1, h2: ((h1 << 5) + h1) ^ h2, hashes)


def structural_hash(x: Any) -> int:
    return hash(x)


def array_hash(xs: List[Any]) -> int:
    hashes: List[int] = []
    for x in xs:
        hashes.append(structural_hash(x))

    return combine_hash_codes(hashes)


def physical_hash(x: Any) -> int:
    if hasattr(x, "__hash__") and callable(x.__hash__):
        return hash(x)

    return number_hash(ObjectRef.id(x))


def round(value: float, digits: int = 0):
    m = pow(10, digits)
    n = +(value * m if digits else value)
    i = math.floor(n)
    f = n - i
    e = 1e-8
    r = (i if (i % 2 == 0) else i + 1) if (f > 0.5 - e and f < 0.5 + e) else builtins.round(n)
    return r / m if digits else r


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
