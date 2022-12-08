from __future__ import annotations
import builtins
import functools
import math
import platform
import random
import re

from abc import ABC, abstractmethod
from array import array
from enum import IntEnum
from inspect import Parameter, signature
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
    MutableSequence,
    Optional,
    Protocol,
    Sequence,
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
_T_in = TypeVar("_T_in", contravariant=True)
_T_out = TypeVar("_T_out", covariant=True)
_Key = TypeVar("_Key")
_Value = TypeVar("_Value")
_TSupportsLessThan = TypeVar("_TSupportsLessThan", bound=SupportsLessThan)

Array = MutableSequence


class ObjectDisposedException(Exception):
    def __init__(self):
        super().__init__("Cannot access a disposed object")


class IDisposable(ABC):
    """IDisposable interface.

    Note currently not a protocol since it also impmelents resource
    management and needs to be inherited from.
    """

    __slots__ = ()

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
    __slots__ = "_is_disposed", "_action", "_lock"

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
    @abstractmethod
    def __eq__(self, other: Any) -> bool:
        raise NotImplementedError

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class IComparable(IEquatable, Protocol):
    @abstractmethod
    def __cmp__(self, __other: Any) -> int:
        raise NotImplementedError

    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        raise NotImplementedError


class IComparable_1(Generic[_T_in], IEquatable, Protocol):
    @abstractmethod
    def __cmp__(self, __other: _T_in) -> int:
        raise NotImplementedError

    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        raise NotImplementedError


class IComparer(Protocol):
    """Defines a method that a type implements to compare two objects.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.icomparer-1
    """

    @property
    @abstractmethod
    def Compare(self) -> Callable[[_T_in, _T_in], int]:
        ...


class IComparer_1(Generic[_T_in], Protocol):
    """Defines a method that a type implements to compare two objects.

    https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.icomparer-1
    """

    @property
    @abstractmethod
    def Compare(self) -> Callable[[_T_in, _T_in], int]:
        ...


class IEqualityComparer(Protocol):
    def Equals(self, __x: _T_in, __y: _T_in) -> bool:
        return self.System_Collections_IEqualityComparer_Equals541DA560(__x, __y)

    def GetHashCode(self) -> int:
        return self.System_Collections_IEqualityComparer_GetHashCode4E60E31B()

    @abstractmethod
    def System_Collections_IEqualityComparer_Equals541DA560(
        self, x: Any = None, y: Any = None
    ) -> bool:
        raise NotImplementedError

    @abstractmethod
    def System_Collections_IEqualityComparer_GetHashCode4E60E31B(
        self, x_1: Any = None
    ) -> int:
        raise NotImplementedError


class IEqualityComparer_1(Generic[_T_in], Protocol):
    def Equals(self, __x: _T_in, __y: _T_in) -> bool:
        raise NotImplementedError

    def GetHashCode(self) -> int:
        raise NotImplementedError


class IStructuralEquatable(Protocol):
    @abstractmethod
    def Equals(self, other: Any, comparer: IEqualityComparer) -> bool:
        return NotImplemented

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class IStructuralComparable(Protocol):
    @abstractmethod
    def __cmp__(self, other: Any, comparer: IComparer) -> int:
        raise NotImplementedError


class DateKind(IntEnum):
    Unspecified = 0
    UTC = 1
    Local = 2


def equals(a: Any, b: Any) -> bool:
    if a is b:
        return True

    if is_array_like(a):
        return equal_arrays(a, b)

    return a == b


def is_comparable(x: Any) -> bool:
    return hasattr(x, "__cmp__") and callable(x.__cmp__)


def is_equatable(x: Any) -> bool:
    return hasattr(x, "__eq__") and callable(x.__eq__)


def is_iterable(x: Any) -> bool:
    return isinstance(x, Iterable)


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
        return a.__cmp__(b)

    if isinstance(a, dict):
        return compare_dicts(cast(Dict[str, Any], a), b)

    if isinstance(a, List):
        return compare_arrays(cast(List[Any], a), b)

    if is_equatable(a) and a == b:
        return 0

    if hasattr(a, "__lt__") and callable(a.__lt__) and a < b:
        return -1

    return 1


def equal_arrays_with(
    xs: Optional[Sequence[_T]], ys: Optional[Sequence[_T]], eq: Callable[[_T, _T], bool]
) -> bool:
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


def equal_arrays(x: Sequence[_T], y: Sequence[_T]) -> bool:
    return equal_arrays_with(x, y, equals)


def compare_primitives(x: _TSupportsLessThan, y: _TSupportsLessThan) -> int:
    return 0 if x == y else (-1 if x < y else 1)


def min(comparer: Callable[[_T, _T], int], x: _T, y: _T) -> _T:
    return x if comparer(x, y) < 0 else y


def max(comparer: Callable[[_T, _T], int], x: _T, y: _T) -> _T:
    return x if comparer(x, y) > 0 else y


def clamp(comparer: Callable[[_T, _T], int], value: _T, min: _T, max: _T):
    # return (comparer(value, min) < 0) ? min : (comparer(value, max) > 0) ? max : value;
    return (
        min
        if (comparer(value, min) < 0)
        else max
        if comparer(value, max) > 0
        else value
    )


def assert_equal(actual: Any, expected: Any, msg: Optional[str] = None) -> None:
    if not equals(actual, expected):
        raise Exception(msg or f"Expected: ${expected} - Actual: ${actual}")


def assert_not_equal(actual: _T, expected: _T, msg: Optional[str] = None) -> None:
    if equals(actual, expected):
        raise Exception(msg or f"Expected: ${expected} - Actual: ${actual}")


MAX_LOCKS = 1024


def lock(lock_obj: Any, fn: Callable[[], _T]) -> _T:
    @functools.lru_cache(maxsize=MAX_LOCKS)
    def get_lock(n: int) -> RLock:
        return RLock()

    lock = get_lock(id(lock_obj))
    with lock:
        return fn()


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


class Atom(Generic[_T], Protocol):
    def __call__(self, *value: _T) -> Optional[_T]:
        ...


def create_atom(value: _T) -> Atom[_T]:
    atom = value

    def wrapper(*value: _T) -> Optional[_T]:
        nonlocal atom

        if len(value) == 0:
            return atom

        atom = value[0]
        return None

    return wrapper


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


def clear(col: Optional[Union[Dict[Any, Any], List[Any]]]) -> None:
    if isinstance(col, (List, Dict)):
        col.clear()


class IEnumerator(Iterator[_T], IDisposable):
    __slots__ = ()

    def Current(self) -> _T:
        return self.System_Collections_Generic_IEnumerator_1_get_Current()

    def MoveNext(self) -> bool:
        return self.System_Collections_IEnumerator_MoveNext()

    def Reset(self) -> None:
        return self.System_Collections_IEnumerator_Reset()

    @abstractmethod
    def System_Collections_Generic_IEnumerator_1_get_Current(self) -> _T:
        ...

    def System_Collections_IEnumerator_get_Current(self) -> Any:
        return self.System_Collections_Generic_IEnumerator_1_get_Current()

    @abstractmethod
    def System_Collections_IEnumerator_MoveNext(self) -> bool:
        ...

    @abstractmethod
    def System_Collections_IEnumerator_Reset(self) -> None:
        ...

    def __iter__(self) -> Iterator[_T]:
        return self

    def __next__(self) -> _T:
        if not self.MoveNext():
            raise StopIteration
        return self.Current()


class IEnumerable(Iterable[Any], Protocol):
    __slots__ = ()

    @abstractmethod
    def GetEnumerator(self) -> IEnumerator[Any]:
        ...

    def __iter__(self) -> Iterator[Any]:
        return self.GetEnumerator()


class IEnumerable_1(Iterable[_T], Protocol):
    __slots__ = ()

    @abstractmethod
    def GetEnumerator(self) -> IEnumerator[_T]:
        ...

    def __iter__(self) -> Iterator[_T]:
        return self.GetEnumerator()


class ICollection(IEnumerable_1[_T], Protocol):
    ...


class IDictionary(ICollection[Tuple[_Key, _Value]], Protocol):
    @abstractmethod
    def keys(self) -> IEnumerable_1[_Key]:
        ...

    def values(self) -> IEnumerable_1[_Value]:
        ...


class Enumerator(IEnumerator[_T]):
    __slots__ = "iter", "current"

    def __init__(self, iter: Iterator[_T]) -> None:
        self.iter = iter
        self.current = None

    def System_Collections_Generic_IEnumerator_1_get_Current(self) -> _T:
        if self.current is not None:
            return self.current
        return None  # type: ignore

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

    def __next__(self) -> _T:
        return next(self.iter)

    def __iter__(self) -> Iterator[_T]:
        return self


class Enumerable(IEnumerable_1[_T]):
    __slots__ = "xs"

    def __init__(self, xs: Iterable[_T]) -> None:
        self.xs = xs

    def GetEnumerator(self) -> IEnumerator[_T]:
        return Enumerator(iter(self.xs))

    def __iter__(self) -> Iterator[_T]:
        return iter(self.xs)


def to_enumerable(e: Iterable[_T]) -> IEnumerable_1[_T]:
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


CURRIED_KEY = "__CURRIED__"


def num_args(fn: Callable[..., Any]) -> int:
    return len(
        [
            param
            for param in signature(fn).parameters.values()
            if param.kind == Parameter.POSITIONAL_ONLY
        ]
    )


def uncurry(arity: int, fn: Callable[..., Callable[..., Any]]) -> Callable[..., Any]:
    # Check if function is option with None value
    if fn is None or num_args(fn) == 1:
        return fn

    def uncurried(*args: Any):
        res = fn
        for i in range(arity):
            res = res(args[i])

        return res

    setattr(uncurried, CURRIED_KEY, fn)
    return uncurried


def _curry(*args: Any, arity: int, fn: Callable[..., Any]) -> Callable[..., Any]:
    def curried(arg: Any = None) -> Callable[..., Any]:
        if arity == 1:
            return fn(*args, arg)

        # Note it's important to generate a new args array every time
        # because a partially applied function can be run multiple times
        return _curry(*args, arg, arity=arity - 1, fn=fn)

    return curried


def curry(arity: int, fn: Callable[..., Any]) -> Callable[..., Callable[..., Any]]:
    if fn is None or num_args(fn) == 1:
        return fn

    if hasattr(fn, CURRIED_KEY):
        return getattr(fn, CURRIED_KEY)

    return _curry(arity=arity, fn=fn)


def check_arity(arity: int, fn: Callable[..., Any]) -> Callable[..., Any]:
    sig = signature(fn)
    num_args = len(sig.parameters)

    def gn(*args1: Any) -> Callable[..., Any]:
        def hn(*args2: Any) -> Any:
            return fn(*args1, *args2)

        return hn

    if num_args > arity:
        return gn
    return fn


def partial_apply(
    arity: int, fn: Callable[..., Any], args: List[Any]
) -> Callable[..., Any]:
    if fn is None:
        raise ValueError("partially_apply: Missing function")

    if hasattr(fn, CURRIED_KEY):
        fn = getattr(fn, CURRIED_KEY)

        for arg in args:
            fn = fn(arg)
        return fn
    else:
        return _curry(*args, arity=arity, fn=fn)


def is_array_like(x: Any) -> bool:
    return isinstance(x, (list, tuple, set, array, bytes, bytearray))


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


def to_iterator(en: IEnumerator[_T]) -> IEnumerator[_T]:
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
    return (
        0
        if x is None
        else x.GetHashCode()
        if is_hashable(x)
        else number_hash(ObjectRef.id(x))
    )


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
    r = (
        (i if (i % 2 == 0) else i + 1)
        if (f > 0.5 - e and f < 0.5 + e)
        else builtins.round(n)
    )
    return r / m if digits else r


def randint(a: int, b: int) -> int:
    return random.randint(a, b - 1)


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


def copy_to_array(
    src: Array[_T], srci: int, trg: Array[_T], trgi: int, cnt: int
) -> None:
    for i in range(0, cnt, 1):
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
