from abc import ABC, abstractmethod
import functools
from threading import RLock
from typing import Callable, Iterable, List, TypeVar, Optional

T = TypeVar("T")


class ObjectDisposedException(Exception):
    def __init__(self):
        super().__init__("Cannot access a disposed object")


class IDisposable:
    @abstractmethod
    def dispose(self) -> None:
        ...

    def __enter__(self):
        """Enter context management."""
        return self

    def __exit__(self, exctype, excinst, exctb):
        """Exit context management."""

        self.dispose()
        return False

    @staticmethod
    def create(action):
        """Create disposable from action. Will call action when
        disposed."""
        return AnonymousDisposable(action)


class AnonymousDisposable(IDisposable):
    def __init__(self, action):
        self._is_disposed = False
        self._action = action
        self._lock = RLock()

    def dispose(self) -> None:
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

    # def Equals(self, other):
    #     return self.Equals(other)

    @abstractmethod
    def __eq__(self, other):
        return NotImplemented

    @abstractmethod
    def __hash__(self):
        raise NotImplementedError


class IComparable(IEquatable):
    def CompareTo(self, other):
        if self < other:
            return -1
        elif self == other:
            return 0
        return 1

    @abstractmethod
    def __lt__(self, other):
        raise NotImplementedError


def equals(a, b):
    return a == b


def compare(a, b):
    if a == b:
        return 0
    if a < b:
        return -1
    return 1


def equalArraysWith(x, y, eq):
    if x is None:
        return y is None
    if y is None:
        return False

    if len(x) != len(y):
        return False

    return eq(x, y)


def equalArrays(x, y):
    return equalArraysWith(x, y, equals)


def comparePrimitives(x, y) -> int:
    return 0 if x == y else (-1 if x < y else 1)


def min(comparer, x, y):
    return x if comparer(x, y) < 0 else y


def max(comparer, x, y):
    return x if comparer(x, y) > 0 else y


def assertEqual(actual, expected, msg=None) -> None:
    if actual != expected:
        raise Exception(msg or f"Expected: ${expected} - Actual: ${actual}")


def assertNotEqual(actual: T, expected: T, msg: Optional[str] = None) -> None:
    if actual == expected:
        raise Exception(msg or f"Expected: ${expected} - Actual: ${actual}")


def createAtom(value=None):
    atom = value

    def _(value=None, isSetter=None):
        nonlocal atom

        if not isSetter:
            return atom
        else:
            atom = value
            return None

    return _


def createObj(fields):
    obj = {}

    for k, v in fields:
        obj[k] = v

    return obj


def int16ToString(i, radix=10):
    if radix == 10:
        return "{:d}".format(i)
    if radix == 16:
        return "{:x}".format(i)
    if radix == 2:
        return "{:b}".format(i)
    return str(i)


def int32ToString(i: int, radix: int = 10) -> str:
    if radix == 10:
        return "{:d}".format(i)
    if radix == 16:
        return "{:x}".format(i)
    if radix == 2:
        return "{:b}".format(i)
    return str(i)


def clear(col):
    if isinstance(col, List):
        col.clear()


class IEnumerator(IDisposable):
    @abstractmethod
    def Current(self):
        ...

    @abstractmethod
    def MoveNext(self):
        ...

    @abstractmethod
    def Reset(self):
        ...

    @abstractmethod
    def Dispose(self):
        ...

    def __getattr__(self, name):
        return {
            "System_Collections_Generic_IEnumerator_1_get_Current": self.Current,
            "System_Collections.IEnumerator_get_Current": self.Current,
            "System_Collections_IEnumerator_MoveNext": self.MoveNext,
            "System_Collections.IEnumerator_Reset": self.Reset,
        }[name]


class IEnumerable(Iterable):
    @abstractmethod
    def GetEnumerator(self):
        ...


class Enumerator(IEnumerator):
    def __init__(self, iter) -> None:
        self.iter = iter
        self.current = None

    def Current(self):
        if self.current is not None:
            return self.current
        return None

    def MoveNext(self):
        try:
            cur = next(self.iter)
            self.current = cur
            return True
        except StopIteration:
            return False

    def Reset(self):
        raise Exception("Python iterators cannot be reset")

    def Dispose(self):
        return


def getEnumerator(o):
    attr = getattr(o, "GetEnumerator", None)
    if attr:
        return attr()
    else:
        return Enumerator(iter(o))


CURRIED_KEY = "__CURRIED__"


def uncurry(arity: int, f: Callable):
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
        uncurriedFn = fns[arity]
    except Exception:
        raise Exception(f"Uncurrying to more than 8-arity is not supported: {arity}")

    setattr(f, CURRIED_KEY, f)
    return uncurriedFn


def curry(arity: int, f: Callable) -> Callable:
    if f is None or arity == 1:
        return f

    if hasattr(f, CURRIED_KEY):
        return getattr(f, CURRIED_KEY)

    if arity == 2:
        return lambda a1: lambda a2: f(a1, a2)
    elif arity == 3:
        return lambda a1: lambda a2: lambda a3: f(a1, a2, a3)
    elif arity == 4:
        return lambda a1: lambda a2: lambda a3: lambda a4: f(a1, a2, a3, a4)
    elif arity == 4:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: f(a1, a2, a3, a4, a5)
    elif arity == 6:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: f(a1, a2, a3, a4, a5, a6)
    elif arity == 7:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: f(
            a1, a2, a3, a4, a5, a6, a7
        )
    elif arity == 8:
        return lambda a1: lambda a2: lambda a3: lambda a4: lambda a5: lambda a6: lambda a7: lambda a8: f(
            a1, a2, a3, a4, a5, a6, a7, a8
        )
    else:
        raise Exception("Currying to more than 8-arity is not supported: %d" % arity)


def isArrayLike(x):
    return hasattr(x, "__len__")


def isDisposable(x):
    return x is not None and isinstance(x, IDisposable)


def toIterator(en):
    #print("toIterator: ", en)

    class Iterator:
        def __iter__(self):
            return self

        def __next__(self):
            has_next = getattr(en, "System_Collections_IEnumerator_MoveNext")()
            if not has_next:
                raise StopIteration
            return getattr(en, "System_Collections_IEnumerator_get_Current")()

    return Iterator()


def stringHash(s):
    h = 5381
    for c in s:
        h = (h * 33) ^ ord(c)

    return h


def numberHash(x):
    return x * 2654435761 | 0


def combineHashCodes(hashes):
    if not hashes:
        return 0

    return functools.reduce(lambda h1, h2: ((h1 << 5) + h1) ^ h2, hashes)


def structuralHash(x):
    return hash(x)


def arrayHash(xs):
    hashes = []
    for i, x in enumerate(xs):
        hashes.append(structuralHash(x))

    return combineHashCodes(hashes)


def physicalHash(x):
    return hash(x)
