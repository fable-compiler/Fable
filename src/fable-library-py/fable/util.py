from abc import ABC, abstractmethod
from typing import Callable, Generic, Iterable, Tuple, TypeVar, Any, Optional

T = TypeVar("T")


class IEquatable(Generic[T], ABC):
    def GetHashCode(self) -> int:
        return self.GetHashCode()

    def Equals(self, other: T) -> bool:
        return self.Equals(other)

    @abstractmethod
    def __eq__(self, other: Any) -> bool:
        return NotImplemented

    @abstractmethod
    def __hash__(self) -> int:
        raise NotImplementedError


class IComparable(IEquatable[T]):
    def CompareTo(self, other: T) -> int:
        if self < other:
            return -1
        elif self == other:
            return 0
        return 1

    @abstractmethod
    def __lt__(self, other: Any) -> bool:
        raise NotImplementedError


def equals(a, b):
    return a == b


def assertEqual(actual: T, expected: T, msg: Optional[str] = None) -> None:
    if actual != expected:
        raise Exception(msg or f"Expected: ${expected} - Actual: ${actual}")


def assertNotEqual(actual: T, expected: T, msg: Optional[str] = None) -> None:
    if actual == expected:
        raise Exception(msg or f"Expected: ${expected} - Actual: ${actual}")


def createAtom(value: Optional[T] = None) -> Callable[[Optional[T], Optional[bool]], Optional[T]]:
    atom = value

    def _(value: Optional[T] = None, isSetter: Optional[bool] = None) -> Optional[T]:
        nonlocal atom

        if not isSetter:
            return atom
        else:
            atom = value
            return None

    return _


def createObj(fields: Iterable[Tuple[str, Any]]):
    obj: Any = {}

    for k, v in fields:
        obj[k] = v

    return obj


def int32ToString(i: int, radix: int = 10):
    convertString = "0123456789ABCDEF"
    if i < radix:
        return convertString[i]
    else:
        return int32ToString(i // radix, radix) + convertString[i % radix]
