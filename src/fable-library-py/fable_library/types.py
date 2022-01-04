from __future__ import annotations

import array
from abc import abstractstaticmethod
from typing import Any, Callable, Generic, Iterable, List, Optional, TypeVar
from typing import Union as Union_
from typing import cast

from .util import IComparable, compare

_T = TypeVar("_T")


class FSharpRef(Generic[_T]):
    def __init__(
        self, contents_or_getter: Union_[None, _T, Callable[[], _T]], setter: Optional[Callable[[_T], None]] = None
    ) -> None:
        contents = cast(_T, contents_or_getter)

        def set_contents(value: _T):
            nonlocal contents
            contents = value

        if callable(setter):
            self.getter = cast(Callable[[], _T], contents_or_getter)
            self.setter = setter
        else:
            self.getter = lambda: contents
            self.setter = set_contents

    @property
    def contents(self) -> _T:
        return self.getter()

    @contents.setter
    def contents(self, v: _T) -> None:
        self.setter(v)


class Union(IComparable):
    def __init__(self):
        self.tag: int
        self.fields: List[Any] = []

    @abstractstaticmethod
    def cases() -> List[str]:
        ...

    @property
    def name(self) -> str:
        return self.cases()[self.tag]

    def __str__(self) -> str:
        if not len(self.fields):
            return self.name

        fields = ""
        with_parens = True
        if len(self.fields) == 1:
            field = str(self.fields[0])
            with_parens = field.find(" ") >= 0
            fields = field
        else:
            fields = ", ".join(map(str, self.fields))

        return self.name + (" (" if with_parens else " ") + fields + (")" if with_parens else "")

    def __repr__(self) -> str:
        return str(self)

    def __hash__(self) -> int:
        hashes = map(hash, self.fields)
        return hash([hash(self.tag), *hashes])

    def __eq__(self, other: Any) -> bool:
        if self is other:
            return True
        if not isinstance(other, Union):
            return False

        if self.tag == other.tag:
            return self.fields == other.fields

        return False

    def __lt__(self, other: Any) -> bool:
        if self.tag == other.tag:
            return True if compare(self.fields, other.fields) < 0 else False

        return self.tag < other.tag


def record_equals(self: _T, other: _T) -> bool:
    if self is other:
        return True

    a = self.__dict__ if hasattr(self, "__dict__") else self
    b = other.__dict__ if hasattr(other, "__dict__") else other

    return a == b


def record_compare_to(self: Record, other: Record) -> int:
    if self is other:
        return 0

    else:
        for name in self.__dict__.keys():
            if self.__dict__[name] < other.__dict__.get(name):
                return -1
            elif self.__dict__[name] > other.__dict__.get(name):
                return 1

        return 0


def record_to_string(self: Record) -> str:
    return "{ " + "\n  ".join(map(lambda kv: kv[0] + " = " + str(kv[1]), self.__dict__.items())) + " }"


def record_get_hashcode(self: Record) -> int:
    return hash(*self.values())


class Record(IComparable):
    def __str__(self) -> str:
        return record_to_string(self)

    def __repr__(self) -> str:
        return str(self)

    def GetHashCode(self) -> int:
        return record_get_hashcode(self)

    def Equals(self, other: Record) -> bool:
        return record_equals(self, other)

    def CompareTo(self, other: Record) -> int:
        return record_compare_to(self, other)

    def __lt__(self, other: Any) -> bool:
        return True if self.CompareTo(other) == -1 else False

    def __eq__(self, other: Any) -> bool:
        return self.Equals(other)

    def __hash__(self) -> int:
        return record_get_hashcode(self)


class Attribute:
    pass


def seq_to_string(self: Iterable[Any]) -> str:
    str = "["

    for count, x in enumerate(self):
        if count == 0:
            str += to_string(x)

        elif count == 100:
            str += "; ..."
            break

        else:
            str += "; " + to_string(x)

    return str + "]"


def to_string(x: Union_[Iterable[Any], Any], call_stack: int = 0) -> str:
    if x is not None:
        if isinstance(x, Iterable) and not hasattr(x, "__str__"):
            return seq_to_string(x)

        # else: // TODO: Date?
        #     const cons = Object.getPrototypeOf(x).constructor;
        #     return cons === Object && callStack < 10
        #         // Same format as recordToString
        #         ? "{ " + Object.entries(x).map(([k, v]) => k + " = " + toString(v, callStack + 1)).join("\n  ") + " }"
        #         : cons.name;

    return str(x)


class Exception(Exception):
    def __init__(self, msg: Optional[str] = None):
        self.msg = msg

    def __eq__(self, other: Any) -> bool:
        if self is other:
            return True

        if other is None:
            return False

        return self.msg == other.msg


class FSharpException(Exception, IComparable):
    def __init__(self):
        self.Data0: Any = None

    def __str__(self) -> str:
        return record_to_string(self)

    def __repr__(self) -> str:
        return str(self)

    def __eq__(self, other: Any) -> bool:
        if self is other:
            return True

        if other is None:
            return False

        return self.Data0 == other.Data0

    def __lt__(self, other: Any) -> bool:
        if not isinstance(other, FSharpException):
            return False

        if self.Data0:
            if other.Data0:
                return self.Data0 < other.Data0
            else:
                return False

        elif not self.Data0:
            if other.Data0:
                return False
            else:
                return True

        return super().__lt__(other)

    def __hash__(self) -> int:
        return hash(self.Data0)

    def GetHashCode(self) -> int:
        return hash(self)

    def Equals(self, other: FSharpException):
        return record_equals(self, other)

    def CompareTo(self, other: FSharpException):
        return compare(self.Data0, other.Data0)


def Int8Array(lst: List[int]):
    return array.array("b", lst)


def Uint8Array(lst: List[int]):
    return bytearray(lst)


def Int16Array(lst: List[int]):
    return array.array("h", lst)


def Uint16Array(lst: List[int]):
    return array.array("H", lst)


def Int32Array(lst: List[int]):
    return array.array("i", lst)


def Uint32Array(lst: List[int]):
    return array.array("I", lst)


def Float32Array(lst: List[float]):
    return array.array("f", lst)


def Float64Array(lst: List[float]):
    return array.array("d", lst)


__all__ = ["Attribute", "Exception", "FSharpException", "FSharpRef", "Record", "seq_to_string", "to_string", "Union"]
