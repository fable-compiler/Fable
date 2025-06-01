from __future__ import annotations

from abc import abstractmethod
from collections.abc import Callable, Iterable
from typing import (
    Any,
    Generic,
    TypeAlias,
    TypeVar,
    cast,
)

from .array_ import (
    Array,
    Float32Array,
    Float64Array,
    Int8Array,
    Int16Array,
    Int32Array,
    Int64Array,
    UInt8Array,
    UInt16Array,
    UInt32Array,
    UInt64Array,
)
from .core import byte, float32, float64, int8, int16, int32, int64, sbyte, uint8, uint16, uint32, uint64
from .util import IComparable, compare


_T = TypeVar("_T")


class FSharpRef(Generic[_T]):
    __slots__ = "getter", "setter"

    def __init__(
        self,
        contents_or_getter: None | (_T | Callable[[], _T]),
        setter: Callable[[_T], None] | None = None,
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
    __slots__ = "fields", "tag"

    def __init__(self) -> None:
        self.tag: int
        self.fields: Array[Any] = Array[Any]()

    @staticmethod
    @abstractmethod
    def cases() -> list[str]: ...

    @property
    def name(self) -> str:
        return self.cases()[self.tag]

    def __str__(self) -> str:
        if not len(self.fields):
            return self.name

        def to_string(value: Any) -> str:
            if isinstance(value, str):
                return f'"{value}"'
            return str(value)

        fields = ""
        with_parens = True
        if len(self.fields) == 1:
            field = to_string(self.fields[0])
            with_parens = field.find(" ") >= 0
            fields = field
        else:
            fields = ", ".join(map(to_string, self.fields))

        return self.name + (" (" if with_parens else " ") + fields + (")" if with_parens else "")

    def __repr__(self) -> str:
        return str(self)

    def __hash__(self) -> int:
        hashes = map(hash, self.fields)
        return hash((hash(self.tag), *hashes))

    def __eq__(self, other: Any) -> bool:
        if self is other:
            return True

        if not isinstance(other, Union):
            return False

        # Different objects are not equal even with same structure
        if self.__class__ != other.__class__:
            return False

        if self.tag == other.tag:
            return self.fields == other.fields

        return False

    def __cmp__(self, __other: Any) -> int:
        if self < __other:
            return -1
        elif self == __other:
            return 0
        return 1

    def __lt__(self, other: Any) -> bool:
        if self.tag == other.tag:
            return True if compare(self.fields, other.fields) < 0 else False

        return self.tag < other.tag


def record_compare_to(self: Record, other: Record) -> int:
    if self is other:
        return 0

    def compare_values(self_value: Any, other_value: Any) -> int:
        match (self_value, other_value):
            case (None, None):
                return 0
            case (None, _):
                return -1
            case (_, None):
                return 1
            # Check for custom equality
            case (self_value, other_value) if self_value == other_value:
                return 0
            case (self_value, other_value) if self_value < other_value:
                return -1
            case (self_value, other_value) if self_value > other_value:
                return 1
            case _:
                return 0

    if hasattr(self, "__dict__") and self.__dict__:
        for name in self.__dict__.keys():
            result = compare_values(self.__dict__[name], other.__dict__[name])
            if result != 0:
                return result

    elif hasattr(self, "__slots__") and self.__slots__:
        for name in self.__slots__:
            result = compare_values(getattr(self, name), getattr(other, name))
            if result != 0:
                return result

    return 0


def record_equals(self: _T, other: _T) -> bool:
    if self is other:
        return True

    if self.__class__ != other.__class__:
        return False

    if isinstance(self, Record) and isinstance(other, Record):
        return record_compare_to(self, other) == 0

    return self == other


def record_to_string(self: Record) -> str:
    if hasattr(self, "__slots__"):
        return "{ " + "\n  ".join(map(lambda slot: slot + " = " + str(getattr(self, slot)), self.__slots__)) + " }"
    else:
        return "{ " + "\n  ".join(map(lambda kv: kv[0] + " = " + str(kv[1]), self.__dict__.items())) + " }"


def record_get_hashcode(self: Record) -> int:
    slots = type(self).__slots__
    return hash(tuple(getattr(self, fixed_field) for fixed_field in slots))


class Record(IComparable):
    __slots__: list[str]

    def GetHashCode(self) -> int:
        return record_get_hashcode(self)

    def Equals(self, other: Record) -> bool:
        return record_equals(self, other)

    def __cmp__(self, other: Record) -> int:
        return record_compare_to(self, other)

    def __str__(self) -> str:
        return record_to_string(self)

    def __repr__(self) -> str:
        return str(self)

    def __lt__(self, other: Any) -> bool:
        return True if self.__cmp__(other) == -1 else False

    def __eq__(self, other: Any) -> bool:
        return self.Equals(other)

    def __hash__(self) -> int:
        return record_get_hashcode(self)


class Attribute: ...


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


def to_string(x: object | None, call_stack: int = 0) -> str:
    match x:
        case float() if int(x) == x:
            return str(int(x))
        case bool():
            return str(x).lower()
        case Iterable() if not hasattr(cast(Iterable[Any], x), "__str__"):
            return seq_to_string(cast(Iterable[Any], x))
        case _:
            return str(x)


class FSharpException(Exception, IComparable):
    def __init__(self) -> None:
        self.Data0: Any = None

    def __str__(self) -> str:
        return record_to_string(self)  # type: ignore

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

        return False

    def __hash__(self) -> int:
        return hash(self.Data0)

    def GetHashCode(self) -> int:
        return hash(self)

    def Equals(self, other: FSharpException):
        return record_equals(self, other)

    def CompareTo(self, other: FSharpException):
        return compare(self.Data0, other.Data0)


class char(int):
    __slots__ = ()


IntegerTypes: TypeAlias = int | byte | sbyte | int16 | uint16 | int32 | uint32 | int64 | uint64
FloatTypes: TypeAlias = float | float32 | float64


def is_exception(x: Any):
    return isinstance(x, Exception)


__all__ = [
    "Array",
    "Attribute",
    "FSharpException",
    "FSharpRef",
    "Float32Array",
    "Float32Array",
    "Float64Array",
    "Float64Array",
    "FloatTypes",
    "Int8Array",
    "Int16Array",
    "Int32Array",
    "Int64Array",
    "IntegerTypes",
    "UInt8Array",
    "UInt16Array",
    "UInt32Array",
    "UInt64Array",
    "Union",
    "byte",
    "char",
    "float32",
    "float64",
    "int8",
    "int16",
    "int32",
    "int64",
    "is_exception",
    "sbyte",
    "seq_to_string",
    "to_string",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
]
