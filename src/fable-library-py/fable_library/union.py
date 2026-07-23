"""F# Union (Discriminated Union) type for Fable Python runtime."""

from __future__ import annotations

from abc import abstractmethod
from dataclasses import dataclass
from dataclasses import fields as dataclass_fields
from typing import Any, dataclass_transform

from .array_ import Array
from .bases import ComparableBase, EquatableBase, HashableBase, StringableBase
from .core import int32
from .protocols import IComparable
from .util import combine_hash_codes, compare, equal_arrays, number_hash, structural_hash


class Union(StringableBase, EquatableBase, ComparableBase, HashableBase, IComparable):
    """Base class for F# discriminated unions.

    Inherits from ABC base classes that provide Python dunder methods:
    - StringableBase: __str__, __repr__ from ToString
    - EquatableBase: __eq__, __ne__ from Equals
    - ComparableBase: __lt__, __le__, __gt__, __ge__ from CompareTo
    - HashableBase: __hash__ from GetHashCode
    """

    __slots__: list[str] = ["fields", "tag"]

    tag: int32
    fields: Array[Any]

    def __init__(self) -> None:
        self.fields = Array[Any]()

    @staticmethod
    @abstractmethod
    def cases() -> list[str]: ...

    @property
    def name(self) -> str:
        return self.cases()[self.tag]

    # -------------------------------------------------------------------------
    # String representation (used by StringableBase)
    # -------------------------------------------------------------------------

    def ToString(self) -> str:
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

    # -------------------------------------------------------------------------
    # IEquatable - Equality (used by EquatableBase)
    # -------------------------------------------------------------------------

    def Equals(self, other: Any) -> bool:
        if self is other:
            return True

        if not isinstance(other, Union):
            return False

        # Different objects are not equal even with same structure
        if self.__class__ != other.__class__:
            return False

        if self.tag == other.tag:
            # Compare fields with F# equality, not Python `==`, so fields that
            # only implement `Equals` are compared by value.
            return equal_arrays(self.fields, other.fields)

        return False

    # -------------------------------------------------------------------------
    # Hashable (HashableBase provides __hash__ from GetHashCode)
    # -------------------------------------------------------------------------

    def GetHashCode(self) -> int32:
        # Hash the tag and each field structurally instead of hashing a Python
        # tuple, which would hash fields that only implement F# `GetHashCode`
        # by identity.
        hashes = [number_hash(self.tag)]
        hashes.extend(structural_hash(x) for x in self.fields)
        return combine_hash_codes(hashes)

    def __hash__(self) -> int:
        # EquatableBase declares __eq__, so Python implicitly sets __hash__ to
        # None on it, and that None shadows HashableBase.__hash__ in the MRO.
        # Redefine it here so union values stay hashable through GetHashCode.
        return int(self.GetHashCode())

    # -------------------------------------------------------------------------
    # IComparable - Comparison (used by ComparableBase)
    # -------------------------------------------------------------------------

    def CompareTo(self, other: Any) -> int:
        if self.tag == other.tag:
            return compare(self.fields, other.fields)
        return -1 if self.tag < other.tag else 1


def narrow[T](case: type[T], value: object) -> T:
    """Narrow a union value to one of its case classes for the type checker.

    F# discriminated unions compile to a base ``Union`` class plus one case
    class per case (e.g. ``Tree`` -> ``Tree_Leaf | Tree_Node``). Reading a
    case-specific field such as ``x.left_`` requires the value to be typed as
    that case class, but after a ``match`` on ``tag`` it is still statically
    typed as the union. ``narrow(Tree_Node, x)`` re-types ``x`` so the field
    read resolves, playing the same role as ``typing.cast`` but scoped to
    unions. Keeping it in the library (instead of emitting ``cast`` inline)
    lets generated code stay ``cast``-free and preserves ``reportUnnecessaryCast``.

    ``case`` is only used by the type checker to bind ``T``; this is an identity
    function at runtime.
    """
    return value  # pyright: ignore[reportReturnType]


@dataclass_transform()
def tagged_union(tag: int):
    """Decorator for union case classes.

    Uses @dataclass_transform() so type checkers understand:
    - Field annotations become constructor parameters
    - __match_args__ is generated
    - __eq__, __repr__ are generated

    Additionally sets:
    - cls.tag = tag (numeric case discriminator)
    - cls.fields property (list of field values for backwards compat)

    The dataclass is created with `eq=False` so that `Union.Equals` (F#
    equality) is inherited instead of the generated `__eq__`, which would
    compare fields with Python `==`. That also keeps dataclass from setting
    `__hash__` to None, so union values stay hashable via `Union.__hash__`.
    """

    def decorator[T](cls: type[T]) -> type[T]:
        # Apply dataclass internally
        dc_cls: Any = dataclass(cls, eq=False)

        # Set the tag
        dc_cls.tag = tag

        # Generate fields property from dataclass fields
        field_names = [f.name for f in dataclass_fields(dc_cls)]

        @property
        def fields(self) -> Array[Any]:
            return Array[Any]([getattr(self, name) for name in field_names])

        dc_cls.fields = fields

        return dc_cls

    return decorator


__all__ = ["Union", "narrow", "tagged_union"]
