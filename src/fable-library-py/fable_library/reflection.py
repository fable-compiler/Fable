from __future__ import annotations

import functools
from collections.abc import Callable
from dataclasses import dataclass
from typing import Any, cast

from .types import FSharpRef, Record
from .types import Union as FsUnion
from .util import Array, combine_hash_codes, equal_arrays_with


Constructor = Callable[..., Any]

EnumCase = tuple[str, int]
FieldInfo = tuple[str, "TypeInfo"]
PropertyInfo = FieldInfo
ParameterInfo = FieldInfo


@dataclass
class CaseInfo:
    declaringType: TypeInfo
    tag: int
    name: str
    fields: list[FieldInfo]


@dataclass
class MethodInfo:
    name: str
    parameters: list[ParameterInfo]
    returnType: TypeInfo


@dataclass
class TypeInfo:
    fullname: str
    generics: list[TypeInfo] | None = None
    construct: Constructor | None = None
    parent: TypeInfo | None = None
    fields: Callable[[], list[FieldInfo]] | None = None
    cases: Callable[[], list[CaseInfo]] | None = None
    enum_cases: list[EnumCase] | None = None

    def __str__(self) -> str:
        return full_name(self)

    def __eq__(self, other: Any) -> bool:
        return equals(self, other)

    def __hash__(self) -> int:
        hashes = list(map(hash, self.generics if self.generics else []))
        hashes.append(hash(self.fullname))
        return combine_hash_codes(hashes)


def class_type(
    fullname: str,
    generics: list[TypeInfo] | None = None,
    construct: Constructor | None = None,
    parent: TypeInfo | None = None,
) -> TypeInfo:
    return TypeInfo(fullname, generics, construct, parent)


def union_type(
    fullname: str,
    generics: list[TypeInfo],
    construct: type[FsUnion],
    cases: Callable[[], list[list[FieldInfo]]],
) -> TypeInfo:
    def fn() -> list[CaseInfo]:
        caseNames: list[str] = construct.cases()

        def mapper(i: int, fields: list[FieldInfo]) -> CaseInfo:
            return CaseInfo(t, i, caseNames[i], fields)

        return [mapper(i, x) for i, x in enumerate(cases())]

    t: TypeInfo = TypeInfo(fullname, generics, construct, None, None, fn, None)
    return t


def lambda_type(argType: TypeInfo, returnType: TypeInfo):
    return TypeInfo("Microsoft.FSharp.Core.FSharpFunc`2", [argType, returnType])


def delegate_type(*generics: TypeInfo) -> TypeInfo:
    return TypeInfo("System.Func`%d" % len(generics), list(generics))


def record_type(
    fullname: str,
    generics: list[TypeInfo],
    construct: Constructor,
    fields: Callable[[], list[FieldInfo]],
) -> TypeInfo:
    return TypeInfo(fullname, generics, construct, fields=fields)


def anon_record_type(*fields: FieldInfo) -> TypeInfo:
    return TypeInfo("", None, None, None, lambda: list(fields))


def option_type(generic: TypeInfo) -> TypeInfo:
    return TypeInfo("Microsoft.FSharp.Core.FSharpOption`1", [generic])


def list_type(generic: TypeInfo) -> TypeInfo:
    return TypeInfo("Microsoft.FSharp.Collections.FSharpList`1", [generic])


def array_type(generic: TypeInfo) -> TypeInfo:
    return TypeInfo(generic.fullname + "[]", [generic])


def enum_type(fullname: str, underlyingType: TypeInfo, enumCases: list[EnumCase]) -> TypeInfo:
    return TypeInfo(fullname, [underlyingType], None, None, None, None, enumCases)


def tuple_type(*generics: TypeInfo) -> TypeInfo:
    return TypeInfo(fullname=f"System.Tuple`{len(generics)}", generics=list(generics))


obj_type: TypeInfo = TypeInfo(fullname="System.Object")
unit_type: TypeInfo = TypeInfo("Microsoft.FSharp.Core.Unit")
char_type: TypeInfo = TypeInfo("System.Char")
string_type: TypeInfo = TypeInfo("System.String")
bool_type: TypeInfo = TypeInfo("System.Boolean")
int8_type: TypeInfo = TypeInfo("System.SByte")
uint8_type: TypeInfo = TypeInfo("System.Byte")
int16_type: TypeInfo = TypeInfo("System.Int16")
uint16_type: TypeInfo = TypeInfo("System.UInt16")
int32_type: TypeInfo = TypeInfo("System.Int32")
uint32_type: TypeInfo = TypeInfo("System.UInt32")
int64_type: TypeInfo = TypeInfo("System.Int64")
uint64_type: TypeInfo = TypeInfo("System.Int64")
bigint_type: TypeInfo = TypeInfo("System.Numerics.BigInteger")
nativeint_type: TypeInfo = TypeInfo("System.IntPtr")
unativeint_type: TypeInfo = TypeInfo("System.UIntPtr")
float32_type: TypeInfo = TypeInfo("System.Single")
float64_type: TypeInfo = TypeInfo("System.Double")
decimal_type: TypeInfo = TypeInfo("System.Decimal")


def equals(t1: TypeInfo, t2: TypeInfo) -> bool:
    if not t1.fullname:
        return not t2.fullname and equal_arrays_with(
            get_record_elements(t1),
            get_record_elements(t2),
            lambda kv1, kv2: kv1[0] == kv2[0] and equals(kv1[1], kv2[1]),
        )

    return t1.fullname == t2.fullname and equal_arrays_with(t1.generics, t2.generics, equals)


def is_generic_type(t: TypeInfo) -> bool:
    return t.generics is not None and len(t.generics) > 0


def get_generic_type_definition(t: TypeInfo):
    return t if t.generics is None else TypeInfo(t.fullname, list(map(lambda _: obj_type, t.generics)))


def get_generics(t: TypeInfo) -> list[TypeInfo]:
    return t.generics if t.generics else []


def make_generic_type(t: TypeInfo, generics: list[TypeInfo]) -> TypeInfo:
    return TypeInfo(t.fullname, generics, t.construct, t.parent, t.fields, t.cases)


def create_instance(t: TypeInfo, consArgs: list[Any]) -> Any:
    # TODO: Check if consArgs length is same as t.construct?
    # (Arg types can still be different)
    if callable(t.construct):
        return t.construct(*(consArgs or []))
    else:
        raise ValueError(f"Cannot access constructor of {t.fullname}")


def get_value(propertyInfo: PropertyInfo, v: Any) -> Any:
    return getattr(v, str(propertyInfo[0]))


def name(info: FieldInfo | (TypeInfo | CaseInfo)) -> str:
    if isinstance(info, tuple):
        return info[0]

    elif isinstance(info, TypeInfo):
        i = info.fullname.rfind(".")
        return info.fullname if i == -1 else info.fullname[i + 1 :]
    else:
        return info.name


def full_name(t: TypeInfo) -> str:
    gen = t.generics if t.generics and not is_array(t) else []
    if len(gen):
        gen_ = ",".join([full_name(x) for x in gen])
        return f"{t.fullname}[{gen_}]"

    else:
        return t.fullname


def namespace(t: TypeInfo) -> str:
    i = t.fullname.rfind(".")
    return "" if i == -1 else t.fullname[0:i]


def is_array(t: TypeInfo) -> bool:
    return t.fullname.endswith("[]")


def is_enum(t: TypeInfo) -> bool:
    return t.enum_cases is not None and len(t.enum_cases) > 0


def is_subclass_of(t1: TypeInfo, t2: TypeInfo) -> bool:
    return t1.parent is not None and ((t1.parent == t2) or is_subclass_of(t1.parent, t2))


def is_erased_to_number(t: TypeInfo) -> bool:
    return is_enum(t) or t.fullname in [
        int8_type.fullname,
        uint8_type.fullname,
        int16_type.fullname,
        uint16_type.fullname,
        int32_type.fullname,
        uint32_type.fullname,
        float32_type.fullname,
        float64_type.fullname,
    ]


def is_instance_of_type(t: TypeInfo, o: Any) -> bool:
    if t.fullname == obj_type.fullname:
        return True

    if isinstance(o, bool):
        return t.fullname == bool_type.fullname

    if isinstance(o, str):
        return t.fullname == string_type.fullname

    if isinstance(o, int | float):
        return is_erased_to_number(t)

    if callable(o):
        return is_function(t)

    return t.construct is not None and isinstance(o, t.construct)


def is_record(t: Any) -> bool:
    return (t.fields is not None) if isinstance(t, TypeInfo) else isinstance(t, Record)


def is_tuple(t: TypeInfo) -> bool:
    return t.fullname.startswith("System.Tuple") and not is_array(t)


def is_union(t: Any) -> bool:
    if isinstance(t, TypeInfo):
        return t.cases is not None

    return isinstance(t, FsUnion)


# In .NET this is false for delegates
def is_function(t: TypeInfo) -> bool:
    return t.fullname == "Microsoft.FSharp.Core.FSharpFunc`2"


def get_element_type(t: TypeInfo) -> TypeInfo | None:
    return (t.generics[0] if t.generics else None) if is_array(t) else None


def get_enum_underlying_type(t: TypeInfo):
    return t.generics[0] if t.generics else None


def get_enum_values(t: TypeInfo) -> list[int]:
    if is_enum(t) and t.enum_cases is not None:
        return [int(kv[1]) for kv in t.enum_cases]
    else:
        raise ValueError(f"{t.fullname} is not an enum type")


def get_enum_names(t: TypeInfo) -> list[str]:
    if is_enum(t) and t.enum_cases is not None:
        return [str(kv[0]) for kv in t.enum_cases]
    else:
        raise ValueError(f"{t.fullname} is not an enum type")


def get_enum_case(t: TypeInfo, v: int | str) -> EnumCase:
    if t.enum_cases is None:
        raise ValueError(f"{t.fullname} is not an enum type")

    if isinstance(v, str):
        for kv in t.enum_cases:
            if kv[0] == v:
                return kv

        raise ValueError(f"{v}' was not found in {t.fullname}")

    for kv in t.enum_cases:
        if kv[1] == v:
            return kv

    # .NET returns the number even if it doesn't match any of the cases
    return ("", v)


def get_tuple_elements(t: TypeInfo) -> list[TypeInfo]:
    if is_tuple(t) and t.generics is not None:
        return t.generics
    else:
        raise ValueError(f"{t.fullname} is not a tuple type")


def get_function_elements(t: TypeInfo) -> list[TypeInfo]:
    if is_function(t) and t.generics is not None:
        gen = t.generics
        return [gen[0], gen[1]]
    else:
        raise ValueError(f"{t.fullname} is not an F# function type")


def parse_enum(t: TypeInfo, string: str) -> int:
    value: int | None
    try:
        value = int(string)
    except Exception:
        value = None

    return int(get_enum_case(t, value if value else string)[1])


def try_parse_enum(t: TypeInfo, string: str, def_value: FSharpRef[int]) -> bool:
    try:
        def_value.contents = parse_enum(t, string)
        return True
    except Exception:
        return False


def get_enum_name(t: TypeInfo, v: int) -> str:
    return str(get_enum_case(t, v)[0])


def is_enum_defined(t: TypeInfo, v: str | int) -> bool:
    try:
        kv = get_enum_case(t, v)
        return kv[0] is not None and kv[0] != ""
    except Exception:
        # Supress error
        pass

    return False


def get_record_elements(t: TypeInfo) -> list[FieldInfo]:
    if t.fields is not None:
        return t.fields()
    else:
        raise ValueError(f"{t.fullname} is not an F# record type")


def get_record_fields(v: Any) -> list[str]:
    if isinstance(v, dict):
        return list(cast(dict[str, Any], v).values())

    return [getattr(v, k) for k in v.__slots__]


def get_record_field(v: Any, field: FieldInfo) -> Any:
    if not isinstance(field[0], str):  # type: ignore
        raise ValueError("Field not a string.")

    if isinstance(v, dict):
        return v[field[0]]
    return getattr(v, field[0])


def get_tuple_fields(v: tuple[Any, ...]) -> Array[Any]:
    return list(v)


def get_tuple_field(v: tuple[Any, ...], i: int) -> Any:
    return v[i]


def make_record(t: TypeInfo, values: list[Any]) -> dict[str, Any]:
    fields = get_record_elements(t)
    if len(fields) != len(values):
        raise ValueError(f"Expected an array of length {len(fields)} but got {len(values)}")

    if t.construct is not None:
        return t.construct(*values)

    def reducer(obj: dict[str, Any], ifield: tuple[int, FieldInfo]):
        i, field = ifield
        obj[field[0]] = values[i]
        return obj

    initial: dict[str, Any] = {}
    return functools.reduce(reducer, enumerate(fields), initial)


def make_tuple(values: list[Any], _t: TypeInfo) -> tuple[Any, ...]:
    return tuple(values)


def make_union(uci: CaseInfo, values: list[Any]) -> Any:
    expectedLength = len(uci.fields or [])
    if len(values) != expectedLength:
        raise ValueError(f"Expected an array of length {expectedLength} but got {len(values)}")

    return uci.declaringType.construct(uci.tag, *values) if uci.declaringType.construct else {}


def get_union_cases(t: TypeInfo) -> list[CaseInfo]:
    if t.cases and callable(t.cases):
        return t.cases()
    else:
        raise ValueError(f"{t.fullname} is not an F# union type")


def get_union_fields(v: Any, t: TypeInfo) -> list[Any]:
    cases = get_union_cases(t)
    case_ = cases[v.tag]
    if not case_:
        raise ValueError(f"Cannot find case {v.name} in union type")

    return [case_, list(v.fields)]


def get_union_case_fields(uci: CaseInfo) -> list[FieldInfo]:
    return uci.fields if uci.fields else []


def assert_union(x: Any) -> None:
    if not isinstance(x, FsUnion):
        raise Exception("Value is not an F# union type")


def get_case_tag(x: Any) -> int:
    assert_union(x)
    return x.tag


def get_case_name(x: Any) -> str:
    assert_union(x)
    return x.cases()[x.tag]


def get_case_fields(x: Any) -> list[Any]:
    assert_union(x)
    return x.fields
