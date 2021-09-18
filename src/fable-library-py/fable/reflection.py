from __future__ import annotations

import functools
from dataclasses import dataclass
from tkinter import E
from typing import Any, Callable, List, Optional, Type, Union

from .types import Union as FsUnion, FSharpRef, Record

Constructor = Callable[..., Any]

EnumCase = List[Union[str, int]]
FieldInfo = List[Union[str, "TypeInfo"]]


@dataclass
class CaseInfo:
    declaringType: TypeInfo
    tag: int
    name: str
    fields: List[FieldInfo]


@dataclass
class TypeInfo:
    fullname: str
    generics: Optional[List[TypeInfo]] = None
    construct: Optional[Constructor] = None
    parent: Optional[TypeInfo] = None
    fields: Optional[Callable[[], List[FieldInfo]]] = None
    cases: Optional[Callable[[], List[CaseInfo]]] = None
    enum_cases: Optional[List[EnumCase]] = None

    def __str__(self) -> str:
        return full_name(self)

    def __eq__(self, other: Any) -> bool:
        return self.fullname == other.fullname and self.generics == other.generics


def class_type(
    fullname: str,
    generics: Optional[List[TypeInfo]] = None,
    construct: Optional[Constructor] = None,
    parent: Optional[TypeInfo] = None,
) -> TypeInfo:
    return TypeInfo(fullname, generics, construct, parent)


def union_type(
    fullname: str,
    generics: List[TypeInfo],
    construct: Type[FsUnion],
    cases: Callable[[], List[List[FieldInfo]]],
) -> TypeInfo:
    def fn() -> List[CaseInfo]:
        nonlocal construct

        caseNames: List[str] = construct.cases()
        mapper: Callable[[int, List[FieldInfo]], CaseInfo] = lambda i, fields: CaseInfo(t, i, caseNames[i], fields)
        return [mapper(i, x) for i, x in enumerate(cases())]

    t: TypeInfo = TypeInfo(fullname, generics, construct, None, None, fn, None)
    return t


def lambda_type(argType: TypeInfo, returnType: TypeInfo):
    return TypeInfo("Microsoft.FSharp.Core.FSharpFunc`2", [argType, returnType])


def delegate_type(*generics):
    return TypeInfo("System.Func` %d" % len(generics), list(generics))


def record_type(
    fullname: str, generics: List[TypeInfo], construct: Constructor, fields: Callable[[], List[FieldInfo]]
) -> TypeInfo:
    return TypeInfo(fullname, generics, construct, fields=fields)


def option_type(generic: TypeInfo) -> TypeInfo:
    return TypeInfo("Microsoft.FSharp.Core.FSharpOption`1", [generic])


def list_type(generic: TypeInfo) -> TypeInfo:
    return TypeInfo("Microsoft.FSharp.Collections.FSharpList`1", [generic])


def array_type(generic: TypeInfo) -> TypeInfo:
    return TypeInfo(generic.fullname + "[]", [generic])


def enum_type(fullname: str, underlyingType: TypeInfo, enumCases: List[EnumCase]) -> TypeInfo:
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
float32_type: TypeInfo = TypeInfo("System.Single")
float64_type: TypeInfo = TypeInfo("System.Double")
decimal_type: TypeInfo = TypeInfo("System.Decimal")


def equals(t1: TypeInfo, t2: TypeInfo) -> bool:
    return t1 == t2


def is_generic_type(t):
    return t.generics is not None and len(t.generics)


def get_generic_type_definition(t):
    return t if t.generics is None else TypeInfo(t.fullname, list(map(lambda _: obj_type, t.generics)))


def name(info):
    if isinstance(info, list):
        return info[0]

    elif isinstance(info, CaseInfo):
        return info.name

    else:
        i = info.fullname.rfind(".")
        return info.fullname if i == -1 else info.fullname[i + 1 :]


def full_name(t: TypeInfo) -> str:
    gen = t.generics if t.generics and not is_array(t) else []
    if len(gen):
        gen = ",".join([full_name(x) for x in gen])
        return f"${t.fullname}[{gen}]"

    else:
        return t.fullname


def namespace(t: TypeInfo) -> str:
    i = t.fullname.rfind(".")
    return "" if i == -1 else t.fullname[0:i]


def is_array(t: TypeInfo) -> bool:
    return t.fullname.endswith("[]")


def is_enum(t: TypeInfo):
    return t.enum_cases is not None and len(t.enum_cases)


def is_record(t: Any) -> bool:
    return (t.fields is not None) if isinstance(t, TypeInfo) else isinstance(t, Record)


def is_tuple(t: TypeInfo) -> bool:
    return t.fullname.startswith("System.Tuple") and not is_array(t)


# In .NET this is false for delegates
def is_function(t: TypeInfo) -> bool:
    return t.fullname == "Microsoft.FSharp.Core.FSharpFunc`2"


def get_element_type(t: TypeInfo) -> Optional[TypeInfo]:
    return (t.generics[0] if t.generics else None) if is_array(t) else None


def get_enum_values(t: TypeInfo) -> List[int]:
    if is_enum(t) and t.enum_cases is not None:
        return [int(kv[1]) for kv in t.enum_cases]
    else:
        raise ValueError(f"${t.fullname} is not an enum type")


def get_enum_names(t: TypeInfo) -> List[str]:
    if is_enum(t) and t.enum_cases is not None:
        return [str(kv[0]) for kv in t.enum_cases]
    else:
        raise ValueError(f"${t.fullname} is not an enum type")


def get_enum_case(t: TypeInfo, v: Union[int, str]) -> EnumCase:
    if t.enum_cases is None:
        raise ValueError(f"${t.fullname} is not an enum type")

    if isinstance(v, str):
        for kv in t.enum_cases:
            if kv[0] == v:
                return kv

        raise ValueError(f"${v}' was not found in ${t.fullname}")

    for kv in t.enum_cases:
        if kv[1] == v:
            return kv

    # .NET returns the number even if it doesn't match any of the cases
    return ["", v]


def parse_enum(t: TypeInfo, string: str) -> int:
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


def is_enum_defined(t: TypeInfo, v: Union[str, int]) -> bool:
    try:
        kv = get_enum_case(t, v)
        return kv[0] is not None and kv[0] != ""
    except Exception:
        # supress error
        pass

    return False


def get_record_elements(t: TypeInfo) -> List[FieldInfo]:
    if t.fields is not None:
        return t.fields()
    else:
        raise ValueError(f"${t.fullname} is not an F# record type")


def get_record_fields(v: Any) -> List:
    return [getattr(v, k) for k in v.__dict__.keys()]


def get_record_field(v: Any, field: FieldInfo) -> Any:
    if isinstance(field[0], str):
        return getattr(v, field[0])
    raise ValueError("Field not a string.")


def get_tuple_fields(v: Any) -> List:
    return v


def get_tuple_field(v: Any, i: int) -> Any:
    return v[i]


def make_record(t: TypeInfo, values: List) -> Any:
    fields = get_record_elements(t)
    if len(fields) != len(values):
        raise ValueError(f"Expected an array of length ${len(fields)} but got ${len(values)}")

    if t.construct is not None:
        return t.construct(*values)
    else:

        def reducer(iobj, kv):
            i, obj = iobj
            obj[kv[0]] = values[i]
            return obj

        return functools.reduce(reducer, enumerate(fields), {})


def make_tuple(values: List, _t: TypeInfo) -> Any:
    return values
