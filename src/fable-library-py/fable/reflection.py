from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, List, Optional, Type, Union

from .types import Union as FsUnion

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
        return fullName(self)

    def __eq__(self, other) -> bool:
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


def isGenericType(t):
    return t.generics is not None and len(t.generics)


def getGenericTypeDefinition(t):
    return t if t.generics is None else TypeInfo(t.fullname, list(map(lambda _: obj_type, t.generics)))


def name(info):
    if isinstance(info, list):
        return info[0]

    elif isinstance(info, CaseInfo):
        return info.name

    else:
        i = info.fullname.rfind(".");
        return info.fullname if i == -1 else info.fullname[i + 1:]


def fullName(t: TypeInfo) -> str:
    gen = t.generics if t.generics and not isArray(t) else []
    if len(gen):
        gen = ",".join([fullName(x) for x in gen])
        return f"${t.fullname}[{gen}]"

    else:
        return t.fullname


def namespace(t: TypeInfo) -> str:
    i = t.fullname.rfind(".")
    return "" if i == -1 else t.fullname[0: i]


def isArray(t: TypeInfo) -> bool:
    return t.fullname.endswith("[]")


def getElementType(t: TypeInfo) -> Optional[TypeInfo]:
    return (t.generics[0] if t.generics else None) if isArray(t) else None

#   if (t1.fullname === "") { // Anonymous records
#     return t2.fullname === ""
#       && equalArraysWith(getRecordElements(t1),
#         getRecordElements(t2),
#         ([k1, v1], [k2, v2]) => k1 === k2 && equals(v1, v2));
#   } else {
#     return t1.fullname === t2.fullname
#       && equalArraysWith(getGenerics(t1), getGenerics(t2), equals);
