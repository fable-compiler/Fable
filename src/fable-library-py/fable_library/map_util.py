import re

from enum import IntEnum
from typing import (
    TYPE_CHECKING,
    Any,
    ByteString,
    Dict,
    Iterable,
    List,
    Match,
    MutableSequence,
    NoReturn,
    Set,
    TypeVar,
)

from .types import FSharpRef, Union


_K = TypeVar("_K")
_V = TypeVar("_V")


class CaseRules(IntEnum):
    Ignore = 0
    LowerFirst = 1
    SnakeCase = 2
    SnakeCaseAllCaps = 3
    KebabCase = 4


def dashify(string: str, separator: str) -> str:
    def match(m: Match[str]) -> str:
        s = m.string
        if len(s) == 1:
            return s.lower()
        else:
            return s[0] + separator + s[1].lower()

    return re.sub(r"[a-z]?[A-Z]", match, string)


def change_case(string: str, case_rule: CaseRules) -> str:
    if case_rule == CaseRules.LowerFirst:
        return string[0].lower() + string[1:]

    if case_rule == CaseRules.SnakeCase:
        return dashify(string, "_")
    if case_rule == CaseRules.SnakeCaseAllCaps:
        return dashify(string, "_").upper()
    if case_rule == CaseRules.KebabCase:
        return dashify(string, "-")

    return string


if TYPE_CHECKING:

    class FSharpMap(Dict[_K, _V]):
        ...


else:
    from .map import FSharpMap


def add_to_set(v: _V, st: Set[_V]) -> bool:
    if v in st:
        return False

    st.add(v)
    return True


def add_to_dict(di: Dict[_K, _V], k: _K, v: _V) -> None:
    if k in di:
        raise Exception(
            "An item with the same key has already been added. Key: " + str(k)
        )

    di[k] = v


def try_get_value(
    map: FSharpMap[_K, _V], key: _K, default_value: FSharpRef[_V]
) -> bool:
    if key in map.keys():
        default_value.contents = map[key]
        return True

    return False


def get_item_from_dict(map: Dict[_K, _V], key: _K) -> _V:
    if key in map:
        return map[key]
    else:
        raise Exception(f"The given key '{key}' was not present in the dictionary.")


def contains_value(v: _V, map: Dict[Any, _V]) -> bool:
    return v in map.values()


def key_value_list(fields: Iterable[Any], case_rule: CaseRules = CaseRules.Ignore):
    obj: Dict[str, Any] = {}
    defined_case_rule = case_rule

    def fail(kvPair: Any) -> NoReturn:
        raise Exception("Cannot infer key and value of " + str(kvPair))

    def assign(key: str, case_rule: CaseRules, value: Any):
        key = change_case(key, case_rule)
        obj[key] = value

    for kv_pair in fields:
        case_rule = CaseRules.Ignore
        if kv_pair is None:
            fail(kv_pair)

        # Deflate unions and use the defined case rule
        if isinstance(kv_pair, Union):
            name = kv_pair.cases()[kv_pair.tag]
            kv_pair = name if len(kv_pair.fields) == 0 else [name] + kv_pair.fields
            case_rule = defined_case_rule

        if isinstance(kv_pair, (List, MutableSequence, ByteString)):
            length = len(kv_pair)
            if length == 0:
                fail(kv_pair)

            elif length == 1:
                assign(kv_pair[0], case_rule, True)
                break

            elif length == 2:
                value = kv_pair[1]
                assign(kv_pair[0], case_rule, value)
                break

            assign(kv_pair[0], case_rule, kv_pair[1:])

        elif isinstance(kv_pair, str):
            assign(kv_pair, case_rule, True)
        else:
            fail(kv_pair)

    return obj
