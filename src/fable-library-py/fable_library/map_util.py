from __future__ import annotations

import re
from collections.abc import Iterable, Mapping, MutableMapping, MutableSet
from enum import IntEnum
from re import Match
from typing import (
    Any,
    NoReturn,
    TypeVar,
    cast,
)

from .array_ import Array
from .core import FSharpRef
from .protocols import IEnumerable_1
from .union import Union
from .util import to_iterable


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


def add_to_set(v: object, st: MutableSet[object]) -> bool:
    """Add to set-like object - returns True if added, False if already present."""
    if v in st:
        return False

    st.add(v)
    return True


def remove_from_set(v: object, st: MutableSet[object]) -> bool:
    """Remove from set-like object - returns True if removed, False if not present."""
    if v in st:
        st.discard(v)
        return True

    return False


def contains_in_set(v: object, st: MutableSet[object]) -> bool:
    """Check if set-like object contains a value."""
    return v in st


def union_with_set(s1: MutableSet[object], s2: Iterable[object]) -> None:
    """Add all elements from s2 to s1. Works with both native set and custom HashSet."""
    for x in s2:
        s1.add(x)


def intersect_with_set(s1: MutableSet[object], s2: Iterable[object]) -> None:
    """Remove elements from s1 that are not in s2. Works with both native set and custom HashSet."""
    s2_items = set(s2)
    to_remove = [x for x in s1 if x not in s2_items]
    for x in to_remove:
        s1.discard(x)


def except_with_set(s1: MutableSet[object], s2: Iterable[object]) -> None:
    """Remove all elements in s2 from s1. Works with both native set and custom HashSet."""
    for x in s2:
        s1.discard(x)


def add_to_dict(di: dict[_K, _V], k: _K, v: _V) -> None:
    if k in di:
        raise Exception("An item with the same key has already been added. Key: " + str(k))

    di[k] = v


def remove_from_dict(di: MutableMapping[_K, Any], k: _K) -> bool:
    if k in di:
        del di[k]
        return True

    return False


def try_get_value(map: Mapping[_K, _V], key: _K, default_value: FSharpRef[_V]) -> bool:
    if key in map.keys():
        default_value.contents = map[key]
        return True

    return False


def get_item_from_dict(map: Mapping[_K, _V], key: _K) -> _V:
    if key in map:
        return map[key]
    else:
        raise Exception(f"The given key '{key}' was not present in the dictionary.")


def contains_value(v: _V, map: Mapping[Any, _V]) -> bool:
    return v in map.values()


def make_dict[K, V](source: Mapping[K, V] | Iterable[tuple[K, V]] | IEnumerable_1[tuple[K, V]]) -> dict[K, V]:
    """Convert a source to a Python dict.

    Handles:
    - dict objects (returns a copy)
    - Mapping objects (converts to dict)
    - Python iterables of (key, value) tuples
    - Fable IEnumerable_1 objects (uses GetEnumerator)

    Prefers Python protocols (more efficient), falls back to GetEnumerator.
    """
    # Fast path for dict
    if isinstance(source, dict):
        return dict(cast(dict[K, V], source))  # Return a copy

    # Other Mapping types
    if isinstance(source, Mapping):
        return dict(cast(dict[K, V], source))  # Convert to dict

    # Use to_iterable for both Python iterables and Fable IEnumerable_1
    return dict(to_iterable(source))


def key_value_list(fields: Iterable[Any], case_rule: CaseRules = CaseRules.Ignore):
    obj: dict[str, Any] = {}
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
            kv_pair = name if len(kv_pair.fields) == 0 else [name, *kv_pair.fields]
            case_rule = defined_case_rule

        if isinstance(kv_pair, Array):
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
