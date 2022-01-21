from typing import TYPE_CHECKING, Any, Dict, Set, TypeVar

from .types import FSharpRef

_K = TypeVar("_K")
_V = TypeVar("_V")

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
    print([map], key)
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
