from typing import Any

from .map import FSharpMap  # type: ignore


def add_to_set(v: Any, st: set) -> bool:
    if v in st:
        return False

    st.add(v)
    return True


def add_to_dict(di, k: Any, v: Any) -> None:
    if k in di:
        raise Exception("An item with the same key has already been added. Key: " + str(k))

    di[k] = v


def try_get_value(map: FSharpMap, key: Any, default_value: Any) -> bool:
    print([map], key)
    if key in map.keys():

        default_value.contents = map[key]
        return True

    return False


def get_item_from_dict(map: dict, key: Any) -> Any:
    if key in map:
        return map.get(key)
    else:
        raise Exception(f"The given key '{key}' was not present in the dictionary.")
