from typing import Dict, TypeVar

K = TypeVar("K")
V = TypeVar("V")


def addToSet(v, set):
    if v in set:
        return False

    set.add(v)
    return True


def addToDict(dict: Dict[K, V], k: K, v: V):
    if k in dict:
        raise Exception("An item with the same key has already been added. Key: " + str(k))

    dict[k] = v


def tryGetValue(map, key, defaultValue):
    print("tryGetValue", (map, key))
    if key in map:
        defaultValue.contents = map.get(key)
        return True

    return False


def getItemFromDict(map, key):
    if key in map:
        return map.get(key)
    else:
        raise Exception(f"The given key '${key}' was not present in the dictionary.")
