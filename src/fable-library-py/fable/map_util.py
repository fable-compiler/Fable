from typing import Dict, TypeVar

K = TypeVar("K")
V = TypeVar("V")


def addToDict(dict: Dict[K, V], k: K, v: V):
    if k in dict:
        raise Exception("An item with the same key has already been added. Key: " + str(k))

    dict[k] = v
