def add_to_set(v, set):
    if v in set:
        return False

    set.add(v)
    return True


def add_to_dict(dict, k, v):
    if k in dict:
        raise Exception("An item with the same key has already been added. Key: " + str(k))

    dict.set(k, v)


def try_get_value(map, key, defaultValue):
    # print("tryGetValue", (map, key))
    if key in map:
        defaultValue.contents = map.get(key)
        return True

    return False


def get_item_from_dict(map, key):
    if key in map:
        return map.get(key)
    else:
        raise Exception(f"The given key '${key}' was not present in the dictionary.")
