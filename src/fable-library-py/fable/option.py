from expression.core import option


class Some:
    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        if self is other:
            return True

        if other is None:
            return False

        if self.value == other.value:
            return True

        return False

    def __str__(self):
        return f"Some {self.value}"

    def __repr__(self):
        return str(self)


def defaultArg(value, default_value):
    return option.default_arg(option.of_optional(value), default_value)


def defaultArgWith(value, default_value):
    return option.default_arg(option.of_optional(value), default_value())


def map(mapping, value):
    return option.of_optional(value).map(mapping).default_value(None)


def toArray(value):
    return option.of_optional(value).to_list()


def some(x):
    return Some(x) if x is None or isinstance(x, Some) else x


def value(x):
    if x is None:
        raise Exception("Option has no value")
    else:
        return x.value if isinstance(x, Some) else x


__all__ = ["defaultArg", "defaultArgWith", "map", "some", "Some", "toArray", "value"]
