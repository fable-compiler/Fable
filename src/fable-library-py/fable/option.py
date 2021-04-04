from expression.core import option


def defaultArg(value, default_value):
    return option.default_arg(option.of_optional(value), default_value)


def defaultArgWith(value, default_value):
    return option.default_arg(option.of_optional(value), default_value())


def map(mapping, value):
    return option.of_optional(value).map(mapping).default_value(None)


def toArray(value):
    return option.of_optional(value).to_list()


__all__ = [
    "defaultArg",
    "defaultArgWith",
    "map",
    "toArray"
]
