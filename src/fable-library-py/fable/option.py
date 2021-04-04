from expression.core import option


def defaultArg(value, default_value):
    return option.default_arg(option.of_optional(value), default_value)


def defaultArgWith(value, default_value):
    return option.default_arg(option.of_optional(value), default_value())


__all__ = [
    "defaultArg",
]
