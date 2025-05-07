from __future__ import annotations

from .core import option


# Re-export the functions from core.option
bind = option.bind
default_arg = option.default_arg
default_arg_with = option.default_arg_with
flatten = option.flatten
map = option.map
map2 = option.map2
map3 = option.map3
of_nullable = option.of_nullable
some = option.some
to_array = option.to_array
to_nullable = option.to_nullable
value = option.value
or_else = option.or_else
or_else_with = option.or_else_with
filter = option.filter

__all__ = [
    "bind",
    "default_arg",
    "default_arg_with",
    "flatten",
    "map",
    "map2",
    "map3",
    "of_nullable",
    "some",
    "to_array",
    "to_nullable",
    "value",
]
