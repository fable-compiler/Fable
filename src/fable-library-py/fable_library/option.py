from __future__ import annotations

from collections.abc import Callable
from typing import Any, overload

from .core import option


type Option[T] = option.SomeWrapper[T] | T | None


@overload
def erase[T](__fn: Callable[..., Option[T]], /) -> Callable[..., T | None]: ...


@overload
def erase[T](__value: Option[T], /) -> T | None: ...


def erase(__value_or_fn: Any, /) -> Any:
    """Erase Option[T] to T | None for the type checker.

    Works on both values and functions. Identity at runtime.
    Used when compiler knows Option is non-nested.
    """
    return __value_or_fn


@overload
def widen[T](__fn: Callable[..., T | None], /) -> Callable[..., Option[T]]: ...


@overload
def widen[T](__value: T | None, /) -> Option[T]: ...


def widen(__value_or_fn: Any, /) -> Any:
    """Widen T | None to Option[T] for the type checker.

    Works on both values and functions. Identity at runtime.
    Inverse of erase().
    """
    return __value_or_fn


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
some = option.some
non_null = option.non_null
of_null = option.of_nullable


__all__ = [
    "Option",
    "bind",
    "default_arg",
    "default_arg_with",
    "erase",
    "filter",
    "flatten",
    "map",
    "map2",
    "map3",
    "non_null",
    "of_null",
    "of_nullable",
    "or_else",
    "or_else_with",
    "some",
    "to_array",
    "to_nullable",
    "value",
    "widen",
]
