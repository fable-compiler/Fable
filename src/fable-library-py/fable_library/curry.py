from __future__ import annotations

import weakref
from collections.abc import Callable
from typing import Any, Literal, overload


_curried = weakref.WeakKeyDictionary[Any, Any]()


def _curry_n(f: Callable[..., Any], arity: int, args: tuple[Any, ...] = ()) -> Callable[..., Any]:
    """Dynamic curry implementation that builds nested lambdas."""
    return lambda a: f(*args, a) if arity == 1 else _curry_n(f, arity - 1, (*args, a))


def _uncurry_n(f: Callable[..., Any], arity: int) -> Callable[..., Any]:
    """Dynamic uncurry implementation."""

    def uncurried(*args: Any) -> Any:
        result = f
        for arg in args:
            result = result(arg)
        return result

    _curried[uncurried] = f  # Memoize the relationship
    return uncurried


# Overloads for curry with precise typing
@overload
def curry[T1, T2, TResult](
    arity: Literal[2], f: Callable[[T1, T2], TResult]
) -> Callable[[T1], Callable[[T2], TResult]]: ...


@overload
def curry[T1, T2, T3, TResult](
    arity: Literal[3], f: Callable[[T1, T2, T3], TResult]
) -> Callable[[T1], Callable[[T2], Callable[[T3], TResult]]]: ...


@overload
def curry[T1, T2, T3, T4, TResult](
    arity: Literal[4], f: Callable[[T1, T2, T3, T4], TResult]
) -> Callable[[T1], Callable[[T2], Callable[[T3], Callable[[T4], TResult]]]]: ...


@overload
def curry[T1, T2, T3, T4, T5, TResult](
    arity: Literal[5], f: Callable[[T1, T2, T3, T4, T5], TResult]
) -> Callable[[T1], Callable[[T2], Callable[[T3], Callable[[T4], Callable[[T5], TResult]]]]]: ...


# Fallback for higher arities
@overload
def curry(arity: int, f: Callable[..., Any]) -> Callable[..., Any]: ...


def curry(arity: int, f: Callable[..., Any]) -> Callable[..., Any]:
    """Curry a function with the given arity.

    If f was previously created by uncurry(), returns the original curried function.
    """
    cached = _curried.get(f)
    if cached is not None:
        return cached
    return _curry_n(f, arity)


# Overloads for uncurry with precise typing
@overload
def uncurry[T1, T2, TResult](
    arity: Literal[2], f: Callable[[T1], Callable[[T2], TResult]]
) -> Callable[[T1, T2], TResult]: ...


@overload
def uncurry[T1, T2, T3, TResult](
    arity: Literal[3], f: Callable[[T1], Callable[[T2], Callable[[T3], TResult]]]
) -> Callable[[T1, T2, T3], TResult]: ...


@overload
def uncurry[T1, T2, T3, T4, TResult](
    arity: Literal[4], f: Callable[[T1], Callable[[T2], Callable[[T3], Callable[[T4], TResult]]]]
) -> Callable[[T1, T2, T3, T4], TResult]: ...


@overload
def uncurry[T1, T2, T3, T4, T5, TResult](
    arity: Literal[5], f: Callable[[T1], Callable[[T2], Callable[[T3], Callable[[T4], Callable[[T5], TResult]]]]]
) -> Callable[[T1, T2, T3, T4, T5], TResult]: ...


# Fallback for higher arities
@overload
def uncurry(arity: int, f: Callable[..., Any]) -> Callable[..., Any]: ...


def uncurry(arity: int, f: Callable[..., Any]) -> Callable[..., Any]:
    """Uncurry a function with the given arity."""
    return _uncurry_n(f, arity)


__all__ = [
    "curry",
    "uncurry",
]
