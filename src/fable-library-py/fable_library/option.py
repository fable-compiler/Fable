from typing import Any, Callable, Generic, List, Optional, TypeVar, cast


_T = TypeVar("_T")
_U = TypeVar("_U")
_V = TypeVar("_V")
_W = TypeVar("_W")


class Some(Generic[_T]):
    ___slots__ = "value"

    def __init__(self, value: _T):
        self.value = value

    def __eq__(self, other: Any) -> bool:
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


def default_arg(opt: Optional[_T], default_value: _T) -> _T:
    return value(opt) if opt is not None else default_value


def default_arg_with(opt: Optional[_T], def_thunk: Callable[[], _T]) -> _T:
    return value(opt) if opt is not None else def_thunk()


def filter(predicate: Callable[[_T], bool], opt: Optional[_T]) -> Optional[_T]:
    if opt is not None:
        return opt if predicate(value(opt)) else None
    return opt


def map(mapping: Callable[[_T], _U], opt: Optional[_T]) -> Optional[_U]:
    return some(mapping(value(opt))) if opt is not None else None


def map2(
    mapping: Callable[[_T, _U], _V], opt1: Optional[_T], opt2: Optional[_U]
) -> Optional[_V]:
    return (
        mapping(value(opt1), value(opt2))
        if (opt1 is not None and opt2 is not None)
        else None
    )


def map3(
    mapping: Callable[[_T, _U, _V], _W],
    opt1: Optional[_T],
    opt2: Optional[_U],
    opt3: Optional[_V],
) -> Optional[_W]:
    return (
        mapping(value(opt1), value(opt2), value(opt3))
        if (opt1 is not None and opt2 is not None and opt3 is not None)
        else None
    )


def some(x: Any) -> Optional[Any]:
    return Some[Any](x) if x is None or isinstance(x, Some) else x


def value(x: Optional[_T]) -> _T:
    if x is None:
        raise Exception("Option has no value")
    elif isinstance(x, Some):
        y = cast(Some[_T], x)
        return y.value
    return x


def of_nullable(x: Optional[_T]) -> Optional[_T]:
    return x


def to_nullable(x: Optional[_T]) -> Optional[_T]:
    return None if x is None else value(x)


def flatten(x: Optional[Optional[_T]]) -> Optional[_T]:
    return x if x is None else value(x)


def to_array(opt: Optional[_T]) -> List[_T]:
    return [] if opt is None else [value(opt)]


def bind(binder: Callable[[_T], Optional[_U]], opt: Optional[_T]) -> Optional[_U]:
    return binder(value(opt)) if opt is not None else None


def or_else(opt: Optional[_T], if_none: Optional[_T]) -> Optional[_T]:
    return if_none if opt is None else opt


def or_else_with(
    opt: Optional[_T], if_none_thunk: Callable[[], Optional[_T]]
) -> Optional[_T]:
    return if_none_thunk() if opt is None else opt


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
    "Some",
    "to_array",
    "to_nullable",
    "value",
]
