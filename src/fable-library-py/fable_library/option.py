from typing import TypeVar, Generic, Any, Optional, Union, List, Callable, cast

_T = TypeVar("_T")
_U = TypeVar("_U")
_V = TypeVar("_V")
_W = TypeVar("_W")


class Some(Generic[_T]):
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


Option = Union[Some[_T], _T, None]


def default_arg(opt: Option[_T], default_value: _T) -> _T:
    return value(opt) if opt is not None else default_value


def default_arg_with(opt: Option[_T], def_thunk: Callable[[], _T]) -> _T:
    return value(opt) if opt is not None else def_thunk()


def filter(predicate: Callable[[_T], bool], opt: Option[_T]) -> Option[_T]:
    if opt is not None:
        return opt if predicate(value(opt)) else None
    return opt


def map(mapping: Callable[[_T], _U], opt: Option[_T]) -> Option[_U]:
    return some(mapping(value(opt))) if opt is not None else None


def map2(mapping: Callable[[_T, _U], _V], opt1: Option[_T], opt2: Option[_U]) -> Option[_V]:
    return mapping(value(opt1), value(opt2)) if (opt1 is not None and opt2 is not None) else None


def map3(mapping: Callable[[_T, _U, _V], _W], opt1: Option[_T], opt2: Option[_U], opt3: Option[_V]) -> Option[_W]:
    return (
        mapping(value(opt1), value(opt2), value(opt3))
        if (opt1 is not None and opt2 is not None and opt3 is not None)
        else None
    )


def some(x: _T) -> Option[_T]:
    return Some(x) if x is None or isinstance(x, Some) else x  # type: ignore


def value(x: Option[_T]) -> _T:
    if x is None:
        raise Exception("Option has no value")
    elif isinstance(x, Some):
        y = cast(Some[_T], x)
        return y.value
    return x


def of_nullable(x: Optional[_T]) -> Option[_T]:
    return x


def to_nullable(x: Option[_T]) -> Optional[_T]:
    return None if x is None else value(x)


def flatten(x: Option[Option[_T]]) -> Option[_T]:
    return None if x is None else value(x)


def to_array(opt: Option[_T]) -> List[_T]:
    return [] if opt is None else [value(opt)]


def bind(binder: Callable[[_T], Option[_U]], opt: Option[_T]) -> Option[_U]:
    return binder(value(opt)) if opt is not None else None


__all__ = [
    "bind",
    "default_arg",
    "default_arg_with",
    "flatten",
    "map",
    "map2",
    "map3",
    "of_nullable",
    "Option",
    "some",
    "Some",
    "to_array",
    "to_nullable",
    "value",
]
