from typing import TypeVar, Generic, Any, Optional, Union, List, Callable

T = TypeVar("T")
U = TypeVar("U")
V = TypeVar("V")


class Some(Generic[T]):
    def __init__(self, value: T):
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


Option = Union[Some[T], Any]


def default_arg(opt: Option[T], default_value: T) -> T:
    return value(opt) if opt is not None else default_value


def default_arg_with(opt: Option[T], def_thunk: Callable[[], T]) -> T:
    return value(opt) if opt is not None else def_thunk()


def filter(predicate: Callable[[T], bool], opt: Option[T]) -> Option[T]:
    if opt is not None:
        return opt if predicate(value(opt)) else None
    return opt


def map(mapping: Callable[[T], U], opt: Option[T]) -> Option[U]:
    return some(mapping(value(opt))) if opt is not None else None


def map2(mapping: Callable[[T, U], V], opt1: Optional[T], opt2: Optional[U]) -> Optional[V]:
    return mapping(value(opt1), value(opt2)) if (opt1 is not None and opt2 is not None) else None


def map3(mapping, opt1, opt2, opt3):
    return (
        mapping(value(opt1), value(opt2), value(opt3))
        if (opt1 is not None and opt2 is not None and opt3 is not None)
        else None
    )


def some(x: T) -> Option[T]:
    return Some(x) if x is None or isinstance(x, Some) else x


def value(x: Option[T]) -> T:
    if x is None:
        raise Exception("Option has no value")
    else:
        return x.value if isinstance(x, Some) else x


def of_nullable(x: Optional[T]) -> Option[T]:
    return x


def to_nullable(x: Option[T]) -> Optional[T]:
    return None if x is None else value(x)


def flatten(x: Option[Option[T]]) -> Option[T]:
    return None if x is None else value(x)


def to_array(opt: Option[T]) -> List[T]:
    return [] if opt is None else [value(opt)]


def bind(binder: Callable[[T], Option[U]], opt: Option[T]) -> Option[U]:
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
    "some",
    "Some",
    "to_array",
    "to_nullable",
    "value",
]
