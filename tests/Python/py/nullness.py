from typing import Any, cast


class nullable:
    """Returns a tuple of None values based on type parameters."""

    _params: tuple[type, ...] | None = None

    def __class_getitem__(cls, params: Any) -> type["nullable"]:
        # Create a new class that remembers the params
        class _Initialize(nullable):
            _params = params if isinstance(params, tuple) else (params,)

        return _Initialize

    def __new__(cls) -> Any:
        if cls._params is None:
            raise TypeError("Must specify type parameters: initialize[T1, T2, ...]()")
        return tuple(None for _ in cls._params)


def maybe_undefined(value: str) -> str:
    """
    Returns the value if it's "ok", otherwise returns a cast null.
    This matches Fable's representation of `string | null` where null
    is represented as `cast(str, None)` rather than Optional.
    """
    if value == "ok":
        return value
    else:
        return cast(str, None)


def maybe_null(value: str) -> str:
    """
    Returns the value if it's "ok", otherwise returns a cast null.
    This matches Fable's representation of `string | null` where null
    is represented as `cast(str, None)` rather than Optional.
    """
    if value == "ok":
        return value

    return cast(str, None)
