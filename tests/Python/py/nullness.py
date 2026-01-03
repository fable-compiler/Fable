from typing import cast


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
