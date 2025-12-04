def maybe_undefined(value: str) -> str | None:
    """
    Returns the value if it's "ok", otherwise returns None (Python equivalent of undefined)
    """
    if value == "ok":
        return value
    else:
        return None


def maybe_null(value: str) -> str | None:
    """
    Returns the value if it's "ok", otherwise returns None (Python equivalent of null)
    """
    if value == "ok":
        return value

    return None
