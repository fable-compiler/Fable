def to_fixed(x: float, dp=None) -> str:
    if dp is not None:
        fmt = "{:.%sf}" % dp
        return fmt.format(x)

    return "{}".format(x)


def to_precision(x: float, sd=None):
    if sd is not None:
        fmt = "{:.%se}" % sd
        return fmt.format(x)

    return "{}".format(x)


def to_exponential(x: float, dp=None):
    if dp is not None:
        fmt = "{:.%se}" % dp
        return fmt.format(x)

    return "{}".format(x)


def to_hex(x) -> str:
    return "{0:x}".format(x)


def multiply(x: int, y: int):
    return x * y
