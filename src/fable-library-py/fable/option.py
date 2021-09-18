class Some:
    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
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


def default_arg(opt, default_value):
    return value(opt) if opt is not None else default_value


def default_arg_with(opt, def_thunk):
    return value(opt) if opt is not None else def_thunk()


def filter(predicate, opt):
    if opt is not None:
        return opt if predicate(value(opt)) else None
    return opt


def map(mapping, opt):
    return some(mapping(value(opt))) if opt is not None else None


def map2(mapping, opt1, opt2):
    return mapping(value(opt1), value(opt2)) if (opt1 is not None and opt2 is not None) else None


def map3(mapping, opt1, opt2, opt3):
    return (
        mapping(value(opt1), value(opt2), value(opt3))
        if (opt1 is not None and opt2 is not None and opt3 is not None)
        else None
    )


def some(x):
    return Some(x) if x is None or isinstance(x, Some) else x


def value(x):
    if x is None:
        raise Exception("Option has no value")
    else:
        return x.value if isinstance(x, Some) else x


def of_nullable(x):
    return x


def to_nullable(x):
    return None if x is None else value(x)


def flatten(x):
    return None if x is None else value(x)


def to_array(opt):
    return [] if opt is None else [value(opt)]


def bind(binder, opt):
    return binder(value(opt)) if opt is not None else None


__all__ = [
    "bind",
    "defaultArg",
    "defaultArgWith",
    "flatten",
    "map",
    "map2",
    "map3",
    "ofNullable",
    "some",
    "Some",
    "toArray",
    "toNullable",
    "value",
]
