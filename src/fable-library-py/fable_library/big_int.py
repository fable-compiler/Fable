def from_zero() -> int:
    return 0


def from_one() -> int:
    return 1


def from_int32(x: int) -> int:
    return x


def from_int64(x: int) -> int:
    return x


def from_string(x: str) -> int:
    return int(x)


def op_addition(a: int, b: int) -> int:
    return a + b

def parse(value: Any) -> int:
    return int(value)


def to_string(value: int) -> str:
    str(value)