from typing import Tuple

from .types import FSharpRef


def get_range(unsigned: bool, bitsize: int) -> Tuple[int, int]:
    if bitsize == 8:
        return (0, 255) if unsigned else (-128, 127)
    if bitsize == 16:
        return (0, 65535) if unsigned else (-32768, 32767)
    if bitsize == 32:
        return (0, 4294967295) if unsigned else (-2147483648, 2147483647)
    raise ValueError("Invalid bit size.")


AllowHexSpecifier = 0x00000200


def parse(
    string: str, style: int, unsigned: bool, bitsize: int, radix: int = 10
) -> int:
    # const res = isValid(str, style, radix);
    if style & AllowHexSpecifier or string.startswith("0x"):
        radix = 16
    elif string.startswith("0b"):
        radix = 2
    elif string.startswith("0o"):
        radix = 8

    try:
        v = int(string, base=radix)
    except Exception:
        raise ValueError("Input string was not in a correct format.")

    (umin, umax) = get_range(True, bitsize)
    if not unsigned and radix != 10 and v >= umin and v <= umax:
        mask = 1 << (bitsize - 1)
        if v & mask:  # Test if negative
            v = v - (mask << 1)

    (min, max) = get_range(unsigned, bitsize)
    if v >= min and v <= max:
        return v

    raise ValueError("Input string was not in a correct format.")


def try_parse(
    string: str, style: int, unsigned: bool, bitsize: int, defValue: FSharpRef[int]
) -> bool:
    try:
        defValue.contents = parse(string, style, unsigned, bitsize)
        return True
    except Exception:
        return False


def op_unary_negation_int8(x: int) -> int:
    return x if x == -128 else -x


def op_unary_negation_int16(x: int) -> int:
    return x if x == -32768 else -x


def op_unary_negation_int32(x: int) -> int:
    return x if x == -2147483648 else -x
