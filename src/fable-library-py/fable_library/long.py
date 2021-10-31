from typing import Optional

from fable_library.types import FSharpRef


def from_bits(lowBits: int, highBits: int, unsigned: bool):
    ret = lowBits + (highBits << 32)
    if ret > 0x7FFFFFFFFFFFFFFF:
        return ret - 0x10000000000000000

    return ret


def from_int(value: int, unsigned: bool):
    return value

def from_number(value, unsigned: bool):
    return int(value)

def to_number(value: int) -> float:
    return float(value)


def from_integer(value: int, unsigned: bool = None, kind: int=None) -> int:
    return value


def op_left_shift(self, numBits):
    return self << numBits


def op_addition(a, b):
    return a + b


def op_multiply(a, b):
    return a * b


def op_unary_negation(value):
    return -value


def parse(string: str, style: int, unsigned: bool, _bitsize: int, radix: Optional[int] = None):
    return int(string)
    # res = isValid(str, style, radix)
    # if res:
    #     def lessOrEqual(x: str, y: str):
    #         length = max(len(x), len(y))
    #         return x.padStart(len, "0") <= y.padStart(len, "0");

    #     isNegative = res.sign == "-"
    #     maxValue = getMaxValue(unsigned or res.radix != 10, res.radix, isNegative);
    #     if (lessOrEqual(res.digits.upper(), maxValue)):
    #         string = res.sign + res.digits if isNegative else res.digits
    #         return LongLib.fromString(str, unsigned, res.radix);

    # raise Exception("Input string was not in a correct format.");

def try_parse(string: str, style: int, unsigned: bool, bitsize: int, defValue: FSharpRef[int]) -> bool:
    try:
        defValue.contents = parse(string, style, unsigned, bitsize)
        return True
    except Exception:
        return False

def to_string(x):
    return str(x)


def compare(a, b):
    return 0 if a == b else -1 if a < b else 1


def equals(a, b):
    return a == b