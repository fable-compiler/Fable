from typing import Optional


def fromBits(lowBits: int, highBits: int, unsigned: bool):
    ret = lowBits + (highBits << 32)
    if ret > 0x7FFFFFFFFFFFFFFF:
        return ret - 0x10000000000000000

    return ret


def fromInt(x):
    return x


def op_LeftShift(self, numBits):
    return self << numBits


def op_Addition(a, b):
    return a + b


def op_Multiply(a, b):
    return a * b


def op_UnaryNegation(value):
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


def toString(x):
    return str(x)
