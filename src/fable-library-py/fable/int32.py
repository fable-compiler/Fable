def parse(string: str, style, unsigned, bitsize, radix: int = 10) -> int:
    return int(string)
    # const res = isValid(str, style, radix);
    # if (res != null) {
    #     let v = Number.parseInt(res.sign + res.digits, res.radix);
    #     if (!Number.isNaN(v)) {
    #         const [umin, umax] = getRange(true, bitsize);
    #         if (!unsigned && res.radix !== 10 && v >= umin && v <= umax) {
    #             v = v << (32 - bitsize) >> (32 - bitsize);
    #         }
    #         const [min, max] = getRange(unsigned, bitsize);
    #         if (v >= min && v <= max) {
    #             return v;
    #         }
    #     }
    # }
    # throw new Error("Input string was not in a correct format.");


def op_UnaryNegation_Int8(x):
    return x if x == -128 else -x


def op_UnaryNegation_Int16(x):
    return x if x == -32768 else -x


def op_UnaryNegation_Int32(x):
    return x if x == -2147483648 else -x
