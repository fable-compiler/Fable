from fable_library.types import FSharpRef


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


def try_parse(string: str, style: int, unsigned: bool, bitsize: int, defValue: FSharpRef[int]) -> bool:
    try:
        defValue.contents = parse(string, style, unsigned, bitsize)
        return True
    except Exception:
        return False


def op_unary_negation_int8(x):
    return x if x == -128 else -x


def op_unary_negation_int16(x):
    return x if x == -32768 else -x


def op_unary_negation_int32(x):
    return x if x == -2147483648 else -x
