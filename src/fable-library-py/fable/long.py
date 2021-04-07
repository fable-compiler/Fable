def fromBits(lowBits: int, highBits: int, unsigned: bool):
    ret = lowBits + (highBits << 32)
    if ret > 0x7FFFFFFFFFFFFFFF:
        return ret - 0x10000000000000000

    return ret


def op_LeftShift(self, numBits):
    return self << numBits
