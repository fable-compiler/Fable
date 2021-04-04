def fromBits(lowBits: int, highBits: int, unsigned: bool):
    return lowBits + (highBits << 32)


def op_LeftShift(self, numBits):
    return self << numBits
