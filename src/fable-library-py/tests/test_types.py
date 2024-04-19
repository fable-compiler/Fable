from array import array
from decimal import Decimal

from fable_library.core import byte, int16, int32, int64, sbyte, uint16, uint32, uint64


def test_byte_create() -> None:
    assert byte(0) == 0
    assert byte(byte(42)) == 42
    assert byte(uint16(42)) == 42
    assert byte(1.0) == 1


def test_sbyte_create() -> None:
    assert sbyte(0) == 0
    assert sbyte(sbyte(42)) == 42
    assert sbyte(42) == 42
    assert sbyte(-42) == -42


def test_uint16_create() -> None:
    assert uint16(0) == 0
    assert uint16(uint16(42)) == 42
    assert uint16(42) == 42
    assert uint16(65535) == 65535
    assert uint16(65536) == 0
    assert uint16(-1) == 65535


def test_int16_create() -> None:
    assert int16(0) == 0
    assert int16(int16(42)) == 42
    assert int16(42) == 42
    assert int16(32767) == 32767
    assert int16(32768) == -32768
    assert int16(-1) == -1


def test_byte_add() -> None:
    assert byte(42) + byte(42) == 84
    assert byte(255) + byte(1) == 0
    assert byte(255) + 1 == 0
    assert byte(42) + 42 == 84
    # assert byte(1) + 1.1 == 2.1
    assert 42 + byte(42) == 84  # Uses the __radd__ method
    assert Decimal(42) + byte(42) == 84  # Uses the __radd__ method
    assert 1.1 + byte(1) == 2.1  # Uses the __radd__ method


def test_byte_coerce():
    assert int(byte(42)) == 42  # Uses the __int__ method
    assert array("B", [byte(42), byte(42)]) == array("B", [42, 42])  # Uses the __index__ method
    # Can be used as slice indices
    assert [1, 2, 3][byte(1) : byte(2)] == [2]


def test_byte_rich_compare():
    assert byte(42) == 42
    assert byte(42) != 43
    assert byte(42) < 43
    assert byte(42) <= 43
    assert byte(42) <= 42
    assert byte(42) > 41
    assert byte(42) >= 41
    assert byte(42) >= 42
    assert 42 == byte(42)
    assert 43 != byte(42)
    assert 43 > byte(42)
    assert 43 >= byte(42)
    assert 42 < byte(43)
    assert 42 <= byte(43)
    assert 41 < byte(42)
    assert 41 <= byte(42)
    assert 42 >= byte(42)
    assert byte(42) == byte(42)
    assert byte(42) != byte(43)
    assert byte(42) < byte(43)
    assert byte(42) <= byte(43)
    assert byte(42) <= byte(42)
    assert byte(42) > byte(41)
    assert byte(42) >= byte(41)
    assert byte(42) >= byte(42)
    assert not (byte(42) == 43)


def test_uin8_rsift():
    assert byte(2) >> 1 == 1
    assert byte(1) >> 1 == 0
    assert byte(1) >> 8 == 1
    assert byte(1) >> 9 == 0
    assert 2 >> byte(1) == 1
    assert 1 >> byte(1) == 0
    assert 1 >> byte(8) == 0
    assert 1 >> byte(9) == 0


def test_byte_lshift():
    assert byte(1) << 1 == 2
    assert byte(1) << 8 == 1
    assert byte(1) << 9 == 2
    assert 1 << byte(1) == 2
    assert 1 << byte(8) == 256
    assert 1 << byte(9) == 512


def test_binary_complement():
    assert ~byte(0) == 255
    assert ~byte(1) == 254
    assert ~byte(255) == 0
    assert ~byte(254) == 1
    assert ~byte(42) == 213
    assert ~byte(213) == 42


def test_format():
    assert f"{byte(42)}" == "42"
    assert f"{byte(42):02X}" == "2A"
    assert f"{byte(42):02x}" == "2a"
    assert f"{byte(42):08b}" == "00101010"
    assert f"{byte(42):08}" == "00000042"
    assert f"{byte(42):08d}" == "00000042"
    assert f"{byte(42):08o}" == "00000052"
    assert f"{uint16(42):08d}" == "00000042"
    assert f"{int16(42):08d}" == "00000042"
    assert f"{sbyte(42):08d}" == "00000042"
    assert f"{byte(42):08d}" == "00000042"


def test_abs():
    assert abs(byte(42)) == 42
    assert abs(byte(0)) == 0
    assert abs(sbyte(-42)) == 42
    assert abs(sbyte(-1)) == 1
    assert abs(int16(-42)) == 42
    assert abs(int16(-1)) == 1
    assert abs(int32(-42)) == 42
    assert abs(int32(-1)) == 1
    assert abs(int64(-42)) == 42
    assert abs(int64(-1)) == 1
    assert abs(uint16(42)) == 42
    assert abs(uint16(0)) == 0
    assert abs(uint32(42)) == 42
    assert abs(uint32(0)) == 0
    assert abs(uint64(42)) == 42


def test_floor_div():
    assert 10 // byte(3) == 3
