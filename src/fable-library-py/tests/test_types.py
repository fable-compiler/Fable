from array import array

from fable_library.core import uint8, uint16


def test_uint8_create() -> None:
    assert uint8(0) == 0
    assert uint8(uint8(42)) == 42
    assert uint8(uint16(42)) == 42


def test_uint8_add() -> None:
    assert uint8(42) + uint8(42) == 84
    assert uint8(255) + uint8(1) == 0
    assert uint8(255) + 1 == 0
    assert 42 + uint8(42) == 84  # Uses the __radd__ method
    assert uint8(42) + 42 == 84
    assert uint8(1) + 1.1 == 2.1


def test_uint8_coerce():
    assert int(uint8(42)) == 42  # Uses the __int__ method
    assert array("B", [uint8(42), uint8(42)]) == array("B", [42, 42])  # Uses the __index__ method


def test_uint8_rich_compare():
    assert uint8(42) == 42
    assert uint8(42) != 43
    assert uint8(42) < 43
    assert uint8(42) <= 43
    assert uint8(42) <= 42
    assert uint8(42) > 41
    assert uint8(42) >= 41
    assert uint8(42) >= 42
    assert 42 == uint8(42)
    assert 43 != uint8(42)
    assert 43 > uint8(42)
    assert 43 >= uint8(42)
    assert 42 < uint8(43)
    assert 42 <= uint8(43)
    assert 41 < uint8(42)
    assert 41 <= uint8(42)
    assert 42 >= uint8(42)
    assert uint8(42) == uint8(42)
    assert uint8(42) != uint8(43)
    assert uint8(42) < uint8(43)
    assert uint8(42) <= uint8(43)
    assert uint8(42) <= uint8(42)
    assert uint8(42) > uint8(41)
    assert uint8(42) >= uint8(41)
    assert uint8(42) >= uint8(42)
    assert not (uint8(42) == 43)
