import pytest
from fable_library.core import (
    array,
    byte,
    float32,
    float64,
    int16,
    int32,
    int64,
    sbyte,
    uint16,
    uint32,
    uint64,
)


def test_array_create():
    """Test the array.create function which initializes arrays with values."""
    # Test with Int8
    int8_array = array.create(3, sbyte(42))
    assert len(int8_array) == 3
    assert int8_array[0] == sbyte(42)
    assert int8_array[1] == sbyte(42)
    assert int8_array[2] == sbyte(42)

    # Test with UInt8
    uint8_array = array.create(3, byte(42))
    assert len(uint8_array) == 3
    assert uint8_array[0] == byte(42)
    assert uint8_array[1] == byte(42)
    assert uint8_array[2] == byte(42)

    # Test with Int16
    int16_array = array.create(3, int16(42))
    assert len(int16_array) == 3
    assert int16_array[0] == int16(42)
    assert int16_array[1] == int16(42)
    assert int16_array[2] == int16(42)

    # Test with UInt16
    uint16_array = array.create(3, uint16(42))
    assert len(uint16_array) == 3
    assert uint16_array[0] == uint16(42)
    assert uint16_array[1] == uint16(42)
    assert uint16_array[2] == uint16(42)

    # Test with Int32
    int32_array = array.create(3, int32(42))
    assert len(int32_array) == 3
    assert int32_array[0] == int32(42)
    assert int32_array[1] == int32(42)
    assert int32_array[2] == int32(42)

    # Test with UInt32
    uint32_array = array.create(3, uint32(42))
    assert len(uint32_array) == 3
    assert uint32_array[0] == uint32(42)
    assert uint32_array[1] == uint32(42)
    assert uint32_array[2] == uint32(42)

    # Test with Int64
    int64_array = array.create(3, int64(42))
    assert len(int64_array) == 3
    assert int64_array[0] == int64(42)
    assert int64_array[1] == int64(42)
    assert int64_array[2] == int64(42)

    # Test with UInt64
    uint64_array = array.create(3, uint64(42))
    assert len(uint64_array) == 3
    assert uint64_array[0] == uint64(42)
    assert uint64_array[1] == uint64(42)
    assert uint64_array[2] == uint64(42)

    # Test with Float32
    float32_array = array.create(3, float32(3.14))
    assert len(float32_array) == 3
    assert float32_array[0] == pytest.approx(float32(3.14))
    assert float32_array[1] == pytest.approx(float32(3.14))
    assert float32_array[2] == pytest.approx(float32(3.14))

    # Test with Float64
    float64_array = array.create(3, float64(3.14))
    assert len(float64_array) == 3
    assert float64_array[0] == pytest.approx(float64(3.14))
    assert float64_array[1] == pytest.approx(float64(3.14))
    assert float64_array[2] == pytest.approx(float64(3.14))

    # Test with String
    string_array = array.create(3, "hello")
    assert len(string_array) == 3
    assert string_array[0] == "hello"
    assert string_array[1] == "hello"
    assert string_array[2] == "hello"

    # Test with Generic (None)
    none_array = array.create(3, None)
    assert len(none_array) == 3
    assert none_array[0] is None
    assert none_array[1] is None
    assert none_array[2] is None

    # Test with Generic (mixed type)
    mixed_array = array.create(3, [1, 2, 3])
    assert len(mixed_array) == 3
    assert mixed_array[0] == [1, 2, 3]
    assert mixed_array[1] == [1, 2, 3]
    assert mixed_array[2] == [1, 2, 3]

    # Test with empty array (count = 0)
    empty_array = array.create(0, int32(42))
    assert len(empty_array) == 0


def test_array_create_vs_fsharpcons_allocate():
    """Test the difference between array.create and FSharpCons.allocate."""
    # Create a constructor
    int32_cons = array.FSharpCons("Int32")

    # Using allocate - only sets capacity, not initialized values
    allocated_array = int32_cons.allocate(3)
    assert len(allocated_array) == 0  # Length is 0 because no elements are initialized

    # Using create - initializes all elements with the given value
    created_array = array.create(3, int32(42))
    assert len(created_array) == 3  # Length is 3 because all elements are initialized
    assert created_array[0] == int32(42)
    assert created_array[1] == int32(42)
    assert created_array[2] == int32(42)


def test_array_initialize():
    """Test the array.initialize function which initializes arrays with a function."""
    # Initialize an array of Int32 with index*2
    int32_array = array.initialize(5, lambda i: int32(i * 2))
    assert len(int32_array) == 5
    assert int32_array[0] == int32(0)
    assert int32_array[1] == int32(2)
    assert int32_array[2] == int32(4)
    assert int32_array[3] == int32(6)
    assert int32_array[4] == int32(8)

    # Initialize an array of strings with "item-{index}"
    string_array = array.initialize(3, lambda i: f"item-{i}")
    assert len(string_array) == 3
    assert string_array[0] == "item-0"
    assert string_array[1] == "item-1"
    assert string_array[2] == "item-2"

    # Initialize an array of Float64 with index/2.0
    float64_array = array.initialize(4, lambda i: float64(i / 2.0))
    assert len(float64_array) == 4
    assert float64_array[0] == pytest.approx(float64(0.0))
    assert float64_array[1] == pytest.approx(float64(0.5))
    assert float64_array[2] == pytest.approx(float64(1.0))
    assert float64_array[3] == pytest.approx(float64(1.5))

    # Initialize with a specific constructor
    int8_cons = array.FSharpCons("Int8")
    int8_array = array.initialize(3, lambda i: sbyte(i + 1), int8_cons)
    assert len(int8_array) == 3
    assert int8_array[0] == sbyte(1)
    assert int8_array[1] == sbyte(2)
    assert int8_array[2] == sbyte(3)

    # Initialize with empty array (count = 0)
    empty_array = array.initialize(0, lambda i: int32(i))
    assert len(empty_array) == 0
