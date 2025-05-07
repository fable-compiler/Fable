from fable_library.core import float32, float64


def test_float_math_operations():
    """Test the math operations for Float32/Float64 types."""
    # Test floor
    assert float32(3.7).floor().value() == 3.0
    assert float32(-3.7).floor().value() == -4.0
    assert float64(3.7).floor().value() == 3.0
    assert float64(-3.7).floor().value() == -4.0

    # Test ceil
    assert float32(3.7).ceil().value() == 4.0
    assert float32(-3.7).ceil().value() == -3.0
    assert float64(3.7).ceil().value() == 4.0
    assert float64(-3.7).ceil().value() == -3.0

    # Test round
    assert float32(3.7).round().value() == 4.0
    assert float32(3.2).round().value() == 3.0
    assert float64(3.7).round().value() == 4.0
    assert float64(3.2).round().value() == 3.0


def test_float_to_int_with_math():
    """Test conversion from Float32/Float64 to integer types with math operations."""
    from fable_library.core import byte, int16, int32, int64, sbyte, uint16, uint32, uint64

    # Test Float32 floor to various integer types
    assert byte(float32(3.7).floor()) == 3
    assert sbyte(float32(3.7).floor()) == 3
    assert int16(float32(3.7).floor()) == 3
    assert uint16(float32(3.7).floor()) == 3
    assert int32(float32(3.7).floor()) == 3
    assert uint32(float32(3.7).floor()) == 3
    assert int64(float32(3.7).floor()) == 3
    assert uint64(float32(3.7).floor()) == 3

    # Test Float64 floor to various integer types
    assert byte(float64(3.7).floor()) == 3
    assert sbyte(float64(3.7).floor()) == 3
    assert int16(float64(3.7).floor()) == 3
    assert uint16(float64(3.7).floor()) == 3
    assert int32(float64(3.7).floor()) == 3
    assert uint32(float64(3.7).floor()) == 3
    assert int64(float64(3.7).floor()) == 3
    assert uint64(float64(3.7).floor()) == 3

    # Test negative values
    assert sbyte(float32(-3.7).floor()) == -4
    assert int16(float32(-3.7).floor()) == -4
    assert int32(float32(-3.7).floor()) == -4
    assert int64(float32(-3.7).floor()) == -4

    assert sbyte(float64(-3.7).floor()) == -4
    assert int16(float64(-3.7).floor()) == -4
    assert int32(float64(-3.7).floor()) == -4
    assert int64(float64(-3.7).floor()) == -4
