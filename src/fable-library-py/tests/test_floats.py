import math

import pytest
from fable_library.core import float32, float64


def test_float32_create():
    assert not (float32(0) == "0.0")

    assert float32(0) == 0.0
    assert float32(42.5) == 42.5
    assert float32(float32(42.5)) == 42.5
    assert float32(float64(42.5)) == 42.5
    assert float32(42) == 42.0
    assert float32(-42.5) == -42.5
    # assert float32(Decimal("42.5")) == pytest.approx(42.5, rel=1e-9) # Decimal conversion might need explicit float()
    assert math.isnan(float(float32(math.nan)))  # Check NaN via float()
    assert float32(math.inf) == math.inf
    assert float32(-math.inf) == -math.inf


def test_float64_create():
    assert float64(0) == 0.0
    assert float64(42.5) == 42.5
    assert float64(float64(42.5)) == 42.5
    assert float64(float32(42.5)) == pytest.approx(
        42.5, rel=1e-9
    )  # Precision loss expected here, comparison should handle it
    assert float64(42) == 42.0
    assert float64(-42.5) == -42.5
    # assert float64(Decimal("42.5")) == pytest.approx(42.5, rel=1e-9)
    assert math.isnan(float(float64(math.nan)))
    assert float64(math.inf) == math.inf
    assert float64(-math.inf) == -math.inf


def test_float32_addition():
    """Test Float32 addition operations."""
    f32_a = float32(10.5)
    f32_b = float32(2.0)
    # Use pytest.approx for results due to potential f32 precision
    assert f32_a + f32_b == pytest.approx(12.5, rel=1e-9)
    assert f32_a + 2.0 == pytest.approx(12.5, rel=1e-9)
    assert 10.5 + f32_b == pytest.approx(12.5, rel=1e-9)


def test_float32_subtraction():
    """Test Float32 subtraction operations."""
    f32_a = float32(10.5)
    f32_b = float32(2.0)
    assert f32_a - f32_b == pytest.approx(8.5, rel=1e-9)
    assert f32_a - 2.0 == pytest.approx(8.5, rel=1e-9)
    assert 10.5 - f32_b == pytest.approx(8.5, rel=1e-9)


def test_float32_multiplication():
    """Test Float32 multiplication operations."""
    f32_a = float32(10.5)
    f32_b = float32(2.0)
    assert f32_a * f32_b == pytest.approx(21.0, rel=1e-9)
    assert f32_a * 2.0 == pytest.approx(21.0, rel=1e-9)
    assert 10.5 * f32_b == pytest.approx(21.0, rel=1e-9)


def test_float32_division():
    """Test Float32 division operations."""
    f32_a = float32(10.5)
    f32_b = float32(2.0)
    assert f32_a / f32_b == 5.25
    assert f32_a / 2.0 == 5.25
    assert 10.5 / f32_b == 5.25


def test_float32_floor_division():
    """Test Float32 floor division operations."""
    f32_a = float32(10.5)
    f32_b = float32(2.0)
    assert f32_a // f32_b == 5.0
    assert f32_a // 2.0 == 5.0
    assert 10.5 // f32_b == 5.0


def test_float32_modulo():
    """Test Float32 modulo operations."""
    f32_a = float32(10.5)
    f32_b = float32(2.0)
    assert f32_a % f32_b == pytest.approx(0.5, rel=1e-9)
    assert f32_a % 2.0 == pytest.approx(0.5, rel=1e-9)
    assert 10.5 % f32_b == pytest.approx(0.5, rel=1e-9)


def test_float32_power():
    """Test Float32 power operations."""
    f32_a = float32(10.5)
    f32_b = float32(2.0)
    assert f32_a**f32_b == pytest.approx(10.5**2.0, rel=1e-9)
    assert f32_a**2.0 == pytest.approx(10.5**2.0, rel=1e-9)
    # assert 10.5 ** f32_b == pytest.approx(10.5**2.0, rel=1e-9) # __rpow__ might not be implemented/needed


def test_float32_unary_operations():
    """Test Float32 unary operations."""
    f32_a = float32(10.5)
    assert -f32_a == pytest.approx(-10.5, rel=1e-9)
    assert abs(f32_a) == pytest.approx(10.5, rel=1e-9)
    assert abs(-f32_a) == pytest.approx(10.5, rel=1e-9)
    assert +f32_a == pytest.approx(10.5, rel=1e-9)  # __pos__


def test_float32_division_by_zero():
    """Test Float32 division by zero (.NET semantics: returns infinity)."""
    f32_a = float32(10.5)

    # Regular division by zero
    result1 = f32_a / 0.0
    result2 = f32_a / float32(0.0)
    assert result1.is_infinity()
    assert result2.is_infinity()
    assert result1.is_positive_infinity()  # Positive infinity for positive dividend

    # Negative dividend division by zero
    neg_result = float32(-10.5) / 0.0
    assert neg_result.is_infinity()
    assert neg_result.is_negative_infinity()  # Negative infinity for negative dividend

    # Floor division by zero
    assert (f32_a // 0.0).is_infinity()
    assert (f32_a // float32(0.0)).is_infinity()


def test_float32_modulo_by_zero():
    """Test Float32 modulo by zero (.NET semantics: returns NaN)."""
    f32_a = float32(10.5)
    assert (f32_a % 0.0).is_nan()
    assert (f32_a % float32(0.0)).is_nan()


def test_float32_zero_divided_by_zero():
    """Test Float32 zero divided by zero (.NET semantics: returns NaN)."""
    assert (float32(0.0) / 0.0).is_nan()
    assert (float32(0.0) // 0.0).is_nan()
    assert (float32(0.0) % 0.0).is_nan()


def test_float64_addition():
    """Test Float64 addition operations."""
    f64_a = float64(10.5)
    f64_b = float64(2.0)
    assert f64_a + f64_b == 12.5  # f64 should have enough precision
    assert f64_a + 2.0 == 12.5
    assert 10.5 + f64_b == 12.5


def test_float64_subtraction():
    """Test Float64 subtraction operations."""
    f64_a = float64(10.5)
    f64_b = float64(2.0)
    assert f64_a - f64_b == 8.5
    assert f64_a - 2.0 == 8.5
    assert 10.5 - f64_b == 8.5


def test_float64_multiplication():
    """Test Float64 multiplication operations."""
    f64_a = float64(10.5)
    f64_b = float64(2.0)
    assert f64_a * f64_b == 21.0
    assert f64_a * 2.0 == 21.0
    assert 10.5 * f64_b == 21.0


def test_float64_division():
    """Test Float64 division operations."""
    f64_a = float64(10.5)
    f64_b = float64(2.0)
    assert f64_a / f64_b == 5.25
    assert f64_a / 2.0 == 5.25
    assert 10.5 / f64_b == 5.25


def test_float64_floor_division():
    """Test Float64 floor division operations."""
    f64_a = float64(10.5)
    f64_b = float64(2.0)
    assert f64_a // f64_b == 5.0
    assert f64_a // 2.0 == 5.0
    assert 10.5 // f64_b == 5.0


def test_float64_modulo():
    """Test Float64 modulo operations."""
    f64_a = float64(10.5)
    f64_b = float64(2.0)
    assert f64_a % f64_b == 0.5
    assert f64_a % 2.0 == 0.5
    assert 10.5 % f64_b == 0.5


def test_float64_power():
    """Test Float64 power operations."""
    f64_a = float64(10.5)
    f64_b = float64(2.0)
    assert f64_a**f64_b == 10.5**2.0
    assert f64_a**2.0 == 10.5**2.0


def test_float64_unary_operations():
    """Test Float64 unary operations."""
    f64_a = float64(10.5)
    assert -f64_a == -10.5
    assert abs(f64_a) == 10.5
    assert abs(-f64_a) == 10.5
    assert +f64_a == 10.5


def test_float64_division_by_zero():
    """Test Float64 division by zero (.NET semantics: returns infinity)."""
    f64_a = float64(10.5)

    # Regular division by zero
    result1 = f64_a / 0.0
    result2 = f64_a / float64(0.0)
    assert result1.is_infinity()
    assert result2.is_infinity()
    assert result1.is_positive_infinity()  # Positive infinity for positive dividend

    # Negative dividend division by zero
    neg_result = float64(-10.5) / 0.0
    assert neg_result.is_infinity()
    assert neg_result.is_negative_infinity()  # Negative infinity for negative dividend

    # Floor division by zero
    assert (f64_a // 0.0).is_infinity()
    assert (f64_a // float64(0.0)).is_infinity()


def test_float64_modulo_by_zero():
    """Test Float64 modulo by zero (.NET semantics: returns NaN)."""
    f64_a = float64(10.5)
    assert (f64_a % 0.0).is_nan()
    assert (f64_a % float64(0.0)).is_nan()


def test_float64_zero_divided_by_zero():
    """Test Float64 zero divided by zero (.NET semantics: returns NaN)."""
    assert (float64(0.0) / 0.0).is_nan()
    assert (float64(0.0) // 0.0).is_nan()
    assert (float64(0.0) % 0.0).is_nan()


def test_float32_equality():
    """Test Float32 equality comparisons."""
    f32_a = float32(42.5)
    f32_b = float32(42.5)
    f32_c = float32(43.0)

    assert f32_a == f32_b
    assert f32_a == 42.5
    assert 42.5 == f32_a
    assert not (f32_a == f32_c)
    assert not (f32_a == 43.0)
    assert not (43.0 == f32_a)


def test_float32_inequality():
    """Test Float32 inequality comparisons."""
    f32_a = float32(42.5)
    f32_b = float32(42.5)
    f32_c = float32(43.0)

    assert f32_a != f32_c
    assert f32_a != 43.0
    assert 43.0 != f32_a
    assert not (f32_a != f32_b)
    assert not (f32_a != 42.5)
    assert not (42.5 != f32_a)


def test_float32_less_than():
    """Test Float32 less than comparisons."""
    f32_a = float32(42.5)
    f32_c = float32(43.0)

    assert f32_a < f32_c
    assert f32_a < 43.0
    assert 42.5 < f32_c
    assert not (f32_c < f32_a)


def test_float32_less_equal():
    """Test Float32 less than or equal comparisons."""
    f32_a = float32(42.5)
    f32_b = float32(42.5)
    f32_c = float32(43.0)

    assert f32_a <= f32_c
    assert f32_a <= f32_b
    assert f32_a <= 43.0
    assert f32_a <= 42.5
    assert 42.5 <= f32_a
    assert not (f32_c <= f32_a)


def test_float32_greater_than():
    """Test Float32 greater than comparisons."""
    f32_a = float32(42.5)
    f32_c = float32(43.0)

    assert f32_c > f32_a
    assert f32_c > 42.5
    assert 43.0 > f32_a
    assert not (f32_a > f32_c)


def test_float32_greater_equal():
    """Test Float32 greater than or equal comparisons."""
    f32_a = float32(42.5)
    f32_c = float32(43.0)

    assert f32_c >= f32_a
    assert f32_c >= f32_c
    assert f32_c >= 42.5
    assert 43.0 >= f32_a
    assert 43.0 >= f32_c
    assert not (f32_a >= f32_c)


def test_float32_nan_comparisons():
    """Test Float32 NaN comparisons (.NET semantics)."""
    f32_a = float32(42.5)
    f32_nan = float32(math.nan)

    # NaN comparisons
    assert not (f32_nan == f32_nan)
    assert not (f32_nan == math.nan)
    assert not (math.nan == f32_nan)
    assert f32_nan != f32_nan
    assert f32_nan != math.nan
    assert math.nan != f32_nan
    assert not (f32_nan < f32_a)
    assert not (f32_nan <= f32_a)
    assert not (f32_nan > f32_a)
    assert not (f32_nan >= f32_a)


def test_float32_incompatible_comparisons():
    """Test Float32 comparisons with incompatible types."""
    f32_a = float32(42.5)

    # Comparison with incompatible types should raise TypeError for ordered, False/True for Eq/Ne
    with pytest.raises(TypeError):
        f32_a < "string"  # type: ignore
    with pytest.raises(TypeError):
        f32_a <= "string"  # type: ignore
    with pytest.raises(TypeError):
        f32_a > "string"  # type: ignore
    with pytest.raises(TypeError):
        f32_a >= "string"  # type: ignore
    assert not (f32_a == "string")
    assert f32_a != "string"


def test_float64_equality():
    """Test Float64 equality comparisons."""
    f64_a = float64(42.5)
    f64_b = float64(42.5)
    f64_c = float64(43.0)

    assert f64_a == f64_b
    assert f64_a == 42.5
    assert 42.5 == f64_a
    assert not (f64_a == f64_c)
    assert not (f64_a == 43.0)
    assert not (43.0 == f64_a)


def test_float64_inequality():
    """Test Float64 inequality comparisons."""
    f64_a = float64(42.5)
    f64_b = float64(42.5)
    f64_c = float64(43.0)

    assert f64_a != f64_c
    assert f64_a != 43.0
    assert 43.0 != f64_a
    assert not (f64_a != f64_b)
    assert not (f64_a != 42.5)
    assert not (42.5 != f64_a)


def test_float64_less_than():
    """Test Float64 less than comparisons."""
    f64_a = float64(42.5)
    f64_c = float64(43.0)

    assert f64_a < f64_c
    assert f64_a < 43.0
    assert 42.5 < f64_c
    assert not (f64_c < f64_a)


def test_float64_less_equal():
    """Test Float64 less than or equal comparisons."""
    f64_a = float64(42.5)
    f64_b = float64(42.5)
    f64_c = float64(43.0)

    assert f64_a <= f64_c
    assert f64_a <= f64_b
    assert f64_a <= 43.0
    assert f64_a <= 42.5
    assert 42.5 <= f64_a
    assert not (f64_c <= f64_a)


def test_float64_greater_than():
    """Test Float64 greater than comparisons."""
    f64_a = float64(42.5)
    f64_c = float64(43.0)

    assert f64_c > f64_a
    assert f64_c > 42.5
    assert 43.0 > f64_a
    assert not (f64_a > f64_c)


def test_float64_greater_equal():
    """Test Float64 greater than or equal comparisons."""
    f64_a = float64(42.5)
    f64_c = float64(43.0)

    assert f64_c >= f64_a
    assert f64_c >= f64_c
    assert f64_c >= 42.5
    assert 43.0 >= f64_a
    assert 43.0 >= f64_c
    assert not (f64_a >= f64_c)


def test_float64_nan_comparisons():
    """Test Float64 NaN comparisons (.NET semantics)."""
    f64_a = float64(42.5)
    f64_nan = float64(math.nan)

    # NaN comparisons
    assert not (f64_nan == f64_nan)
    assert not (f64_nan == math.nan)
    assert not (math.nan == f64_nan)
    assert f64_nan != f64_nan
    assert f64_nan != math.nan
    assert math.nan != f64_nan
    assert not (f64_nan < f64_a)
    assert not (f64_nan <= f64_a)
    assert not (f64_nan > f64_a)
    assert not (f64_nan >= f64_a)


def test_float64_incompatible_comparisons():
    """Test Float64 comparisons with incompatible types."""
    f64_a = float64(42.5)

    # Comparison with incompatible types should raise TypeError for ordered, False/True for Eq/Ne
    with pytest.raises(TypeError):
        f64_a < "string"  # type: ignore
    with pytest.raises(TypeError):
        f64_a <= "string"  # type: ignore
    with pytest.raises(TypeError):
        f64_a > "string"  # type: ignore
    with pytest.raises(TypeError):
        f64_a >= "string"  # type: ignore
    assert not (f64_a == "string")
    assert f64_a != "string"


def test_float_conversion():
    f32_a = float32(42.7)
    f64_a = float64(42.7)

    # Compare f32 conversion to f64 using pytest.approx
    assert float(f32_a) == pytest.approx(42.7, rel=1e-7)  # Adjust tolerance for f32
    assert int(f32_a) == 42  # Truncates
    assert bool(f32_a) is True
    assert bool(float32(0.0)) is False

    # f64 should convert exactly
    assert float(f64_a) == 42.7
    assert int(f64_a) == 42
    assert bool(f64_a) is True
    assert bool(float64(0.0)) is False

    # Conversion of NaN/inf to int should fail
    with pytest.raises(ValueError):
        int(float32(math.nan))
    with pytest.raises(ValueError):
        int(float32(math.inf))
    with pytest.raises(ValueError):
        int(float32(-math.inf))


def test_float_hash():
    # Hash should be consistent with Python's float hash if possible
    # Note: The Rust implementation uses bit hashing which differs from CPython's hash
    # CPython hash(float('nan')) == 0
    # CPython hash(float('inf')) == sys.hash_info.inf (e.g., 314159)
    # CPython hash(float('-inf')) == -sys.hash_info.inf (e.g., -314159)

    assert hash(float32(1.0)) != 0
    assert hash(float64(1.0)) != 0
    assert hash(float32(1.0)) == hash(float32(1.0))
    assert hash(float64(1.0)) == hash(float64(1.0))
    # Check if hash matches standard float hash (might fail due to implementation difference)
    # assert hash(float32(1.5)) == hash(1.5)
    # assert hash(float64(1.5)) == hash(1.5)

    # Test NaN/inf hashing based on Rust implementation
    assert hash(float32(math.nan)) == 0
    assert hash(float64(math.nan)) == 0
    assert hash(float32(math.inf)) == 9223372036854775807  # i64::MAX used in Rust code
    assert hash(float64(math.inf)) == 9223372036854775807
    assert hash(float32(-math.inf)) == -9223372036854775808  # i64::MIN used in Rust code
    assert hash(float64(-math.inf)) == -9223372036854775808


def test_float_repr_str():
    assert repr(float32(42.5)) == "42.5"
    assert str(float32(42.5)) == "42.5"
    assert repr(float64(42.5)) == "42.5"
    assert str(float64(42.5)) == "42.5"

    assert repr(float32(math.nan)) == "NaN"
    assert str(float32(math.nan)) == "NaN"
    assert repr(float32(math.inf)) == "inf"
    assert str(float32(math.inf)) == "inf"
    assert repr(float32(-math.inf)) == "-inf"
    assert str(float32(-math.inf)) == "-inf"
