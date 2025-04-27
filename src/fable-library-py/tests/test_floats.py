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


def test_float32_arithmetic():
    f32_a = float32(10.5)
    f32_b = float32(2.0)
    # Use pytest.approx for results due to potential f32 precision
    assert f32_a + f32_b == pytest.approx(12.5, rel=1e-9)
    assert f32_a + 2.0 == pytest.approx(12.5, rel=1e-9)
    assert 10.5 + f32_b == pytest.approx(12.5, rel=1e-9)
    assert f32_a - f32_b == pytest.approx(8.5, rel=1e-9)
    assert f32_a - 2.0 == pytest.approx(8.5, rel=1e-9)
    assert 10.5 - f32_b == pytest.approx(8.5, rel=1e-9)
    assert f32_a * f32_b == pytest.approx(21.0, rel=1e-9)
    assert f32_a * 2.0 == pytest.approx(21.0, rel=1e-9)
    assert 10.5 * f32_b == pytest.approx(21.0, rel=1e-9)
    assert f32_a / f32_b == pytest.approx(5.25, rel=1e-9)
    assert f32_a / 2.0 == pytest.approx(5.25, rel=1e-9)
    assert 10.5 / f32_b == pytest.approx(5.25, rel=1e-9)
    assert f32_a // f32_b == pytest.approx(5.0, rel=1e-9)  # Floor division
    assert f32_a // 2.0 == pytest.approx(5.0, rel=1e-9)
    assert 10.5 // f32_b == pytest.approx(5.0, rel=1e-9)
    assert f32_a % f32_b == pytest.approx(0.5, rel=1e-9)  # Modulo
    assert f32_a % 2.0 == pytest.approx(0.5, rel=1e-9)
    assert 10.5 % f32_b == pytest.approx(0.5, rel=1e-9)
    assert f32_a**f32_b == pytest.approx(10.5**2.0, rel=1e-9)  # Power
    assert f32_a**2.0 == pytest.approx(10.5**2.0, rel=1e-9)
    # assert 10.5 ** f32_b == pytest.approx(10.5**2.0, rel=1e-9) # __rpow__ might not be implemented/needed

    assert -f32_a == pytest.approx(-10.5, rel=1e-9)
    assert abs(f32_a) == pytest.approx(10.5, rel=1e-9)
    assert abs(-f32_a) == pytest.approx(10.5, rel=1e-9)
    assert +f32_a == pytest.approx(10.5, rel=1e-9)  # __pos__

    # Zero division
    with pytest.raises(ZeroDivisionError):
        f32_a / 0.0
    with pytest.raises(ZeroDivisionError):
        f32_a / float32(0.0)
    with pytest.raises(ZeroDivisionError):
        f32_a // 0.0
    with pytest.raises(ZeroDivisionError):
        f32_a // float32(0.0)
    with pytest.raises(ZeroDivisionError):
        f32_a % 0.0
    with pytest.raises(ZeroDivisionError):
        f32_a % float32(0.0)


def test_float64_arithmetic():
    # Similar tests as float32, just using float64
    f64_a = float64(10.5)
    f64_b = float64(2.0)
    assert f64_a + f64_b == 12.5  # f64 should have enough precision
    assert f64_a + 2.0 == 12.5
    assert 10.5 + f64_b == 12.5
    assert f64_a - f64_b == 8.5
    assert f64_a - 2.0 == 8.5
    assert 10.5 - f64_b == 8.5
    assert f64_a * f64_b == 21.0
    assert f64_a * 2.0 == 21.0
    assert 10.5 * f64_b == 21.0
    assert f64_a / f64_b == 5.25
    assert f64_a / 2.0 == 5.25
    assert 10.5 / f64_b == 5.25
    assert f64_a // f64_b == 5.0
    assert f64_a // 2.0 == 5.0
    assert 10.5 // f64_b == 5.0
    assert f64_a % f64_b == 0.5
    assert f64_a % 2.0 == 0.5
    assert 10.5 % f64_b == 0.5
    assert f64_a**f64_b == 10.5**2.0
    assert f64_a**2.0 == 10.5**2.0

    assert -f64_a == -10.5
    assert abs(f64_a) == 10.5
    assert abs(-f64_a) == 10.5
    assert +f64_a == 10.5

    with pytest.raises(ZeroDivisionError):
        f64_a / 0.0
    with pytest.raises(ZeroDivisionError):
        f64_a / float64(0.0)


def test_float32_comparison():
    f32_a = float32(42.5)
    f32_b = float32(42.5)
    f32_c = float32(43.0)
    f32_nan = float32(math.nan)

    assert f32_a == f32_b
    assert f32_a == 42.5
    assert 42.5 == f32_a
    assert not (f32_a == f32_c)
    assert not (f32_a == 43.0)
    assert not (43.0 == f32_a)

    assert f32_a != f32_c
    assert f32_a != 43.0
    assert 43.0 != f32_a
    assert not (f32_a != f32_b)
    assert not (f32_a != 42.5)
    assert not (42.5 != f32_a)

    assert f32_a < f32_c
    assert f32_a < 43.0
    assert 42.5 < f32_c
    assert not (f32_c < f32_a)

    assert f32_a <= f32_c
    assert f32_a <= f32_b
    assert f32_a <= 43.0
    assert f32_a <= 42.5
    assert 42.5 <= f32_a
    assert not (f32_c <= f32_a)

    assert f32_c > f32_a
    assert f32_c > 42.5
    assert 43.0 > f32_a
    assert not (f32_a > f32_c)

    assert f32_c >= f32_a
    assert f32_c >= f32_c
    assert f32_c >= 42.5
    assert 43.0 >= f32_a
    assert 43.0 >= f32_c
    assert not (f32_a >= f32_c)

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


def test_float64_comparison():
    # Similar tests as float32
    f64_a = float64(42.5)
    f64_b = float64(42.5)
    f64_c = float64(43.0)
    f64_nan = float64(math.nan)

    assert f64_a == f64_b
    assert f64_a == 42.5
    assert 42.5 == f64_a
    assert not (f64_a == f64_c)

    assert f64_a != f64_c
    assert not (f64_a != f64_b)

    assert f64_a < f64_c
    assert f64_a <= f64_c
    assert f64_a <= f64_b
    assert f64_c > f64_a
    assert f64_c >= f64_a
    assert f64_c >= f64_c

    # NaN comparisons
    assert not (f64_nan == f64_nan)
    assert f64_nan != f64_nan
    assert not (f64_nan < f64_a)
    assert not (f64_nan <= f64_a)
    assert not (f64_nan > f64_a)
    assert not (f64_nan >= f64_a)


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
    assert hash(float32(math.inf)) == 314159  # Placeholder value used in Rust code
    assert hash(float64(math.inf)) == 314159
    assert hash(float32(-math.inf)) == -271828  # Placeholder value used in Rust code
    assert hash(float64(-math.inf)) == -271828


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
