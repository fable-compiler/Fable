"""
Benchmark tests for float operations in Fable Python Library.

These benchmarks focus on measuring the performance of float operations
implemented in Rust (via PyO3) compared to native Python operations.
"""

import math

import pytest
from fable_library.core import float32, float64


# Creation benchmarks


@pytest.mark.parametrize("value", [0.0, 3.14, -2.718, 1e10])
def test_float32_creation_benchmark(benchmark, value):
    """Benchmark creating float32 objects from Python values."""
    result = benchmark(float32, value)
    assert float(result) == pytest.approx(value)


@pytest.mark.parametrize("value", [0.0, 3.14, -2.718, 1e10])
def test_float64_creation_benchmark(benchmark, value):
    """Benchmark creating float64 objects from Python values."""
    result = benchmark(float64, value)
    assert float(result) == pytest.approx(value)


@pytest.mark.parametrize("value", [42, -123, 0, 9999])
@pytest.mark.parametrize("float_type", [float32, float64])
def test_int_to_float_conversion_benchmark(benchmark, value, float_type):
    """Benchmark converting integers to various float types."""
    result = benchmark(float_type, value)
    assert float(result) == pytest.approx(float(value))


def test_float32_to_float64_conversion_benchmark(benchmark):
    """Benchmark converting float32 to float64."""
    f32 = float32(3.14159)
    result = benchmark(float64, f32)
    assert float(result) == pytest.approx(3.14159)


def test_float64_to_float32_conversion_benchmark(benchmark):
    """Benchmark converting float64 to float32."""
    f64 = float64(3.14159)
    result = benchmark(float32, f64)
    assert float(result) == pytest.approx(3.14159)


# Arithmetic operation benchmarks


@pytest.mark.parametrize("a,b", [(3.14, 2.718), (1000.5, 0.5), (-42.0, 42.0)])
def test_float32_add_benchmark(benchmark, a, b):
    """Benchmark addition of float32 values."""
    float32_a = float32(a)
    float32_b = float32(b)
    result = benchmark(lambda: float32_a + float32_b)
    assert float(result) == pytest.approx(a + b)


@pytest.mark.parametrize("a,b", [(3.14, 2.718), (1000.5, 0.5), (-42.0, 42.0)])
def test_float64_add_benchmark(benchmark, a, b):
    """Benchmark addition of float64 values."""
    float64_a = float64(a)
    float64_b = float64(b)
    result = benchmark(lambda: float64_a + float64_b)
    assert float(result) == pytest.approx(a + b)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_subtraction_benchmark(benchmark, float_type):
    """Benchmark subtraction across different float types."""
    a = float_type(10.5)
    b = float_type(3.2)
    result = benchmark(lambda: a - b)
    assert float(result) == pytest.approx(7.3)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_multiplication_benchmark(benchmark, float_type):
    """Benchmark multiplication across different float types."""
    a = float_type(7.5)
    b = float_type(2.0)
    result = benchmark(lambda: a * b)
    assert float(result) == pytest.approx(15.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_division_benchmark(benchmark, float_type):
    """Benchmark division across different float types."""
    a = float_type(15.0)
    b = float_type(3.0)
    result = benchmark(lambda: a / b)
    assert float(result) == pytest.approx(5.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_floor_division_benchmark(benchmark, float_type):
    """Benchmark floor division across different float types."""
    a = float_type(15.7)
    b = float_type(3.2)
    result = benchmark(lambda: a // b)
    assert float(result) == pytest.approx(4.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_modulo_benchmark(benchmark, float_type):
    """Benchmark modulo operation across different float types."""
    a = float_type(15.7)
    b = float_type(3.2)
    result = benchmark(lambda: a % b)
    assert float(result) == pytest.approx(15.7 % 3.2)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_power_benchmark(benchmark, float_type):
    """Benchmark power operation across different float types."""
    a = float_type(2.0)
    b = float_type(3.0)
    result = benchmark(lambda: a**b)
    assert float(result) == pytest.approx(8.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_negation_benchmark(benchmark, float_type):
    """Benchmark negation operation across different float types."""
    a = float_type(42.5)
    result = benchmark(lambda: -a)
    assert float(result) == pytest.approx(-42.5)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_abs_benchmark(benchmark, float_type):
    """Benchmark absolute value operation across different float types."""
    a = float_type(-42.5)
    result = benchmark(lambda: abs(a))
    assert float(result) == pytest.approx(42.5)


# Math function benchmarks


@pytest.mark.parametrize("float_type", [float32, float64])
def test_sqrt_benchmark(benchmark, float_type):
    """Benchmark square root operation across different float types."""
    a = float_type(16.0)
    result = benchmark(lambda: a.sqrt())
    assert float(result) == pytest.approx(4.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_sin_benchmark(benchmark, float_type):
    """Benchmark sine operation across different float types."""
    a = float_type(math.pi / 2)
    result = benchmark(lambda: a.sin())
    assert float(result) == pytest.approx(1.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_cos_benchmark(benchmark, float_type):
    """Benchmark cosine operation across different float types."""
    a = float_type(0.0)
    result = benchmark(lambda: a.cos())
    assert float(result) == pytest.approx(1.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_tan_benchmark(benchmark, float_type):
    """Benchmark tangent operation across different float types."""
    a = float_type(math.pi / 4)
    result = benchmark(lambda: a.tan())
    assert float(result) == pytest.approx(1.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_exp_benchmark(benchmark, float_type):
    """Benchmark exponential operation across different float types."""
    a = float_type(1.0)
    result = benchmark(lambda: a.exp())
    assert float(result) == pytest.approx(math.e)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_log_benchmark(benchmark, float_type):
    """Benchmark natural logarithm operation across different float types."""
    a = float_type(math.e)
    result = benchmark(lambda: a.log())
    assert float(result) == pytest.approx(1.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_log10_benchmark(benchmark, float_type):
    """Benchmark base-10 logarithm operation across different float types."""
    a = float_type(100.0)
    result = benchmark(lambda: a.log10())
    assert float(result) == pytest.approx(2.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_floor_benchmark(benchmark, float_type):
    """Benchmark floor operation across different float types."""
    a = float_type(3.7)
    result = benchmark(lambda: a.floor())
    assert float(result) == pytest.approx(3.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_ceil_benchmark(benchmark, float_type):
    """Benchmark ceiling operation across different float types."""
    a = float_type(3.2)
    result = benchmark(lambda: a.ceil())
    assert float(result) == pytest.approx(4.0)


@pytest.mark.parametrize("float_type", [float32, float64])
def test_round_benchmark(benchmark, float_type):
    """Benchmark rounding operation across different float types."""
    a = float_type(3.5)
    result = benchmark(lambda: a.round())
    assert float(result) == pytest.approx(4.0)


# Comparison benchmarks


def test_float32_comparison_benchmark(benchmark):
    """Benchmark comparison operations on float32 values."""
    a = float32(3.14)
    b = float32(2.718)
    result = benchmark(lambda: a > b)
    assert result is True


def test_float64_comparison_benchmark(benchmark):
    """Benchmark comparison operations on float64 values."""
    a = float64(3.14)
    b = float64(2.718)
    result = benchmark(lambda: a > b)
    assert result is True


# Performance comparison benchmarks


def test_addition_loop_native_benchmark(benchmark, medium_range):
    """Benchmark addition in a loop with Python native floats."""
    py_floats = [float(i) / 10 for i in medium_range]

    def native_add():
        result = 0.0
        for i in py_floats:
            result += i
        return result

    # Benchmark Python native addition
    benchmark.group = "Float Addition Loop"
    benchmark.name = "Python native"
    _native_result = benchmark(native_add)


def test_addition_loop_float32_benchmark(benchmark, medium_range):
    """Benchmark addition in a loop with float32."""
    float32_vals = [float32(i / 10) for i in medium_range]

    def float32_add():
        result = float32(0.0)
        for i in float32_vals:
            result += i
        return result

    # Benchmark float32 addition
    benchmark.group = "Float Addition Loop"
    benchmark.name = "float32"
    _float32_result = benchmark(float32_add)


def test_addition_loop_float64_benchmark(benchmark, medium_range):
    """Benchmark addition in a loop with float64."""
    float64_vals = [float64(i / 10) for i in medium_range]

    def float64_add():
        result = float64(0.0)
        for i in float64_vals:
            result += i
        return result

    # Benchmark float64 addition
    benchmark.group = "Float Addition Loop"
    benchmark.name = "float64"
    _float64_result = benchmark(float64_add)


def test_python_float_math_benchmark(benchmark):
    """Benchmark math operations with Python native float."""
    val = 0.5

    def python_math():
        return math.sin(val) * math.cos(val) + math.sqrt(val)

    # Benchmark Python native math
    benchmark.group = "Float Math Operations"
    benchmark.name = "Python native"
    _py_result = benchmark(python_math)


def test_float32_math_benchmark(benchmark):
    """Benchmark math operations with float32."""
    val = float32(0.5)

    def float32_math():
        return val.sin() * val.cos() + val.sqrt()

    # Benchmark float32 math
    benchmark.group = "Float Math Operations"
    benchmark.name = "float32"
    _float32_result = benchmark(float32_math)


def test_float64_math_benchmark(benchmark):
    """Benchmark math operations with float64."""
    val = float64(0.5)

    def float64_math():
        return val.sin() * val.cos() + val.sqrt()

    # Benchmark float64 math
    benchmark.group = "Float Math Operations"
    benchmark.name = "float64"
    _float64_result = benchmark(float64_math)


# Edge case benchmarks


@pytest.mark.parametrize("float_type", [float32, float64])
def test_nan_handling_benchmark(benchmark, float_type):
    """Benchmark operations with NaN values."""
    a = float_type(float("nan"))
    # Use Python's math.isnan with the float value
    result = benchmark(lambda: math.isnan(float(a)))
    assert result is True


@pytest.mark.parametrize("float_type", [float32, float64])
def test_infinity_handling_benchmark(benchmark, float_type):
    """Benchmark operations with infinity values."""
    a = float_type(float("inf"))
    # Use Python's math.isinf with the float value
    result = benchmark(lambda: math.isinf(float(a)))
    assert result is True


# Format and conversion benchmarks


def test_float_format_benchmark(benchmark):
    """Benchmark string formatting of float types."""
    val = float64(3.14159)
    result = benchmark(lambda: f"{val:.2f}")
    assert result == "3.14"


def test_float_to_string_benchmark(benchmark):
    """Benchmark to_string method."""
    val = float32(3.14159)
    result = benchmark(lambda: str(val))
    assert result == "3.14159"
