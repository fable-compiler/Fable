"""
Benchmark tests for integer operations in Fable Python Library.

These benchmarks focus on measuring the performance of integer operations
implemented in Rust (via PyO3) compared to native Python operations.
"""

import pytest
from fable_library.core import byte, int16, int32, int64, sbyte, uint16, uint32, uint64


# Creation benchmarks


@pytest.mark.parametrize("value", [0, 42, 127, 255])
def test_byte_creation_benchmark(benchmark, value):
    """Benchmark creating byte objects from Python integers."""
    result = benchmark(byte, value)
    assert result == value % 256


@pytest.mark.parametrize("int_type", [byte, sbyte, int16, uint16, int32, uint32, int64, uint64])
def test_int_creation_benchmark(benchmark, int_type):
    """Benchmark creating various integer types."""
    result = benchmark(int_type, 42)
    assert result == 42


@pytest.mark.parametrize("value", [42.7, -42.7])
@pytest.mark.parametrize("int_type", [byte, sbyte, int16, uint16, int32, uint32, int64, uint64])
def test_float_to_int_conversion_benchmark(benchmark, value, int_type):
    """Benchmark converting floats to various integer types."""
    _result = benchmark(int_type, value)
    # We don't assert the result as it depends on the integer type's behavior with negative values


# Arithmetic operation benchmarks


@pytest.mark.parametrize("a,b", [(10, 5), (255, 1), (128, 128)])
def test_byte_add_benchmark(benchmark, a, b):
    """Benchmark addition of byte values."""
    byte_a = byte(a)
    byte_b = byte(b)
    result = benchmark(lambda: byte_a + byte_b)
    assert result == byte((a + b) % 256)


@pytest.mark.parametrize("int_type", [byte, sbyte, int16, uint16, int32, uint32, int64, uint64])
def test_addition_across_types_benchmark(benchmark, int_type):
    """Benchmark addition across different integer types."""
    a = int_type(42)
    b = int_type(10)
    result = benchmark(lambda: a + b)
    assert result == int_type(52)


@pytest.mark.parametrize("int_type", [byte, sbyte, int16, uint16, int32, uint32, int64, uint64])
def test_multiplication_benchmark(benchmark, int_type):
    """Benchmark multiplication across different integer types."""
    a = int_type(7)
    b = int_type(6)
    result = benchmark(lambda: a * b)
    assert result == int_type(42)


# Bit operation benchmarks


def test_byte_bitwise_benchmark(benchmark):
    """Benchmark bitwise operations on byte values."""
    a = byte(0b10101010)
    b = byte(0b01010101)
    result = benchmark(lambda: a & b)
    assert result == byte(0)


@pytest.mark.parametrize("op", ["and", "or", "xor"])
def test_bitwise_operations_benchmark(benchmark, op):
    """Benchmark various bitwise operations."""
    a = byte(0b10101010)
    b = byte(0b01010101)

    if op == "and":
        result = benchmark(lambda: a & b)
        assert result == byte(0)
    elif op == "or":
        result = benchmark(lambda: a | b)
        assert result == byte(0b11111111)
    elif op == "xor":
        result = benchmark(lambda: a ^ b)
        assert result == byte(0b11111111)


# Comparison benchmarks


def test_byte_comparison_benchmark(benchmark):
    """Benchmark comparison operations on byte values."""
    a = byte(42)
    b = byte(43)
    result = benchmark(lambda: a < b)
    assert result is True


# Performance comparison benchmarks


def test_addition_loop_native_benchmark(benchmark, medium_range):
    """Benchmark addition in a loop with Python native integers."""
    py_ints = medium_range

    def native_add():
        result = 0
        for i in py_ints:
            result += i
        return result

    # Benchmark Python native addition
    benchmark.group = "Addition Loop"
    benchmark.name = "Python native"
    _native_result = benchmark(native_add)


def test_addition_loop_fsharp_benchmark(benchmark, medium_range):
    """Benchmark addition in a loop with FSharp bytes."""
    fsharp_ints = [byte(i) for i in medium_range]

    def fsharp_add():
        result = byte(0)
        for i in fsharp_ints:
            result += i
        return result

    # Benchmark FSharp byte addition
    benchmark.group = "Addition Loop"
    benchmark.name = "FSharp byte"
    _fsharp_result = benchmark(fsharp_add)


def test_int16_benchmark(benchmark):
    """Benchmark int16 multiplication operations."""
    # Setup
    int16_val = int16(42)

    # Benchmark int16 operations
    benchmark.group = "Integer Type Comparison"
    benchmark.name = "int16"
    _int16_result = benchmark(lambda: int16_val * int16_val)


def test_int32_benchmark(benchmark):
    """Benchmark int32 multiplication operations."""
    # Setup
    int32_val = int32(42)

    # Benchmark int32 operations
    benchmark.group = "Integer Type Comparison"
    benchmark.name = "int32"
    _int32_result = benchmark(lambda: int32_val * int32_val)


# Format and conversion benchmarks


def test_int_format_benchmark(benchmark):
    """Benchmark string formatting of integer types."""
    val = int32(12345)
    result = benchmark(lambda: f"{val:08d}")
    assert result == "00012345"


def test_int_to_string_decimal_benchmark(benchmark):
    """Benchmark to_string method with decimal radix."""
    val = byte(42)

    benchmark.group = "to_string"
    benchmark.name = "decimal"
    decimal = benchmark(lambda: val.to_string(radix=10))
    assert decimal == "42"


def test_int_to_string_binary_benchmark(benchmark):
    """Benchmark to_string method with binary radix."""
    val = byte(42)

    benchmark.group = "to_string"
    benchmark.name = "binary"
    binary = benchmark(lambda: val.to_string(radix=2))
    assert binary == "101010"


def test_int_to_string_hex_benchmark(benchmark):
    """Benchmark to_string method with hexadecimal radix."""
    val = byte(42)

    benchmark.group = "to_string"
    benchmark.name = "hex"
    hex_val = benchmark(lambda: val.to_string(radix=16))
    assert hex_val == "2a"
