"""
Benchmark tests for array operations in Fable Python Library.

These benchmarks focus on measuring the performance of array operations
implemented in Rust (via PyO3) compared to native Python operations.
"""

import array as py_array
from typing import Any, TypeVar

import pytest
from fable_library.core import ArrayType, array, byte, int16, int32, int64, sbyte, uint16, uint32, uint64
from pytest_benchmark.fixture import BenchmarkFixture


_T = TypeVar("_T")

# Use Array as an alias for FSharpArray
Array = array.FSharpArray[_T]

# Creation benchmarks


@pytest.mark.parametrize("size", [10, 100, 1000])
def test_generic_array_creation_benchmark(benchmark: BenchmarkFixture, size: int) -> None:
    """Benchmark creating FSharpArray objects of different sizes."""
    elements = list(range(size))
    # Use Generic array type for regular Python lists
    result = benchmark(lambda: Array(array_type="Generic", elements=elements))
    assert len(result) == size


@pytest.mark.parametrize("array_type", ["Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64"])
def test_typed_array_creation_benchmark(benchmark: BenchmarkFixture, array_type: ArrayType) -> None:
    """Benchmark creating typed FSharpArray objects."""
    elements = list(range(100))
    # Type checking is done at runtime, so this is safe for benchmarking
    result = benchmark(lambda: Array(array_type=array_type, elements=elements))
    assert len(result) == 100


# Array operation benchmarks


def test_array_map_benchmark(benchmark: BenchmarkFixture):
    """Benchmark map operation on FSharpArray."""
    # Use Generic array type for regular Python lists
    arr: Array[Any] = Array(array_type="Generic", elements=list(range(1000)))
    result = benchmark(lambda: arr.map(lambda x: x * 2))
    assert len(result) == 1000
    assert result[0] == 0
    assert result[1] == 2


def test_array_filter_benchmark(benchmark: BenchmarkFixture):
    """Benchmark filter operation on FSharpArray."""
    # Use Generic array type for regular Python lists
    arr: Array[Any] = Array(array_type="Generic", elements=list(range(1000)))
    result = benchmark(lambda: arr.filter(lambda x: x % 2 == 0))
    assert len(result) == 500
    assert result[0] == 0
    assert result[1] == 2


@pytest.mark.parametrize("size", [10, 100, 1000])
def test_array_fold_benchmark(benchmark: BenchmarkFixture, size: int) -> None:
    """Benchmark fold operation on FSharpArray."""
    # Use Generic array type for regular Python lists
    arr: Array[Any] = Array(array_type="Generic", elements=list(range(size)))
    result = benchmark(lambda: arr.fold(lambda acc, x: acc + x, 0))
    assert result == sum(range(size))


# Performance comparison benchmarks


@pytest.mark.parametrize("size", [100, 1000])
def test_python_map_benchmark(benchmark: BenchmarkFixture, size: int) -> None:
    """Benchmark map operation on Python list."""
    py_list = list(range(size))

    def python_map():
        return list(map(lambda x: x * 2, py_list))

    # Benchmark Python list map
    benchmark.group = f"Map Operation (size={size})"
    benchmark.name = "Python list"
    py_result = benchmark(python_map)

    # Verify result
    assert len(py_result) == size
    assert py_result[0] == 0
    assert py_result[1] == 2


@pytest.mark.parametrize("size", [100, 1000])
def test_fsharp_map_benchmark(benchmark: BenchmarkFixture, size: int) -> None:
    """Benchmark map operation on FSharpArray."""
    # Use Generic array type for regular Python lists
    fs_array: Array[Any] = Array[Any](array_type="Generic", elements=list(range(size)))

    def fsharp_map():
        return fs_array.map(lambda x: x * 2)

    # Benchmark FSharpArray map
    benchmark.group = f"Map Operation (size={size})"
    benchmark.name = "FSharpArray"
    fs_result = benchmark(fsharp_map)

    # Verify result
    assert len(fs_result) == size
    assert fs_result[0] == 0
    assert fs_result[1] == 2


@pytest.mark.parametrize("size", [100, 1000])
def test_python_filter_benchmark(benchmark: BenchmarkFixture, size: int) -> None:
    """Benchmark filter operation on Python list."""
    py_list = list(range(size))

    def python_filter():
        return list(filter(lambda x: x % 2 == 0, py_list))

    # Benchmark Python list filter
    benchmark.group = f"Filter Operation (size={size})"
    benchmark.name = "Python list"
    py_result = benchmark(python_filter)

    # Verify result
    assert len(py_result) == size // 2
    assert py_result[0] == 0
    assert py_result[1] == 2


@pytest.mark.parametrize("size", [100, 1000])
def test_fsharp_filter_benchmark(benchmark: BenchmarkFixture, size: int) -> None:
    """Benchmark filter operation on FSharpArray."""
    # Use Generic array type for regular Python lists
    fs_array: Array[Any] = Array(array_type="Generic", elements=list(range(size)))  # type: ignore

    def fsharp_filter():
        return fs_array.filter(lambda x: x % 2 == 0)

    # Benchmark FSharpArray filter
    benchmark.group = f"Filter Operation (size={size})"
    benchmark.name = "FSharpArray"
    fs_result = benchmark(fsharp_filter)

    # Verify result
    assert len(fs_result) == size // 2
    assert fs_result[0] == 0
    assert fs_result[1] == 2


# Specialized array type benchmarks


@pytest.mark.parametrize("array_type", ["Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64"])
def test_typed_array_map_benchmark(benchmark: BenchmarkFixture, array_type: str) -> None:
    """Benchmark map operation on typed FSharpArray."""
    # Type checking is done at runtime, so this is safe for benchmarking
    arr: Array[Any] = Array(array_type=array_type, elements=list(range(100)))  # type: ignore
    result = benchmark(lambda: arr.map(lambda x: x * 2))
    assert len(result) == 100
    assert result[0] == 0
    assert result[1] == 2


# Array access benchmarks


def test_array_getitem_benchmark(benchmark: BenchmarkFixture) -> None:
    """Benchmark array element access."""
    # Use Generic array type for regular Python lists
    arr: Array[Any] = Array(array_type="Generic", elements=list(range(1000)))  # type: ignore

    def access_elements():
        total = 0
        for i in range(0, 1000, 10):  # Access every 10th element
            total += arr[i]
        return total

    result = benchmark(access_elements)
    assert result == sum(range(0, 1000, 10))


# Array setitem benchmarks


@pytest.mark.parametrize("array_type", ["Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64"])
def test_array_setitem_int_benchmark(benchmark: BenchmarkFixture, array_type: ArrayType) -> None:
    """Benchmark setting integer values in typed FSharpArray."""
    # Create an array of the specified type and size
    size = 1000
    arr: Array[Any] = Array(array_type=array_type, elements=[0 for _ in range(size)])

    # Create a value to set (42 is a safe value for all integer types)
    value = 42

    # Benchmark setting the value at the middle index
    middle_index = size // 2

    def set_value():
        arr[middle_index] = value

    benchmark.group = f"Array __setitem__ ({array_type})"
    benchmark.name = "Integer value"
    benchmark(set_value)

    # Verify the value was set correctly
    assert arr[middle_index] == value


@pytest.mark.parametrize("array_type", ["Float32", "Float64"])
def test_array_setitem_float_benchmark(benchmark: BenchmarkFixture, array_type: str) -> None:
    """Benchmark setting float values in typed FSharpArray."""
    # Create an array of the specified type and size
    size = 1000
    arr: Array[Any] = Array(array_type=array_type, elements=[0.0 for _ in range(size)])

    # Create a value to set
    value = 3.14159

    # Benchmark setting the value at the middle index
    middle_index = size // 2

    def set_value():
        arr[middle_index] = value

    benchmark.group = f"Array __setitem__ ({array_type})"
    benchmark.name = "Float value"
    benchmark(set_value)

    # Verify the value was set correctly
    assert arr[middle_index] == pytest.approx(value)


def test_array_setitem_generic_benchmark(benchmark: BenchmarkFixture) -> None:
    """Benchmark setting values in FSharpArray with Generic type."""
    # Create a FSharpArray with Generic type
    size = 1000
    arr: Array[Any] = Array(array_type="Generic", elements=[0 for _ in range(size)])

    # Create a value to set
    value = 42

    # Benchmark setting the value at the middle index
    middle_index = size // 2

    def set_value():
        arr[middle_index] = value

    benchmark.group = "Array __setitem__ comparison"
    benchmark.name = "FSharpArray (Generic)"
    benchmark(set_value)

    # Verify the value was set correctly
    assert arr[middle_index] == value


def test_python_list_setitem_benchmark(benchmark: BenchmarkFixture) -> None:
    """Benchmark setting values in Python list."""
    # Create a Python list
    size = 1000
    py_list = [0 for _ in range(size)]

    # Create a value to set
    value = 42

    # Benchmark setting the value at the middle index
    middle_index = size // 2

    def set_value():
        py_list[middle_index] = value

    benchmark.group = "Array __setitem__ comparison"
    benchmark.name = "Python list"
    benchmark(set_value)

    # Verify the value was set correctly
    assert py_list[middle_index] == value


def test_python_array_setitem_benchmark(benchmark: BenchmarkFixture) -> None:
    """Benchmark setting values in Python array.array."""
    # Create a Python array.array
    size = 1000
    py_arr = py_array.array("i", [0 for _ in range(size)])

    # Create a value to set
    value = 42

    # Benchmark setting the value at the middle index
    middle_index = size // 2

    def set_value():
        py_arr[middle_index] = value

    benchmark.group = "Array __setitem__ comparison"
    benchmark.name = "Python array.array"
    benchmark(set_value)

    # Verify the value was set correctly
    assert py_arr[middle_index] == value


# Array iteration benchmarks


def test_array_iteration_benchmark(benchmark: BenchmarkFixture) -> None:
    """Benchmark iterating through array elements."""
    # Use Generic array type for regular Python lists
    arr: Array[Any] = Array(array_type="Generic", elements=list(range(1000)))  # type: ignore

    def iterate_elements():
        total = 0
        for item in arr:
            total += item
        return total

    result = benchmark(iterate_elements)
    assert result == sum(range(1000))


# Python array vs FSharpArray comparison


def test_python_list_creation_benchmark(benchmark: BenchmarkFixture) -> None:
    """Benchmark Python array.array creation."""
    data = list(range(1000))

    def create_py_list():
        return list(data)

    # Benchmark Python array creation
    benchmark.group = "Array Creation"
    benchmark.name = "Python list"
    py_arr = benchmark(create_py_list)

    # Verify result
    assert len(py_arr) == 1000
    assert py_arr[0] == 0
    assert py_arr[1] == 1


def test_python_array_creation_benchmark(benchmark: BenchmarkFixture) -> None:
    """Benchmark Python array.array creation."""
    data = list(range(1000))

    def create_py_array():
        return py_array.array("i", data)

    # Benchmark Python array creation
    benchmark.group = "Array Creation"
    benchmark.name = "Python array.array"
    py_arr = benchmark(create_py_array)

    # Verify result
    assert len(py_arr) == 1000
    assert py_arr[0] == 0
    assert py_arr[1] == 1


def test_fsharp_array_creation_benchmark(benchmark: BenchmarkFixture) -> None:
    """Benchmark FSharpArray creation with Int32 type."""
    data = list(range(1000))

    def create_fs_array() -> Array[int32]:
        return Array(array_type="Int32", elements=data)

    # Benchmark FSharpArray creation
    benchmark.group = "Array Creation"
    benchmark.name = "Array (Int32)"
    fs_arr: Array[Any] = benchmark(create_fs_array)

    # Verify result
    assert len(fs_arr) == 1000
    assert fs_arr[0] == 0
    assert fs_arr[1] == 1
