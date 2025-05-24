from typing import Any, Literal, TypeVar, cast

import hypothesis.strategies as st
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
from hypothesis import given
from hypothesis.strategies import DrawFn


Array = array.FSharpArray

T = TypeVar("T")

# Define valid array types
ValidArrayType = Literal[
    "Int8", "UInt8", "Int16", "UInt16", "Int32", "UInt32", "Int64", "UInt64", "Float32", "Float64", "String", "Generic"
]

anything = st.one_of(
    st.integers(),
    st.text(),
    st.booleans(),
    st.floats(allow_nan=False, allow_infinity=False),
    st.binary(),
    st.dictionaries(st.text(), st.integers()),
    st.none(),
)

array_types = {
    "Int8": st.integers(min_value=-128, max_value=127).map(lambda x: sbyte(x)),
    "UInt8": st.integers(min_value=0, max_value=255).map(lambda x: byte(x)),
    "Int16": st.integers(min_value=-32768, max_value=32767).map(lambda x: int16(x)),
    "UInt16": st.integers(min_value=0, max_value=65535).map(lambda x: uint16(x)),
    "Int32": st.integers(min_value=-2147483648, max_value=2147483647).map(lambda x: int32(x)),
    "UInt32": st.integers(min_value=0, max_value=4294967295).map(lambda x: uint32(x)),
    "Int64": st.integers(min_value=-9223372036854775808, max_value=9223372036854775807).map(lambda x: int64(x)),
    "UInt64": st.integers(min_value=0, max_value=18446744073709551615).map(lambda x: uint64(x)),
    "Float32": st.floats(allow_nan=False, allow_infinity=False).map(lambda x: float32(x)),
    "Float64": st.floats(allow_nan=False, allow_infinity=False).map(lambda x: float64(x)),
    "String": st.text(),
    "Generic": anything,
}


# Create a composite strategy that generates both array_type and matching elements
@st.composite
def array_with_elements(draw: DrawFn, min_size: int = 0) -> tuple[ValidArrayType, list[Any]]:
    # Draw the array type
    array_type = draw(st.sampled_from(list(array_types.keys())))
    # Draw elements of appropriate type
    elements = draw(st.lists(array_types[array_type], min_size=min_size))
    return cast(ValidArrayType, array_type), elements


@st.composite
def array_with_valid_index(draw: DrawFn) -> tuple[ValidArrayType, list[Any], int]:
    """Generate a tuple of (array_type, elements, index) where index is valid."""
    array_type, elements = draw(array_with_elements(min_size=1))
    # Draw a valid index
    index = draw(st.integers(min_value=0, max_value=len(elements) - 1))
    return array_type, elements, index


@st.composite
def array_with_invalid_index(draw: DrawFn) -> tuple[ValidArrayType, list[Any], int]:
    """Generate a tuple of (array_type, elements, index) where index is invalid."""
    array_type, elements = draw(array_with_elements(min_size=1))
    # Draw an invalid index
    index = draw(st.integers(min_value=len(elements), max_value=len(elements) + 10))
    return array_type, elements, index


@st.composite
def array_with_valid_index_and_new_value(draw: DrawFn) -> tuple[ValidArrayType, list[Any], int, Any]:
    """Generate a tuple of (array_type, elements, index, new_value) where index is valid."""
    array_type, elements = draw(array_with_elements(min_size=1))

    # Draw a valid index
    index = draw(st.integers(min_value=0, max_value=len(elements) - 1))

    # Generate a new value by reusing the array_types strategy that's already defined
    # This avoids the long if-elif-else chain
    new_value = draw(array_types[array_type])

    return array_type, elements, index, new_value


@st.composite
def array_with_invalid_index_and_new_value(draw: DrawFn) -> tuple[ValidArrayType, list[Any], int, Any]:
    """Generate a tuple of (array_type, elements, index, new_value) where index is invalid."""
    array_type, elements = draw(array_with_elements(min_size=1))

    # Draw an invalid index
    index = draw(st.integers(min_value=len(elements), max_value=len(elements) + 10))

    # Generate a new value of appropriate type
    new_value = draw(array_types[array_type])

    return array_type, elements, index, new_value


@pytest.mark.parametrize("array_type", array_types.keys())
def test_fsharp_array_create_empty(array_type: str) -> None:
    """Test creating empty FSharpArray instances with different types."""
    # Create empty array with specified type
    empty_array: Array[Any]
    match array_type:
        case "Int8":
            empty_array = Array[sbyte]()
        case "UInt8":
            empty_array = Array[byte]()
        case "Int16":
            empty_array = Array[int16]()
        case "UInt16":
            empty_array = Array[uint16]()
        case "Int32":
            empty_array = Array[int32]()
        case "UInt32":
            empty_array = Array[uint32]()
        case "Int64":
            empty_array = Array[int64]()
        case "UInt64":
            empty_array = Array[uint64]()
        case "Float32":
            empty_array = Array[float32]()
        case "Float64":
            empty_array = Array[float64]()
        case "String":
            empty_array = Array[str]()
        case _:  # Generic
            empty_array = Array[Any]()

    # Check length
    assert len(empty_array) == 0


@given(array_data=array_with_elements())
def test_fsharp_array_create_with_elements(array_data: tuple[str, list[Any]]) -> None:
    """Test creating FSharpArray instances with elements."""
    array_type, elements = array_data

    # Create array with the given elements and type
    arr: Array[Any]
    match array_type:
        case "Int8":
            arr = Array[sbyte](elements)
        case "UInt8":
            arr = Array[byte](elements)
        case "Int16":
            arr = Array[int16](elements)
        case "UInt16":
            arr = Array[uint16](elements)
        case "Int32":
            arr = Array[int32](elements)
        case "UInt32":
            arr = Array[uint32](elements)
        case "Int64":
            arr = Array[int64](elements)
        case "UInt64":
            arr = Array[uint64](elements)
        case "Float32":
            arr = Array[float32](elements)
        case "Float64":
            arr = Array[float64](elements)
        case "String":
            arr = Array[str](elements)
        case _:  # Generic
            arr = Array[Any](elements)

    # Check length
    assert len(arr) == len(elements)

    # Check each element
    for i, element in enumerate(elements):
        assert arr[i] == element


@given(
    input_data=st.lists(anything, min_size=1, max_size=20),
    invalid_type=st.text().filter(lambda x: x not in array_types.keys()),
)
def test_fsharp_array_invalid_type(input_data: list[Any], invalid_type: str) -> None:
    """Test creating FSharpArray with an invalid array_type string defaults to Generic."""
    try:
        # Create the array with invalid type
        arr: Array[Any] = Array[Any](input_data)

        # Verify the array contains the same input data
        assert list(arr) == input_data
    except Exception as e:
        pytest.fail(f"Creating FSharpArray with invalid type raised unexpected error: {e}")


def test_fsharp_cons_invalid_type() -> None:
    """Test creating FSharpCons with an invalid array_type string defaults to Generic."""
    try:
        cons: array.FSharpCons[Any] = array.FSharpCons("AnotherInvalidType")  # type: ignore
        # The array_type attribute should be accessible and reflect the default
        assert cons.array_type == "Generic"

        # Test allocation using the defaulted generic constructor
        # Note: allocate only sets capacity, not initialized values
        allocated_arr = cons.allocate(3)
        assert len(allocated_arr) == 0  # Length is 0 because no elements are initialized

        # Now test with array.create which actually initializes values
        created_arr = array.create(3, None)
        assert len(created_arr) == 3
        assert created_arr[0] is None  # Default for generic allocation
        created_arr[0] = "works"  # Should allow setting different types
        assert created_arr[0] == "works"
    except Exception as e:
        pytest.fail(f"Creating/using FSharpCons with invalid type raised unexpected error: {e}")


@given(array_data=array_with_valid_index())
def test_fsharp_array_getitem(array_data: tuple[str, list[Any], int]) -> None:
    """Test getting items from FSharpArray using hypothesis."""
    array_type, elements, index = array_data

    # Create array with the given elements and type
    arr: Array[Any]
    match array_type:
        case "Int8":
            arr = Array[sbyte](elements)
        case "UInt8":
            arr = Array[byte](elements)
        case "Int16":
            arr = Array[int16](elements)
        case "UInt16":
            arr = Array[uint16](elements)
        case "Int32":
            arr = Array[int32](elements)
        case "UInt32":
            arr = Array[uint32](elements)
        case "Int64":
            arr = Array[int64](elements)
        case "UInt64":
            arr = Array[uint64](elements)
        case "Float32":
            arr = Array[float32](elements)
        case "Float64":
            arr = Array[float64](elements)
        case "String":
            arr = Array[str](elements)
        case _:  # Generic
            arr = Array[Any](elements)

    # Get the expected value using Python's list indexing
    expected_value = elements[index]

    # Test getting item from FSharpArray
    actual_value = arr[index]

    # Handle float comparisons
    if array_type in ["Float32", "Float64"]:
        assert actual_value == pytest.approx(expected_value)  # type: ignore
    else:
        assert actual_value == expected_value


@given(array_data=array_with_invalid_index())
def test_fsharp_array_getitem_out_of_range(array_data: tuple[str, list[Any], int]) -> None:
    """Test getting items with out-of-range indices using hypothesis."""
    array_type, elements, index = array_data

    # Create array with the given elements and type
    arr: Array[Any]
    match array_type:
        case "Int8":
            arr = Array[sbyte](elements)
        case "UInt8":
            arr = Array[byte](elements)
        case "Int16":
            arr = Array[int16](elements)
        case "UInt16":
            arr = Array[uint16](elements)
        case "Int32":
            arr = Array[int32](elements)
        case "UInt32":
            arr = Array[uint32](elements)
        case "Int64":
            arr = Array[int64](elements)
        case "UInt64":
            arr = Array[uint64](elements)
        case "Float32":
            arr = Array[float32](elements)
        case "Float64":
            arr = Array[float64](elements)
        case "String":
            arr = Array[str](elements)
        case _:  # Generic
            arr = Array[Any](elements)

    # Test out-of-range indices
    with pytest.raises(IndexError):
        _ = arr[index]


@given(array_data=array_with_valid_index_and_new_value())
def test_fsharp_array_setitem(array_data: tuple[str, list[Any], int, Any]) -> None:
    """Test setting items in FSharpArray using hypothesis."""
    array_type, elements, index, new_value = array_data

    # Create array with the given elements and type
    arr: Array[Any]
    match array_type:
        case "Int8":
            arr = Array[sbyte](elements)
        case "UInt8":
            arr = Array[byte](elements)
        case "Int16":
            arr = Array[int16](elements)
        case "UInt16":
            arr = Array[uint16](elements)
        case "Int32":
            arr = Array[int32](elements)
        case "UInt32":
            arr = Array[uint32](elements)
        case "Int64":
            arr = Array[int64](elements)
        case "UInt64":
            arr = Array[uint64](elements)
        case "Float32":
            arr = Array[float32](elements)
        case "Float64":
            arr = Array[float64](elements)
        case "String":
            arr = Array[str](elements)
        case _:  # Generic
            arr = Array[Any](elements)

    # Set the new value
    arr[index] = new_value

    # Verify the value was set correctly
    if array_type in ["Float32", "Float64"]:
        assert arr[index] == pytest.approx(new_value)  # type: ignore
    else:
        assert arr[index] == new_value

    # Verify other elements remain unchanged
    for i in range(len(elements)):
        if i != index:
            if array_type in ["Float32", "Float64"]:
                assert arr[i] == pytest.approx(elements[i])  # type: ignore
            else:
                assert arr[i] == elements[i]


@given(array_data=array_with_invalid_index_and_new_value())
def test_fsharp_array_setitem_out_of_range(array_data: tuple[str, list[Any], int, Any]) -> None:
    """Test setting items with out-of-range indices using hypothesis."""
    array_type, elements, index, new_value = array_data

    arr: Array[Any]
    # Create array with the given elements and type
    match array_type:
        case "Int8":
            arr = Array[sbyte](elements)
        case "UInt8":
            arr = Array[byte](elements)
        case "Int16":
            arr = Array[int16](elements)
        case "UInt16":
            arr = Array[uint16](elements)
        case "Int32":
            arr = Array[int32](elements)
        case "UInt32":
            arr = Array[uint32](elements)
        case "Int64":
            arr = Array[int64](elements)
        case "UInt64":
            arr = Array[uint64](elements)
        case "Float32":
            arr = Array[float32](elements)
        case "Float64":
            arr = Array[float64](elements)
        case "String":
            arr = Array[str](elements)
        case _:  # Generic
            arr = Array[Any](elements)

    # Test out-of-range indices
    with pytest.raises(IndexError):
        arr[index] = new_value


def test_fsharp_array_mixed_types() -> None:
    """Test FSharpArray with mixed data types (Generic)."""
    mixed_list = [1, "hello", 3.14, True, None, sbyte(5)]
    arr: Array[Any] = Array[Any](mixed_list)

    # Check length
    assert len(arr) == len(mixed_list)

    # Check __getitem__
    assert arr[0] == 1
    assert arr[1] == "hello"
    assert arr[2] == 3.14
    assert arr[3] is True
    assert arr[4] is None
    assert arr[5] == sbyte(5)

    # Check __setitem__ with different types
    arr[0] = "world"
    assert arr[0] == "world"

    arr[1] = False
    assert arr[1] is False

    arr[4] = 100
    assert arr[4] == 100

    # Check length again after setting
    assert len(arr) == len(mixed_list)


@given(elements=st.lists(st.integers(min_value=-128, max_value=127), min_size=1, max_size=10))
def test_fsharp_array_map_int8(elements: list[int]) -> None:
    """Test mapping Int8 array by doubling values."""
    # Convert integers to sbyte
    sbyte_elements = [sbyte(x) for x in elements]

    # Create the array
    int8_array: Array[sbyte] = Array[sbyte](sbyte_elements)

    # Define the mapping function
    def double_value(x: sbyte) -> sbyte:
        return sbyte(x * 2)

    # Map the array
    mapped_array = int8_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(sbyte_elements):
        assert mapped_array[i] == sbyte(original * 2)


@given(elements=st.lists(st.integers(min_value=0, max_value=127), min_size=1, max_size=10))
def test_fsharp_array_map_uint8(elements: list[int]) -> None:
    """Test mapping UInt8 array by doubling values."""
    # Convert integers to byte
    byte_elements = [byte(x) for x in elements]

    # Create the array
    uint8_array: Array[byte] = Array[byte](byte_elements)

    # Define the mapping function
    def double_value(x: byte) -> byte:
        return byte(x * 2)

    # Map the array
    mapped_array = uint8_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(byte_elements):
        assert mapped_array[i] == byte(original * 2)


@given(elements=st.lists(st.integers(min_value=-2147483648, max_value=2147483647), min_size=1, max_size=10))
def test_fsharp_array_map_int32(elements: list[int]) -> None:
    """Test mapping Int32 array by doubling values."""
    # Convert integers to int32
    int32_elements = [int32(x) for x in elements]

    # Create the array
    int32_array: Array[int32] = Array[int32](int32_elements)

    # Define the mapping function
    def double_value(x: int32) -> int32:
        return int32(x * 2)

    # Map the array
    mapped_array = int32_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(int32_elements):
        assert mapped_array[i] == int32(original * 2)


@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=1, max_size=10))
def test_fsharp_array_map_float32(elements: list[float]) -> None:
    """Test mapping Float32 array by doubling values."""
    # Convert floats to float32
    float32_elements = [float32(x) for x in elements]

    # Create the array
    float32_array: Array[float32] = Array[float32](float32_elements)

    # Define the mapping function
    def double_value(x: float32) -> float32:
        return float32(x * 2.0)

    # Map the array
    mapped_array = float32_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(float32_elements):
        assert mapped_array[i] == pytest.approx(float32(original * 2.0))  # type: ignore


@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=1, max_size=10))
def test_fsharp_array_map_float64(elements: list[float]) -> None:
    """Test mapping Float64 array by doubling values."""
    # Convert floats to float64
    float64_elements = [float64(x) for x in elements]

    # Create the array
    float64_array: Array[float64] = Array[float64](float64_elements)

    # Define the mapping function
    def double_value(x: float64) -> float64:
        return float64(x * 2.0)

    # Map the array
    mapped_array = float64_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(float64_elements):
        assert mapped_array[i] == pytest.approx(float64(original * 2.0))  # type: ignore


@given(elements=st.lists(st.text(), min_size=1, max_size=10))
def test_fsharp_array_map_string(elements: list[str]) -> None:
    """Test mapping String array by duplicating each string."""
    # Create the array
    string_array: Array[str] = Array[str](elements)

    # Define the mapping function
    def duplicate_string(x: str) -> str:
        return x + x

    # Map the array
    mapped_array = string_array.map(duplicate_string)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(elements):
        assert mapped_array[i] == original + original


@given(elements=st.lists(st.one_of(st.integers(), st.text(), st.booleans(), st.none()), min_size=1, max_size=10))
def test_fsharp_array_map_generic(elements: list[Any]) -> None:
    """Test mapping Generic array by converting elements to strings."""
    # Create the array
    generic_array: Array[Any] = Array[Any](elements)

    # Define the mapping function
    def to_string(x: Any) -> str:
        return f"item: {x}"

    # Map the array
    mapped_array = generic_array.map(to_string)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(elements):
        assert mapped_array[i] == f"item: {original}"


@given(
    elements=st.lists(st.integers(min_value=-42, max_value=42), min_size=1, max_size=10),
)
def test_fsharp_array_map_with_constructor(elements: list[int]) -> None:
    """Test mapping an Int8 array to Int32 using a constructor."""
    # Convert integers to sbyte
    sbyte_elements = [sbyte(x) for x in elements]

    # Create the array
    int8_array: Array[sbyte] = Array[sbyte](sbyte_elements)

    # Create a constructor for Int32
    int32_cons: array.FSharpCons[int32] = array.FSharpCons("Int32")

    # Define the mapping function
    def to_int32(x: sbyte) -> int32:
        return int32(x * 3)

    # Map the array using the constructor
    mapped_array = int8_array.map(to_int32, int32_cons)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(sbyte_elements):
        assert mapped_array[i] == int32(original * 3)


@given(
    elements=st.lists(st.integers(min_value=-128, max_value=127), min_size=1, max_size=10),
)
def test_fsharp_array_map_to_float(elements: list[int]) -> None:
    """Test mapping an Int8 array to Float64 using a constructor."""
    # Convert integers to sbyte
    sbyte_elements = [sbyte(x) for x in elements]

    # Create the array
    int8_array: Array[sbyte] = Array[sbyte](sbyte_elements)

    # Create a constructor for Float64
    float64_cons: array.FSharpCons[float64] = array.FSharpCons("Float64")

    # Define the mapping function
    def to_float64(x: sbyte) -> float64:
        return float64(float(x) * 1.5)

    # Map the array using the constructor
    mapped_array = int8_array.map(to_float64, float64_cons)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(sbyte_elements):
        assert mapped_array[i] == pytest.approx(float64(float(original) * 1.5))  # type: ignore


@given(
    elements=st.lists(st.integers(min_value=-128, max_value=127), min_size=1, max_size=10),
)
def test_fsharp_array_map_to_string(elements: list[int]) -> None:
    """Test mapping an Int8 array to String."""
    # Convert integers to sbyte
    sbyte_elements = [sbyte(x) for x in elements]

    # Create the array
    int8_array: Array[sbyte] = Array[sbyte](sbyte_elements)

    # Create a constructor for String
    string_cons: array.FSharpCons[str] = array.FSharpCons("String")

    # Define the mapping function
    def to_string(x: sbyte) -> str:
        return f"Value: {x}"

    # Map the array using the constructor
    mapped_array = int8_array.map(to_string, string_cons)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(sbyte_elements):
        assert mapped_array[i] == f"Value: {original}"


def test_fsharp_array_map_error_handling() -> None:
    """Test error handling within the map function's callback."""
    int_array: Array[int32] = Array[int32]([int32(1), int32(2), int32(3)])

    def faulty_mapper(x: int32) -> int32:
        if x == 2:
            raise ValueError("Error during mapping")
        return x * 10

    with pytest.raises(ValueError, match="Error during mapping"):
        # The error should propagate out of the map call
        _ = int_array.map(faulty_mapper)


def test_fsharp_array_filter() -> None:
    """Test the filter function of FSharpArray."""
    # Create arrays with elements
    int32_array: Array[int32] = Array[int32]([int32(1), int32(2), int32(3), int32(4)])
    float32_array: Array[float32] = Array[float32]([float32(1.1), float32(-2.2), float32(3.3), float32(0.0)])
    float64_array: Array[float64] = Array[float64]([float64(1.1), float64(-2.2), float64(3.3), float64(0.0)])
    string_array: Array[str] = Array[str](["a", "bb", "ccc", "d"])
    generic_array: Array[Any] = Array[Any]([1, "a", True, 2.0, sbyte(5)])
    empty_array: Array[Any] = Array[Any]([])

    # Test filtering Int32 (keep even numbers)
    filtered_int32 = int32_array.filter(lambda x: x % 2 == 0)
    assert len(filtered_int32) == 2
    assert filtered_int32[0] == int32(2)
    assert filtered_int32[1] == int32(4)

    # Test filtering Float32 (keep positive values)
    filtered_float32 = float32_array.filter(lambda x: x > 0)
    assert len(filtered_float32) == 2
    assert filtered_float32[0] == pytest.approx(1.1)  # type: ignore
    assert filtered_float32[1] == pytest.approx(3.3)  # type: ignore

    # Test filtering Float64 (keep non-zero values)
    filtered_float64 = float64_array.filter(lambda x: x != 0.0)
    assert len(filtered_float64) == 3
    assert filtered_float64[0] == pytest.approx(1.1)  # type: ignore
    assert filtered_float64[1] == pytest.approx(-2.2)  # type: ignore
    assert filtered_float64[2] == pytest.approx(3.3)  # type: ignore

    # Test filtering String (keep length > 1)
    filtered_string = string_array.filter(lambda x: len(x) > 1)
    assert len(filtered_string) == 2
    assert filtered_string[0] == "bb"
    assert filtered_string[1] == "ccc"

    # Test filtering Generic (keep integers)
    filtered_generic = generic_array.filter(lambda x: isinstance(x, int) and not isinstance(x, bool))
    assert len(filtered_generic) == 1
    assert filtered_generic[0] == 1  # Note: sbyte(5) is not an int instance

    # Test filtering - keep all
    filtered_keep_all = int32_array.filter(lambda x: True)
    assert len(filtered_keep_all) == 4
    assert filtered_keep_all[0] == int32(1)
    assert filtered_keep_all[1] == int32(2)
    assert filtered_keep_all[2] == int32(3)
    assert filtered_keep_all[3] == int32(4)

    # Test filtering - keep none
    filtered_keep_none = int32_array.filter(lambda x: False)
    assert len(filtered_keep_none) == 0

    # Test filtering empty array
    filtered_empty = empty_array.filter(lambda x: True)
    assert len(filtered_empty) == 0


def test_fsharp_array_filter_error_handling() -> None:
    """Test error handling within the filter function's predicate."""
    string_array: Array[str] = Array[str](["a", "b", "c"])

    def faulty_predicate(x: str) -> bool:
        if x == "b":
            raise ValueError("Error during filtering")
        return len(x) == 1

    with pytest.raises(ValueError, match="Error during filtering"):
        # The error should propagate out of the filter call
        _ = string_array.filter(faulty_predicate)


@pytest.mark.parametrize("array_type", ["Int8", "Int32", "Float64", "String", "Generic"])
def test_fsharp_array_append(array_type: ValidArrayType) -> None:
    """Test appending two FSharpArrays of the same type."""
    # Create arrays of the same type
    array1: Array[Any]
    array2: Array[Any]
    elements1: list[Any]
    elements2: list[Any]
    match array_type:
        case "Int8":
            elements1 = [sbyte(1), sbyte(2), sbyte(3)]
            elements2 = [sbyte(4), sbyte(5)]
            array1 = Array[sbyte](elements1)
            array2 = Array[sbyte](elements2)
        case "Int32":
            elements1 = [int32(1), int32(2), int32(3)]
            elements2 = [int32(4), int32(5)]
            array1 = Array[int32](elements1)
            array2 = Array[int32](elements2)
        case "Float64":
            elements1 = [float64(1.1), float64(2.2)]
            elements2 = [float64(3.3), float64(4.4), float64(5.5)]
            array1 = Array[float64](elements1)
            array2 = Array[float64](elements2)
        case "String":
            elements1 = ["a", "b", "c"]
            elements2 = ["d", "e"]
            array1 = Array[str](elements1)
            array2 = Array[str](elements2)
        case _:  # Generic
            elements1 = [1, "a", True]
            elements2 = [False, 2]
            array1 = Array[Any](elements1)
            array2 = Array[Any](elements2)

    # Append arrays
    result = array.append(array1, array2)

    # Check length
    assert len(result) == len(elements1) + len(elements2)

    # Check each element
    for i, element in enumerate(elements1):
        if array_type in ["Float32", "Float64"]:
            assert result[i] == pytest.approx(element)  # type: ignore
        else:
            assert result[i] == element

    for i, element in enumerate(elements2):
        if array_type in ["Float32", "Float64"]:
            assert result[i + len(elements1)] == pytest.approx(element)  # type: ignore
        else:
            assert result[i + len(elements1)] == element


# Test Int8 arrays
@given(elements=st.lists(st.integers(min_value=-128, max_value=127), min_size=0))
def test_int8_array_create(elements: list[int]) -> None:
    """Test creating Int8 arrays."""
    sbyte_elements = [sbyte(x) for x in elements]
    arr: Array[sbyte] = Array[sbyte](sbyte_elements)
    assert len(arr) == len(elements)
    for i, element in enumerate(sbyte_elements):
        assert arr[i] == element


@given(elements=st.lists(st.integers(min_value=-128, max_value=127), min_size=1))
def test_int8_array_getitem(elements: list[int]) -> None:
    """Test getting items from Int8 arrays."""
    sbyte_elements = [sbyte(x) for x in elements]
    arr: Array[sbyte] = Array[sbyte](sbyte_elements)
    for i, element in enumerate(sbyte_elements):
        assert arr[i] == element


@given(elements=st.lists(st.integers(min_value=-128, max_value=127), min_size=1))
def test_int8_array_setitem(elements: list[int]) -> None:
    """Test setting items in Int8 arrays."""
    sbyte_elements = [sbyte(x) for x in elements]
    arr: Array[sbyte] = Array[sbyte](sbyte_elements)
    new_value = sbyte(42)
    arr[0] = new_value
    assert arr[0] == new_value
    for i in range(1, len(elements)):
        assert arr[i] == sbyte_elements[i]


# Test UInt8 arrays
@given(elements=st.lists(st.integers(min_value=0, max_value=255), min_size=0))
def test_uint8_array_create(elements: list[int]) -> None:
    """Test creating UInt8 arrays."""
    byte_elements = [byte(x) for x in elements]
    arr: Array[byte] = Array[byte](byte_elements)
    assert len(arr) == len(elements)
    for i, element in enumerate(byte_elements):
        assert arr[i] == element


@given(elements=st.lists(st.integers(min_value=0, max_value=255), min_size=1))
def test_uint8_array_getitem(elements: list[int]) -> None:
    """Test getting items from UInt8 arrays."""
    byte_elements = [byte(x) for x in elements]
    arr: Array[byte] = Array[byte](byte_elements)
    for i, element in enumerate(byte_elements):
        assert arr[i] == element


@given(elements=st.lists(st.integers(min_value=0, max_value=255), min_size=1))
def test_uint8_array_setitem(elements: list[int]) -> None:
    """Test setting items in UInt8 arrays."""
    byte_elements = [byte(x) for x in elements]
    arr: Array[byte] = Array[byte](byte_elements)
    new_value = byte(42)
    arr[0] = new_value
    assert arr[0] == new_value
    for i in range(1, len(elements)):
        assert arr[i] == byte_elements[i]


# Test Int32 arrays
@given(elements=st.lists(st.integers(min_value=-2147483648, max_value=2147483647), min_size=0))
def test_int32_array_create(elements: list[int]) -> None:
    """Test creating Int32 arrays."""
    int32_elements = [int32(x) for x in elements]
    arr: Array[int32] = Array[int32](int32_elements)
    assert len(arr) == len(elements)
    for i, element in enumerate(int32_elements):
        assert arr[i] == element


@given(elements=st.lists(st.integers(min_value=-2147483648, max_value=2147483647), min_size=1))
def test_int32_array_getitem(elements: list[int]) -> None:
    """Test getting items from Int32 arrays."""
    int32_elements = [int32(x) for x in elements]
    arr: Array[int32] = Array[int32](int32_elements)
    for i, element in enumerate(int32_elements):
        assert arr[i] == element


@given(elements=st.lists(st.integers(min_value=-2147483648, max_value=2147483647), min_size=1))
def test_int32_array_setitem(elements: list[int]) -> None:
    """Test setting items in Int32 arrays."""
    int32_elements = [int32(x) for x in elements]
    arr: Array[int32] = Array[int32](int32_elements)
    new_value = int32(42)
    arr[0] = new_value
    assert arr[0] == new_value
    for i in range(1, len(elements)):
        assert arr[i] == int32_elements[i]


# Test Float32 arrays
@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=0))
def test_float32_array_create(elements: list[float]) -> None:
    """Test creating Float32 arrays."""
    float32_elements = [float32(x) for x in elements]
    arr: Array[float32] = Array[float32](float32_elements)
    assert len(arr) == len(elements)
    for i, element in enumerate(float32_elements):
        assert arr[i] == pytest.approx(element)  # type: ignore


@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=1))
def test_float32_array_getitem(elements: list[float]) -> None:
    """Test getting items from Float32 arrays."""
    float32_elements = [float32(x) for x in elements]
    arr: Array[float32] = Array[float32](float32_elements)
    for i, element in enumerate(float32_elements):
        assert arr[i] == pytest.approx(element)  # type: ignore


@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=1))
def test_float32_array_setitem(elements: list[float]) -> None:
    """Test setting items in Float32 arrays."""
    float32_elements = [float32(x) for x in elements]
    arr: Array[float32] = Array[float32](float32_elements)
    new_value = float32(42.0)
    arr[0] = new_value
    assert arr[0] == pytest.approx(new_value)  # type: ignore
    for i in range(1, len(elements)):
        assert arr[i] == pytest.approx(float32_elements[i])  # type: ignore


# Test Float64 arrays
@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=0))
def test_float64_array_create(elements: list[float]) -> None:
    """Test creating Float64 arrays."""
    float64_elements = [float64(x) for x in elements]
    arr: Array[float64] = Array[float64](float64_elements)
    assert len(arr) == len(elements)
    for i, element in enumerate(float64_elements):
        assert arr[i] == pytest.approx(element)  # type: ignore


@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=1))
def test_float64_array_getitem(elements: list[float]) -> None:
    """Test getting items from Float64 arrays."""
    float64_elements = [float64(x) for x in elements]
    arr: Array[float64] = Array[float64](float64_elements)
    for i, element in enumerate(float64_elements):
        assert arr[i] == pytest.approx(element)  # type: ignore


@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=1))
def test_float64_array_setitem(elements: list[float]) -> None:
    """Test setting items in Float64 arrays."""
    float64_elements = [float64(x) for x in elements]
    arr: Array[float64] = Array[float64](float64_elements)
    new_value = float64(42.0)
    arr[0] = new_value
    assert arr[0] == pytest.approx(new_value)  # type: ignore
    for i in range(1, len(elements)):
        assert arr[i] == pytest.approx(float64_elements[i])  # type: ignore


# Test String arrays
@given(elements=st.lists(st.text(), min_size=0))
def test_string_array_create(elements: list[str]) -> None:
    """Test creating String arrays."""
    arr: Array[str] = Array[str](elements)
    assert len(arr) == len(elements)
    for i, element in enumerate(elements):
        assert arr[i] == element


@given(elements=st.lists(st.text(), min_size=1))
def test_string_array_getitem(elements: list[str]) -> None:
    """Test getting items from String arrays."""
    arr: Array[str] = Array[str](elements)
    for i, element in enumerate(elements):
        assert arr[i] == element


@given(elements=st.lists(st.text(), min_size=1))
def test_string_array_setitem(elements: list[str]) -> None:
    """Test setting items in String arrays."""
    arr: Array[str] = Array[str](elements)
    new_value = "new value"
    arr[0] = new_value
    assert arr[0] == new_value
    for i in range(1, len(elements)):
        assert arr[i] == elements[i]


# Test Generic arrays
@given(elements=st.lists(anything, min_size=0))
def test_generic_array_create(elements: list[Any]) -> None:
    """Test creating Generic arrays."""
    arr: Array[Any] = Array[Any](elements)
    assert len(arr) == len(elements)
    for i, element in enumerate(elements):
        assert arr[i] == element


@given(elements=st.lists(anything, min_size=1))
def test_generic_array_getitem(elements: list[Any]) -> None:
    """Test getting items from Generic arrays."""
    arr: Array[Any] = Array[Any](elements)
    for i, element in enumerate(elements):
        assert arr[i] == element


@given(elements=st.lists(anything, min_size=1))
def test_generic_array_setitem(elements: list[Any]) -> None:
    """Test setting items in Generic arrays."""
    arr: Array[Any] = Array[Any](elements)
    new_value = "new value"
    arr[0] = new_value
    assert arr[0] == new_value
    for i in range(1, len(elements)):
        assert arr[i] == elements[i]


# Test array operations
def test_array_map() -> None:
    """Test mapping arrays."""
    # Test Int8 array mapping
    int8_arr: Array[sbyte] = Array[sbyte]([sbyte(1), sbyte(2), sbyte(3)])
    mapped_int8 = int8_arr.map(lambda x: sbyte(x * 2))
    assert len(mapped_int8) == 3
    assert mapped_int8[0] == sbyte(2)
    assert mapped_int8[1] == sbyte(4)
    assert mapped_int8[2] == sbyte(6)

    # Test Float32 array mapping
    float32_arr: Array[float32] = Array[float32]([float32(1.1), float32(2.2), float32(3.3)])
    mapped_float32 = float32_arr.map(lambda x: float32(x * 2.0))
    assert len(mapped_float32) == 3
    assert mapped_float32[0] == pytest.approx(float32(2.2))  # type: ignore
    assert mapped_float32[1] == pytest.approx(float32(4.4))  # type: ignore
    assert mapped_float32[2] == pytest.approx(float32(6.6))  # type: ignore

    # Test String array mapping
    string_arr: Array[str] = Array[str](["a", "b", "c"])
    mapped_string = string_arr.map(lambda x: x + x)
    assert len(mapped_string) == 3
    assert mapped_string[0] == "aa"
    assert mapped_string[1] == "bb"
    assert mapped_string[2] == "cc"


def test_array_filter() -> None:
    """Test filtering arrays."""
    # Test Int8 array filtering
    int8_arr: Array[sbyte] = Array[sbyte]([sbyte(1), sbyte(2), sbyte(3), sbyte(4)])
    filtered_int8 = int8_arr.filter(lambda x: x % 2 == 0)
    assert len(filtered_int8) == 2
    assert filtered_int8[0] == sbyte(2)
    assert filtered_int8[1] == sbyte(4)

    # Test Float32 array filtering
    float32_arr: Array[float32] = Array[float32]([float32(1.1), float32(-2.2), float32(3.3), float32(0.0)])
    filtered_float32 = float32_arr.filter(lambda x: x > 0)
    assert len(filtered_float32) == 2
    assert filtered_float32[0] == pytest.approx(float32(1.1))  # type: ignore
    assert filtered_float32[1] == pytest.approx(float32(3.3))  # type: ignore

    # Test String array filtering
    string_arr: Array[str] = Array[str](["a", "bb", "ccc", "d"])
    filtered_string = string_arr.filter(lambda x: len(x) > 1)
    assert len(filtered_string) == 2
    assert filtered_string[0] == "bb"
    assert filtered_string[1] == "ccc"


def test_array_append() -> None:
    """Test appending arrays."""
    # Test Int8 array appending
    int8_arr1: Array[sbyte] = Array[sbyte]([sbyte(1), sbyte(2), sbyte(3)])
    int8_arr2: Array[sbyte] = Array[sbyte]([sbyte(4), sbyte(5)])
    result_int8 = array.append(int8_arr1, int8_arr2)
    assert len(result_int8) == 5
    assert result_int8[0] == sbyte(1)
    assert result_int8[1] == sbyte(2)
    assert result_int8[2] == sbyte(3)
    assert result_int8[3] == sbyte(4)
    assert result_int8[4] == sbyte(5)

    # Test Float32 array appending
    float32_arr1: Array[float32] = Array[float32]([float32(1.1), float32(2.2)])
    float32_arr2: Array[float32] = Array[float32]([float32(3.3), float32(4.4), float32(5.5)])
    result_float32 = array.append(float32_arr1, float32_arr2)
    assert len(result_float32) == 5
    assert result_float32[0] == pytest.approx(float32(1.1))  # type: ignore
    assert result_float32[1] == pytest.approx(float32(2.2))  # type: ignore
    assert result_float32[2] == pytest.approx(float32(3.3))  # type: ignore
    assert result_float32[3] == pytest.approx(float32(4.4))  # type: ignore
    assert result_float32[4] == pytest.approx(float32(5.5))  # type: ignore

    # Test String array appending
    string_arr1: Array[str] = Array[str](["a", "b", "c"])
    string_arr2: Array[str] = Array[str](["d", "e"])
    result_string = array.append(string_arr1, string_arr2)
    assert len(result_string) == 5
    assert result_string[0] == "a"
    assert result_string[1] == "b"
    assert result_string[2] == "c"
    assert result_string[3] == "d"
    assert result_string[4] == "e"


def test_array_error_handling() -> None:
    """Test error handling in array operations."""
    # Test index out of range
    int8_arr: Array[sbyte] = Array[sbyte]([sbyte(1), sbyte(2), sbyte(3)])
    with pytest.raises(IndexError):
        _ = int8_arr[3]
    with pytest.raises(IndexError):
        int8_arr[3] = sbyte(4)

    # Test map error handling
    def faulty_mapper(x: sbyte) -> sbyte:
        if x == sbyte(2):
            raise ValueError("Error during mapping")
        return x * 2

    with pytest.raises(ValueError, match="Error during mapping"):
        _ = int8_arr.map(faulty_mapper)

    # Test filter error handling
    def faulty_predicate(x: sbyte) -> bool:
        if x == sbyte(2):
            raise ValueError("Error during filtering")
        return x > 0

    with pytest.raises(ValueError, match="Error during filtering"):
        _ = int8_arr.filter(faulty_predicate)
