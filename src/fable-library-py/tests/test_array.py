from typing import Any

import hypothesis.strategies as st
import pytest
from fable_library.core import (
    ArrayType,
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


Array = array.FSharpArray

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
def array_with_elements(draw, min_size=0):
    # Draw the array type
    array_type = draw(st.sampled_from(list(array_types.keys())))
    # Draw elements of appropriate type
    elements = draw(st.lists(array_types[array_type], min_size=min_size))
    return array_type, elements


@st.composite
def array_with_valid_index(draw):
    """Generate a tuple of (array_type, elements, index) where index is valid."""
    array_type, elements = draw(array_with_elements(min_size=1))
    # Draw a valid index
    index = draw(st.integers(min_value=0, max_value=len(elements) - 1))
    return array_type, elements, index


@st.composite
def array_with_invalid_index(draw):
    """Generate a tuple of (array_type, elements, index) where index is invalid."""
    array_type, elements = draw(array_with_elements(min_size=1))
    # Draw an invalid index
    index = draw(st.integers(min_value=len(elements), max_value=len(elements) + 10))
    return array_type, elements, index


@st.composite
def array_with_valid_index_and_new_value(draw):
    """Generate a tuple of (array_type, elements, index, new_value) where index is valid."""
    array_type, elements = draw(array_with_elements(min_size=1))

    # Draw a valid index
    index = draw(st.integers(min_value=0, max_value=len(elements) - 1))

    # Generate a new value by reusing the array_types strategy that's already defined
    # This avoids the long if-elif-else chain
    new_value = draw(array_types[array_type])

    return array_type, elements, index, new_value


@st.composite
def array_with_invalid_index_and_new_value(draw):
    """Generate a tuple of (array_type, elements, index, new_value) where index is invalid."""
    array_type, elements = draw(array_with_elements(min_size=1))

    # Draw an invalid index
    index = draw(st.integers(min_value=len(elements), max_value=len(elements) + 10))

    # Generate a new value of appropriate type
    new_value = draw(array_types[array_type])

    return array_type, elements, index, new_value


@pytest.mark.parametrize("array_type", array_types.keys())
def test_fsharp_array_create_empty(array_type: ArrayType) -> None:
    """Test creating empty FSharpArray instances with different types."""
    # Create empty array with specified type
    empty_array: Array = Array(array_type=array_type)

    # Check length
    assert len(empty_array) == 0


@given(array_data=array_with_elements())
def test_fsharp_array_create_with_elements(array_data: tuple[ArrayType, list[Any]]) -> None:
    """Test creating FSharpArray instances with elements."""
    array_type, elements = array_data

    # Create array with the given elements and type
    array_obj = array.FSharpArray(array_type, elements)

    # Check length
    assert len(array_obj) == len(elements)

    # Check each element
    for i, element in enumerate(elements):
        assert array_obj[i] == element


@given(
    input_data=st.lists(anything, min_size=1, max_size=20),
    invalid_type=st.text().filter(lambda x: x not in array_types.keys()),
)
def test_fsharp_array_invalid_type(input_data, invalid_type) -> None:
    """Test creating FSharpArray with an invalid array_type string defaults to Generic."""
    try:
        # Create the array with invalid type
        arr: Array = array.FSharpArray(invalid_type, input_data)

        # Verify the array contains the same input data
        assert list(arr) == input_data
    except Exception as e:
        pytest.fail(f"Creating FSharpArray with invalid type raised unexpected error: {e}")


def test_fsharp_cons_invalid_type():
    """Test creating FSharpCons with an invalid array_type string defaults to Generic."""
    try:
        cons = array.FSharpCons("AnotherInvalidType")  # type:ignore[assignment]
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
def test_fsharp_array_getitem(array_data: tuple[ArrayType, list[Any], int]) -> None:
    """Test getting items from FSharpArray using hypothesis."""
    array_type, elements, index = array_data

    # Create array with the given elements and type
    array_obj = array.FSharpArray(array_type, elements)

    # Get the expected value using Python's list indexing
    expected_value = elements[index]

    # Test getting item from FSharpArray
    actual_value = array_obj[index]

    # Handle float comparisons
    if array_type in ["Float32", "Float64"]:
        assert actual_value == pytest.approx(expected_value)
    else:
        assert actual_value == expected_value


@given(array_data=array_with_invalid_index())
def test_fsharp_array_getitem_out_of_range(array_data: tuple[ArrayType, list[Any], int]) -> None:
    """Test getting items with out-of-range indices using hypothesis."""
    array_type, elements, index = array_data

    # Create array with the given elements and type
    arr = array.FSharpArray(array_type, elements)

    # Test out-of-range indices
    with pytest.raises(IndexError):
        _ = arr[index]


@given(array_data=array_with_valid_index_and_new_value())
def test_fsharp_array_setitem(array_data: tuple[ArrayType, list[Any], int, Any]) -> None:
    """Test setting items in FSharpArray using hypothesis."""

    array_type, elements, index, new_value = array_data

    # Create array with the given elements and type
    array_obj = array.FSharpArray(array_type, elements)

    # Set the new value
    array_obj[index] = new_value

    # Verify the value was set correctly
    if array_type in ["Float32", "Float64"]:
        assert array_obj[index] == pytest.approx(new_value)
    else:
        assert array_obj[index] == new_value

    # Verify other elements remain unchanged
    for i in range(len(elements)):
        if i != index:
            if array_type in ["Float32", "Float64"]:
                assert array_obj[i] == pytest.approx(elements[i])
            else:
                assert array_obj[i] == elements[i]


@given(array_data=array_with_invalid_index_and_new_value())
def test_fsharp_array_setitem_out_of_range(array_data: tuple[ArrayType, list[Any], int, Any]) -> None:
    """Test setting items with out-of-range indices using hypothesis."""
    array_type, elements, index, new_value = array_data

    # Create array with the given elements and type
    arr = array.FSharpArray(array_type, elements)

    # Test out-of-range indices
    with pytest.raises(IndexError):
        arr[index] = new_value


def test_fsharp_array_mixed_types():
    """Test FSharpArray with mixed data types (Generic)."""
    mixed_list = [1, "hello", 3.14, True, None, sbyte(5)]
    arr = array.FSharpArray("Generic", mixed_list)

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
def test_fsharp_array_map_int8(elements):
    """Test mapping Int8 array by doubling values."""
    # Convert integers to sbyte
    sbyte_elements = [sbyte(x) for x in elements]

    # Create the array
    int8_array = array.FSharpArray("Int8", sbyte_elements)

    # Define the mapping function
    def double_value(x):
        return sbyte(x * 2)

    # Map the array
    mapped_array = int8_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(sbyte_elements):
        assert mapped_array[i] == sbyte(original * 2)


@given(elements=st.lists(st.integers(min_value=0, max_value=127), min_size=1, max_size=10))
def test_fsharp_array_map_uint8(elements):
    """Test mapping UInt8 array by doubling values."""
    # Convert integers to byte
    byte_elements = [byte(x) for x in elements]

    # Create the array
    uint8_array = array.FSharpArray("UInt8", byte_elements)

    # Define the mapping function
    def double_value(x):
        return byte(x * 2)

    # Map the array
    mapped_array = uint8_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(byte_elements):
        assert mapped_array[i] == byte(original * 2)


@given(elements=st.lists(st.integers(min_value=-2147483648, max_value=2147483647), min_size=1, max_size=10))
def test_fsharp_array_map_int32(elements):
    """Test mapping Int32 array by doubling values."""
    # Convert integers to int32
    int32_elements = [int32(x) for x in elements]

    # Create the array
    int32_array = array.FSharpArray("Int32", int32_elements)

    # Define the mapping function
    def double_value(x):
        return int32(x * 2)

    # Map the array
    mapped_array = int32_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(int32_elements):
        assert mapped_array[i] == int32(original * 2)


@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=1, max_size=10))
def test_fsharp_array_map_float32(elements):
    """Test mapping Float32 array by doubling values."""
    # Convert floats to float32
    float32_elements = [float32(x) for x in elements]

    # Create the array
    float32_array = array.FSharpArray("Float32", float32_elements)

    # Define the mapping function
    def double_value(x):
        return float32(x * 2.0)

    # Map the array
    mapped_array = float32_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(float32_elements):
        assert mapped_array[i] == pytest.approx(float32(original * 2.0))


@given(elements=st.lists(st.floats(allow_nan=False, allow_infinity=False), min_size=1, max_size=10))
def test_fsharp_array_map_float64(elements):
    """Test mapping Float64 array by doubling values."""
    # Convert floats to float64
    float64_elements = [float64(x) for x in elements]

    # Create the array
    float64_array = array.FSharpArray("Float64", float64_elements)

    # Define the mapping function
    def double_value(x):
        return float64(x * 2.0)

    # Map the array
    mapped_array = float64_array.map(double_value)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(float64_elements):
        assert mapped_array[i] == pytest.approx(float64(original * 2.0))


@given(elements=st.lists(st.text(), min_size=1, max_size=10))
def test_fsharp_array_map_string(elements):
    """Test mapping String array by duplicating each string."""
    # Create the array
    string_array = array.FSharpArray("String", elements)

    # Define the mapping function
    def duplicate_string(x):
        return x + x

    # Map the array
    mapped_array = string_array.map(duplicate_string)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(elements):
        assert mapped_array[i] == original + original


@given(elements=st.lists(st.one_of(st.integers(), st.text(), st.booleans(), st.none()), min_size=1, max_size=10))
def test_fsharp_array_map_generic(elements):
    """Test mapping Generic array by converting elements to strings."""
    # Create the array
    generic_array = array.FSharpArray("Generic", elements)

    # Define the mapping function
    def to_string(x):
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
def test_fsharp_array_map_with_constructor(elements):
    """Test mapping an Int8 array to Int32 using a constructor."""
    # Convert integers to sbyte
    sbyte_elements = [sbyte(x) for x in elements]

    # Create the array
    int8_array = array.FSharpArray("Int8", sbyte_elements)

    # Create a constructor for Int32
    int32_cons = array.FSharpCons("Int32")

    # Define the mapping function
    def to_int32(x):
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
def test_fsharp_array_map_to_float(elements):
    """Test mapping an Int8 array to Float64 using a constructor."""
    # Convert integers to sbyte
    sbyte_elements = [sbyte(x) for x in elements]

    # Create the array
    int8_array = array.FSharpArray("Int8", sbyte_elements)

    # Create a constructor for Float64
    float64_cons = array.FSharpCons("Float64")

    # Define the mapping function
    def to_float64(x):
        return float64(float(x) * 1.5)

    # Map the array using the constructor
    mapped_array = int8_array.map(to_float64, float64_cons)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(sbyte_elements):
        assert mapped_array[i] == pytest.approx(float64(float(original) * 1.5))


@given(
    elements=st.lists(st.integers(min_value=-128, max_value=127), min_size=1, max_size=10),
)
def test_fsharp_array_map_to_string(elements):
    """Test mapping an Int8 array to String."""
    # Convert integers to sbyte
    sbyte_elements = [sbyte(x) for x in elements]

    # Create the array
    int8_array = array.FSharpArray("Int8", sbyte_elements)

    # Create a constructor for String
    string_cons = array.FSharpCons("String")

    # Define the mapping function
    def to_string(x):
        return f"Value: {x}"

    # Map the array using the constructor
    mapped_array = int8_array.map(to_string, string_cons)

    # Check results
    assert len(mapped_array) == len(elements)
    for i, original in enumerate(sbyte_elements):
        assert mapped_array[i] == f"Value: {original}"


def test_fsharp_cons():
    """Test the FSharpCons class."""
    # Create constructors for different types
    int8_cons = array.FSharpCons("Int8")
    uint8_cons = array.FSharpCons("UInt8")
    int16_cons = array.FSharpCons("Int16")
    uint16_cons = array.FSharpCons("UInt16")
    int32_cons = array.FSharpCons("Int32")
    uint32_cons = array.FSharpCons("UInt32")
    int64_cons = array.FSharpCons("Int64")
    uint64_cons = array.FSharpCons("UInt64")
    float32_cons = array.FSharpCons("Float32")
    float64_cons = array.FSharpCons("Float64")
    string_cons = array.FSharpCons("String")
    generic_cons = array.FSharpCons("Generic")

    # Test allocate method - this only sets capacity, not initialized values
    int8_array = int8_cons.allocate(3)
    assert len(int8_array) == 0  # allocate only sets capacity, not length

    uint8_array = uint8_cons.allocate(3)
    assert len(uint8_array) == 0  # allocate only sets capacity, not length

    float32_array_alloc = float32_cons.allocate(3)
    assert len(float32_array_alloc) == 0  # allocate only sets capacity, not length

    float64_array_alloc = float64_cons.allocate(3)
    assert len(float64_array_alloc) == 0  # allocate only sets capacity, not length

    string_array = string_cons.allocate(3)
    assert len(string_array) == 0  # allocate only sets capacity, not length

    int16_array_alloc = int16_cons.allocate(2)
    assert len(int16_array_alloc) == 0  # allocate only sets capacity, not length

    generic_array_alloc = generic_cons.allocate(4)
    assert len(generic_array_alloc) == 0  # allocate only sets capacity, not length

    uint16_array_alloc = uint16_cons.allocate(1)
    assert len(uint16_array_alloc) == 0  # allocate only sets capacity, not length

    # Test __call__ method - same as allocate
    int32_array_call = int32_cons(3)
    assert len(int32_array_call) == 0  # __call__ only sets capacity, not length

    int64_array_call = int64_cons(2)
    assert len(int64_array_call) == 0  # __call__ only sets capacity, not length

    uint32_array_call = uint32_cons(4)
    assert len(uint32_array_call) == 0  # __call__ only sets capacity, not length

    uint64_array_call = uint64_cons(2)
    assert len(uint64_array_call) == 0  # __call__ only sets capacity, not length

    generic_array_call = generic_cons(1)
    assert len(generic_array_call) == 0  # __call__ only sets capacity, not length


def test_allocate_array_from_cons():
    """Test the allocate_array_from_cons function."""
    # Create constructors for different types
    int8_cons = array.FSharpCons("Int8")
    uint8_cons = array.FSharpCons("UInt8")
    float32_cons = array.FSharpCons("Float32")
    float64_cons = array.FSharpCons("Float64")

    # Test with constructor - allocate_array_from_cons only sets capacity, not initialized values
    int8_array = array.allocate_array_from_cons(int8_cons, 3)
    assert len(int8_array) == 0  # Should be empty since we are not initializing with values

    uint8_array = array.allocate_array_from_cons(uint8_cons, 3)
    assert len(uint8_array) == 0  # Should be empty since we are not initializing with values

    float32_array = array.allocate_array_from_cons(float32_cons, 3)
    assert len(float32_array) == 0  # Should be empty since we are not initializing with values

    float64_array = array.allocate_array_from_cons(float64_cons, 3)
    assert len(float64_array) == 0  # Should be empty since we are not initializing with values

    # Test without constructor
    generic_array = array.allocate_array_from_cons(None, 3)
    assert len(generic_array) == 0  # Should be empty since we are not initializing with values


def test_fsharp_array_map_with_cons():
    """Test the map function with a constructor."""
    # Create arrays with elements
    int8_array = array.FSharpArray("Int8", [sbyte(1), sbyte(2), sbyte(3)])

    # Create constructors
    int32_cons = array.FSharpCons("Int32")
    float64_cons = array.FSharpCons("Float64")

    # Test mapping with constructor
    mapped_array = int8_array.map(lambda x: int32(x * 2), int32_cons)
    assert len(mapped_array) == 3
    assert mapped_array[0] == int32(2)
    assert mapped_array[1] == int32(4)
    assert mapped_array[2] == int32(6)

    # Test mapping int to float64 with constructor
    # Cast x (sbyte) to float before multiplying with 1.5
    mapped_float_array = int8_array.map(lambda x: float64(float(x) * 1.5), float64_cons)
    assert len(mapped_float_array) == 3
    assert mapped_float_array[0] == pytest.approx(1.5)
    assert mapped_float_array[1] == pytest.approx(3.0)
    assert mapped_float_array[2] == pytest.approx(4.5)


def test_fsharp_array_map_error_handling():
    """Test error handling within the map function's callback."""
    int_array = array.FSharpArray("Int32", [int32(1), int32(2), int32(3)])

    def faulty_mapper(x):
        if x == 2:
            raise ValueError("Error during mapping")
        return x * 10

    with pytest.raises(ValueError, match="Error during mapping"):
        # The error should propagate out of the map call
        _ = int_array.map(faulty_mapper)


def test_fsharp_array_filter():
    """Test the filter function of FSharpArray."""
    # Create arrays with elements
    int32_array = array.FSharpArray("Int32", [int32(1), int32(2), int32(3), int32(4)])
    float32_array = array.FSharpArray("Float32", [float32(1.1), float32(-2.2), float32(3.3), float32(0.0)])
    float64_array = array.FSharpArray("Float64", [float64(1.1), float64(-2.2), float64(3.3), float64(0.0)])
    string_array = array.FSharpArray("String", ["a", "bb", "ccc", "d"])
    generic_array = array.FSharpArray("Generic", [1, "a", True, 2.0, sbyte(5)])
    empty_array = array.FSharpArray("Generic", [])

    # Test filtering Int32 (keep even numbers)
    filtered_int32 = int32_array.filter(lambda x: x % 2 == 0)
    assert len(filtered_int32) == 2
    assert filtered_int32[0] == int32(2)
    assert filtered_int32[1] == int32(4)

    # Test filtering Float32 (keep positive values)
    filtered_float32 = float32_array.filter(lambda x: x > 0)
    assert len(filtered_float32) == 2
    assert filtered_float32[0] == pytest.approx(1.1)
    assert filtered_float32[1] == pytest.approx(3.3)

    # Test filtering Float64 (keep non-zero values)
    filtered_float64 = float64_array.filter(lambda x: x != 0.0)
    assert len(filtered_float64) == 3
    assert filtered_float64[0] == pytest.approx(1.1)
    assert filtered_float64[1] == pytest.approx(-2.2)
    assert filtered_float64[2] == pytest.approx(3.3)

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


def test_fsharp_array_filter_error_handling():
    """Test error handling within the filter function's predicate."""
    string_array = array.FSharpArray("String", ["a", "b", "c"])

    def faulty_predicate(x):
        if x == "b":
            raise ValueError("Error during filtering")
        return len(x) == 1

    with pytest.raises(ValueError, match="Error during filtering"):
        # The error should propagate out of the filter call
        _ = string_array.filter(faulty_predicate)


@pytest.mark.parametrize("array_type", ["Int8", "Int32", "Float64", "String", "Generic"])
def test_fsharp_array_append(array_type: str) -> None:
    """Test appending two FSharpArrays of the same type."""
    # Create arrays of the same type
    if array_type == "Int8":
        elements1 = [sbyte(1), sbyte(2), sbyte(3)]
        elements2 = [sbyte(4), sbyte(5)]
    elif array_type == "Int32":
        elements1 = [int32(1), int32(2), int32(3)]
        elements2 = [int32(4), int32(5)]
    elif array_type == "Float64":
        elements1 = [float64(1.1), float64(2.2)]
        elements2 = [float64(3.3), float64(4.4), float64(5.5)]
    elif array_type == "String":
        elements1 = ["a", "b", "c"]
        elements2 = ["d", "e"]
    else:  # Generic
        elements1 = [1, "a", True]
        elements2 = [False, 2]

    # Create arrays with the given elements and types
    array1 = array.FSharpArray(array_type, elements1)
    array2 = array.FSharpArray(array_type, elements2)

    # Append arrays

    result = Array.append(array1, array2)

    # Check length
    assert len(result) == len(elements1) + len(elements2)

    # Check each element
    for i, element in enumerate(elements1):
        if array_type in ["Float32", "Float64"]:
            assert result[i] == pytest.approx(element)
        else:
            assert result[i] == element

    for i, element in enumerate(elements2):
        if array_type in ["Float32", "Float64"]:
            assert result[i + len(elements1)] == pytest.approx(element)
        else:
            assert result[i + len(elements1)] == element
