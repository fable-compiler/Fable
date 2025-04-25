import pytest
from fable_library.core import array, byte, int16, int32, int64, sbyte, uint16, uint32, uint64


def test_fsharp_array_create_empty():
    """Test creating empty FSharpArray instances with different types."""
    # Create empty arrays with different types
    empty_int8 = array.FSharpArray(array_type="Int8")
    empty_uint8 = array.FSharpArray(array_type="UInt8")
    empty_int16 = array.FSharpArray(array_type="Int16")
    empty_uint16 = array.FSharpArray(array_type="UInt16")
    empty_int32 = array.FSharpArray(array_type="Int32")
    empty_uint32 = array.FSharpArray(array_type="UInt32")
    empty_int64 = array.FSharpArray(array_type="Int64")
    empty_uint64 = array.FSharpArray(array_type="UInt64")
    empty_string = array.FSharpArray(array_type="String")
    empty_generic = array.FSharpArray(array_type="Generic")

    # Check lengths
    assert len(empty_int8) == 0
    assert len(empty_uint8) == 0
    assert len(empty_int16) == 0
    assert len(empty_uint16) == 0
    assert len(empty_int32) == 0
    assert len(empty_uint32) == 0
    assert len(empty_int64) == 0
    assert len(empty_uint64) == 0
    assert len(empty_string) == 0
    assert len(empty_generic) == 0


def test_fsharp_array_create_with_elements():
    """Test creating FSharpArray instances with elements."""
    # Create arrays with elements
    int8_array = array.FSharpArray([sbyte(1), sbyte(2), sbyte(3)], array_type="Int8")
    uint8_array = array.FSharpArray([byte(1), byte(2), byte(3)], array_type="UInt8")
    int16_array = array.FSharpArray([int16(1), int16(2), int16(3)], array_type="Int16")
    uint16_array = array.FSharpArray([uint16(1), uint16(2), uint16(3)], array_type="UInt16")
    int32_array = array.FSharpArray([int32(1), int32(2), int32(3)], array_type="Int32")
    uint32_array = array.FSharpArray([uint32(1), uint32(2), uint32(3)], array_type="UInt32")
    int64_array = array.FSharpArray([int64(1), int64(2), int64(3)], array_type="Int64")
    uint64_array = array.FSharpArray([uint64(1), uint64(2), uint64(3)], array_type="UInt64")
    string_array = array.FSharpArray(["a", "b", "c"], array_type="String")
    generic_array = array.FSharpArray([1, "a", True], array_type="Generic")

    # Check lengths
    assert len(int8_array) == 3
    assert len(uint8_array) == 3
    assert len(int16_array) == 3
    assert len(int32_array) == 3
    assert len(uint16_array) == 3
    assert len(uint32_array) == 3
    assert len(int64_array) == 3
    assert len(uint64_array) == 3
    assert len(string_array) == 3
    assert len(generic_array) == 3


def test_fsharp_array_invalid_type():
    """Test creating FSharpArray/FSharpCons with an invalid array_type string defaults to Generic."""
    # FSharpArray initialization should default to Generic without raising an error
    try:
        # Simply creating the array should succeed.
        _ = array.FSharpArray([1, "hello"], array_type="InvalidTypeString")
        # The runtime behavior is Generic, even if static analysis might be strict.
        # We rely on the FSharpCons test below to verify the generic nature more explicitly.
    except Exception as e:
        pytest.fail(f"Creating FSharpArray with invalid type raised unexpected error: {e}")

    # FSharpCons should default to Generic without raising an error
    try:
        cons = array.FSharpCons("AnotherInvalidType")
        # The array_type attribute should be accessible and reflect the default
        assert cons.array_type == "Generic"
        # Test allocation using the defaulted generic constructor
        allocated_arr = cons.allocate(3)
        assert len(allocated_arr) == 3
        assert allocated_arr[0] is None  # Default for generic allocation
        allocated_arr[0] = "works"  # Should allow setting different types
        assert allocated_arr[0] == "works"
    except Exception as e:
        pytest.fail(f"Creating/using FSharpCons with invalid type raised unexpected error: {e}")


def test_fsharp_array_getitem():
    """Test getting items from FSharpArray."""
    # Create arrays with elements
    int8_array = array.FSharpArray([sbyte(1), sbyte(2), sbyte(3)], array_type="Int8")
    uint8_array = array.FSharpArray([byte(1), byte(2), byte(3)], array_type="UInt8")
    int16_array = array.FSharpArray([int16(1), int16(2), int16(3)], array_type="Int16")
    uint16_array = array.FSharpArray([uint16(1), uint16(2), uint16(3)], array_type="UInt16")
    int32_array = array.FSharpArray([int32(1), int32(2), int32(3)], array_type="Int32")
    uint32_array = array.FSharpArray([uint32(1), uint32(2), uint32(3)], array_type="UInt32")
    int64_array = array.FSharpArray([int64(1), int64(2), int64(3)], array_type="Int64")
    uint64_array = array.FSharpArray([uint64(1), uint64(2), uint64(3)], array_type="UInt64")
    string_array = array.FSharpArray(["a", "b", "c"], array_type="String")
    generic_array = array.FSharpArray([1, "a", True], array_type="Generic")

    # Test getting items
    assert int8_array[0] == sbyte(1)
    assert int8_array[1] == sbyte(2)
    assert int8_array[2] == sbyte(3)

    assert uint8_array[0] == byte(1)
    assert uint8_array[1] == byte(2)
    assert uint8_array[2] == byte(3)

    assert int16_array[0] == int16(1)
    assert int16_array[1] == int16(2)
    assert int16_array[2] == int16(3)

    assert uint16_array[0] == uint16(1)
    assert uint16_array[1] == uint16(2)
    assert uint16_array[2] == uint16(3)

    assert int32_array[0] == int32(1)
    assert int32_array[1] == int32(2)
    assert int32_array[2] == int32(3)

    assert uint32_array[0] == uint32(1)
    assert uint32_array[1] == uint32(2)
    assert uint32_array[2] == uint32(3)

    assert int64_array[0] == int64(1)
    assert int64_array[1] == int64(2)
    assert int64_array[2] == int64(3)

    assert uint64_array[0] == uint64(1)
    assert uint64_array[1] == uint64(2)
    assert uint64_array[2] == uint64(3)

    assert string_array[0] == "a"
    assert string_array[1] == "b"
    assert string_array[2] == "c"

    assert generic_array[0] == 1
    assert generic_array[1] == "a"
    assert generic_array[2] is True

    # Test negative indices
    assert int8_array[-1] == sbyte(3)
    assert int8_array[-2] == sbyte(2)
    assert int8_array[-3] == sbyte(1)


def test_fsharp_array_getitem_out_of_range():
    """Test getting items with out-of-range indices."""
    arr = array.FSharpArray([1, 2, 3], array_type="Generic")

    # Test out-of-range indices
    with pytest.raises(IndexError):
        _ = arr[3]

    with pytest.raises(IndexError):
        _ = arr[-4]


def test_fsharp_array_setitem():
    """Test setting items in FSharpArray."""
    # Create arrays with elements
    int8_array = array.FSharpArray([sbyte(1), sbyte(2), sbyte(3)], array_type="Int8")
    uint8_array = array.FSharpArray([byte(1), byte(2), byte(3)], array_type="UInt8")
    int16_array = array.FSharpArray([int16(1), int16(2), int16(3)], array_type="Int16")
    uint16_array = array.FSharpArray([uint16(1), uint16(2), uint16(3)], array_type="UInt16")
    int32_array = array.FSharpArray([int32(1), int32(2), int32(3)], array_type="Int32")
    uint32_array = array.FSharpArray([uint32(1), uint32(2), uint32(3)], array_type="UInt32")
    int64_array = array.FSharpArray([int64(1), int64(2), int64(3)], array_type="Int64")
    uint64_array = array.FSharpArray([uint64(1), uint64(2), uint64(3)], array_type="UInt64")
    string_array = array.FSharpArray(["a", "b", "c"], array_type="String")
    generic_array = array.FSharpArray([1, "a", True], array_type="Generic")

    # Test setting items
    int8_array[1] = sbyte(42)
    assert int8_array[1] == sbyte(42)

    uint8_array[1] = byte(42)
    assert uint8_array[1] == byte(42)

    int16_array[1] = int16(42)
    assert int16_array[1] == int16(42)

    uint16_array[1] = uint16(42)
    assert uint16_array[1] == uint16(42)

    int32_array[1] = int32(42)
    assert int32_array[1] == int32(42)

    uint32_array[1] = uint32(42)
    assert uint32_array[1] == uint32(42)

    int64_array[1] = int64(42)
    assert int64_array[1] == int64(42)

    uint64_array[1] = uint64(42)
    assert uint64_array[1] == uint64(42)

    string_array[1] = "x"
    assert string_array[1] == "x"

    generic_array[1] = 42
    assert generic_array[1] == 42


def test_fsharp_array_setitem_out_of_range():
    """Test setting items with out-of-range indices."""
    arr = array.FSharpArray([1, 2, 3], array_type="Generic")

    # Test out-of-range indices
    with pytest.raises(IndexError):
        arr[3] = 4

    with pytest.raises(IndexError):
        arr[-4] = 0


def test_fsharp_array_mixed_types():
    """Test FSharpArray with mixed data types (Generic)."""
    mixed_list = [1, "hello", 3.14, True, None, sbyte(5)]
    arr = array.FSharpArray(mixed_list, array_type="Generic")

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


def test_fsharp_array_map():
    """Test the map function of FSharpArray."""
    # Create arrays with elements
    int8_array = array.FSharpArray([sbyte(1), sbyte(2), sbyte(3)], array_type="Int8")
    uint8_array = array.FSharpArray([byte(1), byte(2), byte(3)], array_type="UInt8")
    int32_array = array.FSharpArray([int32(1), int32(2), int32(3)], array_type="Int32")
    string_array = array.FSharpArray(["a", "b", "c"], array_type="String")

    # Test mapping with same type
    mapped_int8 = int8_array.map(lambda x: sbyte(x * 2))
    assert len(mapped_int8) == 3
    assert mapped_int8[0] == sbyte(2)
    assert mapped_int8[1] == sbyte(4)
    assert mapped_int8[2] == sbyte(6)

    mapped_uint8 = uint8_array.map(lambda x: byte(x * 2))
    assert len(mapped_uint8) == 3
    assert mapped_uint8[0] == byte(2)
    assert mapped_uint8[1] == byte(4)
    assert mapped_uint8[2] == byte(6)

    mapped_int32 = int32_array.map(lambda x: int32(x * 2))
    assert len(mapped_int32) == 3
    assert mapped_int32[0] == int32(2)
    assert mapped_int32[1] == int32(4)
    assert mapped_int32[2] == int32(6)

    mapped_string = string_array.map(lambda x: x + x)
    assert len(mapped_string) == 3
    assert mapped_string[0] == "aa"
    assert mapped_string[1] == "bb"
    assert mapped_string[2] == "cc"

    # Test mapping to different type
    mapped_to_string = int8_array.map(lambda x: str(x))
    assert len(mapped_to_string) == 3
    assert mapped_to_string[0] == "1"
    assert mapped_to_string[1] == "2"
    assert mapped_to_string[2] == "3"

    # Test mapping a generic array without cons (should result in a generic array)
    generic_array = array.FSharpArray([1, "a", True], array_type="Generic")
    mapped_generic = generic_array.map(lambda x: f"item: {x}")
    assert len(mapped_generic) == 3
    assert mapped_generic[0] == "item: 1"
    assert mapped_generic[1] == "item: a"
    assert mapped_generic[2] == "item: True"
    # Note: Even without 'cons', the type checker might infer the type.
    # The runtime behavior should result in a generic array capable of holding the mapped type.

    # Test mapping to mixed types without cons
    int_array_for_mixed_map = array.FSharpArray([int32(1), int32(2), int32(3)], array_type="Int32")
    mapped_mixed = int_array_for_mixed_map.map(lambda x: f"Even: {x}" if x % 2 == 0 else False)
    assert len(mapped_mixed) == 3
    assert mapped_mixed[0] is False  # 1 is odd
    assert mapped_mixed[1] == "Even: 2"  # 2 is even
    assert mapped_mixed[2] is False  # 3 is odd


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
    string_cons = array.FSharpCons("String")
    generic_cons = array.FSharpCons("Generic")

    # Test allocate method
    int8_array = int8_cons.allocate(3)
    assert len(int8_array) == 3
    assert int8_array[0] == sbyte(0)
    assert int8_array[1] == sbyte(0)
    assert int8_array[2] == sbyte(0)

    uint8_array = uint8_cons.allocate(3)
    assert len(uint8_array) == 3
    assert uint8_array[0] == byte(0)
    assert uint8_array[1] == byte(0)
    assert uint8_array[2] == byte(0)

    string_array = string_cons.allocate(3)
    assert len(string_array) == 3
    assert string_array[0] == ""
    assert string_array[1] == ""
    assert string_array[2] == ""

    int16_array_alloc = int16_cons.allocate(2)
    assert len(int16_array_alloc) == 2
    assert int16_array_alloc[0] == int16(0)
    assert int16_array_alloc[1] == int16(0)

    generic_array_alloc = generic_cons.allocate(4)
    assert len(generic_array_alloc) == 4
    assert generic_array_alloc[0] is None  # Default for generic is None
    assert generic_array_alloc[1] is None
    assert generic_array_alloc[2] is None
    assert generic_array_alloc[3] is None

    uint16_array_alloc = uint16_cons.allocate(1)
    assert len(uint16_array_alloc) == 1
    assert uint16_array_alloc[0] == uint16(0)

    # Test __call__ method
    int32_array_call = int32_cons(3)
    assert len(int32_array_call) == 3
    assert int32_array_call[0] == int32(0)
    assert int32_array_call[1] == int32(0)
    assert int32_array_call[2] == int32(0)

    int64_array_call = int64_cons(2)
    assert len(int64_array_call) == 2
    assert int64_array_call[0] == int64(0)
    assert int64_array_call[1] == int64(0)

    uint32_array_call = uint32_cons(4)
    assert len(uint32_array_call) == 4
    assert uint32_array_call[0] == uint32(0)
    assert uint32_array_call[3] == uint32(0)

    uint64_array_call = uint64_cons(2)
    assert len(uint64_array_call) == 2
    assert uint64_array_call[0] == uint64(0)
    assert uint64_array_call[1] == uint64(0)

    generic_array_call = generic_cons(1)
    assert len(generic_array_call) == 1
    assert generic_array_call[0] is None


def test_allocate_array_from_cons():
    """Test the allocate_array_from_cons function."""
    # Create constructors for different types
    int8_cons = array.FSharpCons("Int8")
    uint8_cons = array.FSharpCons("UInt8")

    # Test with constructor
    int8_array = array.allocate_array_from_cons(int8_cons, 3)
    assert len(int8_array) == 3

    uint8_array = array.allocate_array_from_cons(uint8_cons, 3)
    assert len(uint8_array) == 3

    # Test without constructor
    generic_array = array.allocate_array_from_cons(None, 3)
    assert len(generic_array) == 3


def test_fsharp_array_map_with_cons():
    """Test the map function with a constructor."""
    # Create arrays with elements
    int8_array = array.FSharpArray([sbyte(1), sbyte(2), sbyte(3)], array_type="Int8")

    # Create constructors
    int32_cons = array.FSharpCons("Int32")

    # Test mapping with constructor
    mapped_array = int8_array.map(lambda x: int32(x * 2), int32_cons)
    assert len(mapped_array) == 3
    assert mapped_array[0] == int32(2)
    assert mapped_array[1] == int32(4)
    assert mapped_array[2] == int32(6)


def test_fsharp_array_map_error_handling():
    """Test error handling within the map function's callback."""
    int_array = array.FSharpArray([int32(1), int32(2), int32(3)], array_type="Int32")

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
    int32_array = array.FSharpArray([int32(1), int32(2), int32(3), int32(4)], array_type="Int32")
    string_array = array.FSharpArray(["a", "bb", "ccc", "d"], array_type="String")
    generic_array = array.FSharpArray([1, "a", True, 2.0, sbyte(5)], array_type="Generic")
    empty_array = array.FSharpArray([], array_type="Generic")

    # Test filtering Int32 (keep even numbers)
    filtered_int32 = int32_array.filter(lambda x: x % 2 == 0)
    assert len(filtered_int32) == 2
    assert filtered_int32[0] == int32(2)
    assert filtered_int32[1] == int32(4)

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
    string_array = array.FSharpArray(["a", "b", "c"], array_type="String")

    def faulty_predicate(x):
        if x == "b":
            raise ValueError("Error during filtering")
        return len(x) == 1

    with pytest.raises(ValueError, match="Error during filtering"):
        # The error should propagate out of the filter call
        _ = string_array.filter(faulty_predicate)
