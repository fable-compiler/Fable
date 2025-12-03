from array import array
from decimal import Decimal

from fable_library.core import byte, int16, int32, int64, sbyte, uint16, uint32, uint64
from pydantic import BaseModel


def test_byte_create() -> None:
    assert byte(0) == 0
    assert byte(byte(42)) == 42
    assert byte(uint16(42)) == 42
    assert byte(1.0) == 1
    assert byte(byte(42)) == 42
    assert byte(byte(-1)) == 255


def test_sbyte_create() -> None:
    assert sbyte(0) == 0
    assert sbyte(sbyte(42)) == 42
    assert sbyte(42) == 42
    assert sbyte(-42) == -42
    assert sbyte(sbyte(1)) == 1
    assert sbyte(sbyte(-1)) == -1


def test_uint16_create() -> None:
    assert uint16(0) == 0
    assert uint16(uint16(42)) == 42
    assert uint16(42) == 42
    assert uint16(65535) == 65535
    assert uint16(65536) == 0
    assert uint16(-1) == 65535
    assert uint16(uint32(1)) == 1
    assert uint16(uint32(-1)) == 65535


def test_int16_create() -> None:
    assert int16(0) == 0
    assert int16(int16(42)) == 42
    assert int16(42) == 42
    assert int16(32767) == 32767
    assert int16(32768) == -32768
    assert int16(-1) == -1
    assert int16(int32(1)) == 1
    assert int16(int32(-1)) == -1


def test_uint64_create() -> None:
    assert uint64(15210016002388773605) == 15210016002388773605
    assert uint64(0) == 0
    assert uint64(42) == 42
    assert uint64(uint64(42)) == 42
    assert uint64(uint64(1)) == 1
    assert uint64(uint32(-1)) == 4294967295


def test_byte_add() -> None:
    assert byte(42) + byte(42) == 84
    assert byte(255) + byte(1) == 0
    assert byte(255) + 1 == 0
    assert byte(42) + 42 == 84
    # assert byte(1) + 1.1 == 2.1
    assert 42 + byte(42) == 84  # Uses the __radd__ method
    assert Decimal(42) + byte(42) == 84  # Uses the __radd__ method
    assert 1.1 + byte(1) == 2.1  # Uses the __radd__ method


def test_byte_coerce():
    assert int(byte(42)) == 42  # Uses the __int__ method
    assert array("B", [byte(42), byte(42)]) == array("B", [42, 42])  # type: ignore Uses the __index__ method
    # Can be used as slice indices
    assert [1, 2, 3][byte(1) : byte(2)] == [2]


def test_byte_rich_compare():
    assert byte(42) == 42
    assert byte(42) != 43
    assert byte(42) < 43
    assert byte(42) <= 43
    assert byte(42) <= 42
    assert byte(42) > 41
    assert byte(42) >= 41
    assert byte(42) >= 42
    assert 42 == byte(42)
    assert 43 != byte(42)
    assert 43 > byte(42)
    assert 43 >= byte(42)
    assert 42 < byte(43)
    assert 42 <= byte(43)
    assert 41 < byte(42)
    assert 41 <= byte(42)
    assert 42 >= byte(42)
    assert byte(42) == byte(42)
    assert byte(42) != byte(43)
    assert byte(42) < byte(43)
    assert byte(42) <= byte(43)
    assert byte(42) <= byte(42)
    assert byte(42) > byte(41)
    assert byte(42) >= byte(41)
    assert byte(42) >= byte(42)
    assert not (byte(42) == 43)


def test_uin8_rsift():
    assert byte(2) >> 1 == 1
    assert byte(1) >> 1 == 0
    assert byte(1) >> 8 == 1
    assert byte(1) >> 9 == 0
    assert 2 >> byte(1) == 1
    assert 1 >> byte(1) == 0
    assert 1 >> byte(8) == 0
    assert 1 >> byte(9) == 0


def test_byte_lshift():
    assert byte(1) << 1 == 2
    assert byte(1) << 8 == 1
    assert byte(1) << 9 == 2
    assert 1 << byte(1) == 2
    assert 1 << byte(8) == 256
    assert 1 << byte(9) == 512


def test_binary_complement():
    assert ~byte(0) == 255
    assert ~byte(1) == 254
    assert ~byte(255) == 0
    assert ~byte(254) == 1
    assert ~byte(42) == 213
    assert ~byte(213) == 42


def test_format():
    assert f"{byte(42)}" == "42"
    assert f"{byte(42):02X}" == "2A"
    assert f"{byte(42):02x}" == "2a"
    assert f"{byte(42):08b}" == "00101010"
    assert f"{byte(42):08}" == "00000042"
    assert f"{byte(42):08d}" == "00000042"
    assert f"{byte(42):08o}" == "00000052"
    assert f"{uint16(42):08d}" == "00000042"
    assert f"{int16(42):08d}" == "00000042"
    assert f"{sbyte(42):08d}" == "00000042"
    assert f"{byte(42):08d}" == "00000042"


def test_abs():
    assert abs(byte(42)) == 42
    assert abs(byte(0)) == 0
    assert abs(sbyte(-42)) == 42
    assert abs(sbyte(-1)) == 1
    assert abs(int16(-42)) == 42
    assert abs(int16(-1)) == 1
    assert abs(int32(-42)) == 42
    assert abs(int32(-1)) == 1
    assert abs(int64(-42)) == 42
    assert abs(int64(-1)) == 1
    assert abs(uint16(42)) == 42
    assert abs(uint16(0)) == 0
    assert abs(uint32(42)) == 42
    assert abs(uint32(0)) == 0
    assert abs(uint64(42)) == 42


def test_floor_div():
    assert 10 // byte(3) == 3


def test_divide():
    assert 10 / byte(3) == 3.3333333333333335
    assert 10 / sbyte(3) == 3.3333333333333335
    assert 10 / int16(3) == 3.3333333333333335
    assert 10 / int32(3) == 3.3333333333333335
    assert 10 / int64(3) == 3.3333333333333335
    assert 10 / uint16(3) == 3.3333333333333335
    assert 10 / uint32(3) == 3.3333333333333335
    assert 10 / uint64(3) == 3.3333333333333335
    assert byte(10) / 3 == 3
    assert byte(10) / 3.0 == 3.3333333333333335


def test_hash():
    assert hash(byte(42)) != 0
    assert hash(sbyte(42)) != 0
    assert hash(int16(42)) != 0
    assert hash(int32(42)) != 0
    assert hash(int64(42)) != 0
    assert hash(uint16(42)) != 0
    assert hash(uint32(42)) != 0
    assert hash(uint64(42)) != 0


def test_to_string():
    assert byte(7).to_string(radix=2) == "111"
    assert byte(7).to_string(radix=8) == "7"
    assert byte(7).to_string(radix=10) == "7"
    assert byte(7).to_string(radix=16) == "7"
    assert byte(10).to_string(radix=2) == "1010"
    assert byte(10).to_string(radix=8) == "12"
    assert byte(10).to_string(radix=10) == "10"
    assert byte(10).to_string(radix=16) == "a"
    assert byte(255).to_string(radix=2) == "11111111"


def test_addition():
    assert byte(1) + byte(1) == 2
    assert byte(1) + 1 == 2
    assert 1 + byte(1) == 2
    assert byte(1) + sbyte(1) == 2
    assert sbyte(1) + byte(1) == 2
    assert byte(1) + int16(1) == 2
    assert int16(1) + byte(1) == 2
    assert byte(1) + int32(1) == 2
    assert int32(1) + byte(1) == 2
    assert byte(1) + int64(1) == 2
    assert int64(1) + byte(1) == 2


def test_addition_overflow():
    assert byte(255) + byte(1) == 0
    assert byte(255) + 1 == 0
    assert 1 + byte(255) == 256  # Python ints are unbounded
    assert byte(255) + sbyte(1) == 0
    assert sbyte(1) + byte(127) == -128
    assert byte(255) + int16(1) == 0
    assert int16(1) + 32767 == -32768
    assert byte(255) + int32(1) == 0
    assert int32(1) + 2147483647 == -2147483648
    assert byte(255) + int64(1) == 0
    assert int64(1) + 9223372036854775807 == -9223372036854775808


def test_float_to_int_conversion():
    """Test the optimized conversion path from Float32/Float64 to integer types."""
    from fable_library.core import float32, float64

    # Test Float32 to various integer types
    assert byte(float32(42.7)) == 42
    assert sbyte(float32(42.7)) == 42
    assert sbyte(float32(-42.7)) == -42
    assert int16(float32(42.7)) == 42
    assert int16(float32(-42.7)) == -42
    assert uint16(float32(42.7)) == 42
    assert int32(float32(42.7)) == 42
    assert int32(float32(-42.7)) == -42
    assert uint32(float32(42.7)) == 42
    assert int64(float32(42.7)) == 42
    assert int64(float32(-42.7)) == -42
    assert uint64(float32(42.7)) == 42

    # Test Float64 to various integer types
    assert byte(float64(42.7)) == 42
    assert sbyte(float64(42.7)) == 42
    assert sbyte(float64(-42.7)) == -42
    assert int16(float64(42.7)) == 42
    assert int16(float64(-42.7)) == -42
    assert uint16(float64(42.7)) == 42
    assert int32(float64(42.7)) == 42
    assert int32(float64(-42.7)) == -42
    assert uint32(float64(42.7)) == 42
    assert int64(float64(42.7)) == 42
    assert int64(float64(-42.7)) == -42
    assert uint64(float64(42.7)) == 42


def test_pydantic_int_model_definition():
    """Test that Fable integer types can be used in Pydantic models."""

    class User(BaseModel):
        age: int32
        level: byte

    user = User(age=int32(25), level=byte(3))
    assert user.age == 25
    assert user.level == 3
    assert type(user.age).__name__ == "Int32"
    assert type(user.level).__name__ == "UInt8"


def test_pydantic_int_serialization():
    """Test JSON serialization of Pydantic models with Fable integer types."""

    class User(BaseModel):
        age: int32
        level: byte

    user = User(age=int32(25), level=byte(3))
    assert user.model_dump_json() == '{"age":25,"level":3}'
    assert user.model_dump() == {"age": 25, "level": 3}


def test_pydantic_int_json_schema():
    """Test JSON Schema generation for Pydantic models with Fable integer types."""

    class User(BaseModel):
        age: int32

    schema = User.model_json_schema()
    assert schema["properties"]["age"]["type"] == "integer"


def test_pydantic_all_int_types():
    """Test all Fable integer types in Pydantic models."""

    class AllInts(BaseModel):
        a: sbyte
        b: byte
        c: int16
        d: uint16
        e: int32
        f: uint32
        g: int64
        h: uint64

    model = AllInts(
        a=sbyte(-128),
        b=byte(255),
        c=int16(-32768),
        d=uint16(65535),
        e=int32(-2147483648),
        f=uint32(4294967295),
        g=int64(-1000000),
        h=uint64(1000000),
    )

    # Verify types are preserved
    assert type(model.a).__name__ == "Int8"
    assert type(model.b).__name__ == "UInt8"
    assert type(model.c).__name__ == "Int16"
    assert type(model.d).__name__ == "UInt16"
    assert type(model.e).__name__ == "Int32"
    assert type(model.f).__name__ == "UInt32"
    assert type(model.g).__name__ == "Int64"
    assert type(model.h).__name__ == "UInt64"

    # Verify serialization
    json_str = model.model_dump_json()
    assert "-128" in json_str
    assert "255" in json_str
    assert "1000000" in json_str


def test_pydantic_int_from_fable_type():
    """Test that Pydantic models accept Fable types directly as input."""

    class Simple(BaseModel):
        val: int32

    # Pass a Fable Int32 directly
    simple = Simple(val=int32(42))
    assert simple.val == 42
    assert type(simple.val).__name__ == "Int32"


def test_pydantic_nested_int_models():
    """Test nested Pydantic models with Fable integer types."""

    class Address(BaseModel):
        zip_code: int32

    class Person(BaseModel):
        age: int32
        address: Address

    person = Person(age=int32(30), address=Address(zip_code=int32(12345)))
    assert person.age == 30
    assert person.address.zip_code == 12345
    assert person.model_dump_json() == '{"age":30,"address":{"zip_code":12345}}'


def test_pydantic_int_json_deserialization():
    """Test JSON deserialization into Pydantic models with Fable integer types."""

    class User(BaseModel):
        name: str
        age: int32

    json_str = '{"name": "Alice", "age": 30}'
    user = User.model_validate_json(json_str)
    assert user.name == "Alice"
    assert user.age == 30
    assert type(user.age).__name__ == "Int32"


def test_pydantic_int_round_trip():
    """Test round-trip serialization/deserialization preserves Fable types."""

    class Data(BaseModel):
        value: int64

    original = Data(value=int64(123456789))
    json_str = original.model_dump_json()
    restored = Data.model_validate_json(json_str)
    assert restored.value == 123456789
    assert type(restored.value).__name__ == "Int64"


def test_pydantic_large_int64_values():
    """Test that large int64/uint64 values work correctly with Pydantic.

    This specifically tests the fix for the issue where large int64 values
    would fail with 'Unable to parse input string as an integer, exceeded
    maximum size' due to Pydantic's int_schema not recognizing custom types.
    """

    class Int64Data(BaseModel):
        value: int64

    class UInt64Data(BaseModel):
        value: uint64

    # Test int64 max value
    max_int64 = int64(9223372036854775807)
    model = Int64Data(value=max_int64)
    assert model.value == 9223372036854775807
    assert type(model.value).__name__ == "Int64"

    # Test int64 min value
    min_int64 = int64(-9223372036854775808)
    model = Int64Data(value=min_int64)
    assert model.value == -9223372036854775808
    assert type(model.value).__name__ == "Int64"

    # Test uint64 max value
    max_uint64 = uint64(18446744073709551615)
    model = UInt64Data(value=max_uint64)
    assert model.value == 18446744073709551615
    assert type(model.value).__name__ == "UInt64"

    # Test serialization of large values
    model = Int64Data(value=int64(9223372036854775807))
    json_str = model.model_dump_json()
    assert "9223372036854775807" in json_str

    # Test round-trip with large values
    restored = Int64Data.model_validate_json(json_str)
    assert restored.value == 9223372036854775807
    assert type(restored.value).__name__ == "Int64"
