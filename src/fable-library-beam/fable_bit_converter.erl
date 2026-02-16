-module(fable_bit_converter).
-export([get_bytes/2, get_bytes_bool/1, to_int/3, to_uint/3, to_float/3,
         to_boolean/2, to_string/1, to_string/2, to_string/3]).

%% Convert a numeric value to a byte array (little-endian, matching .NET convention).
get_bytes(Value, BitSize) when is_integer(Value) ->
    fable_utils:new_byte_array(binary_to_list(<<Value:BitSize/little-signed-integer>>));
get_bytes(Value, 32) when is_float(Value) ->
    fable_utils:new_byte_array(binary_to_list(<<Value:32/little-float>>));
get_bytes(Value, 64) when is_float(Value) ->
    fable_utils:new_byte_array(binary_to_list(<<Value:64/little-float>>)).

%% GetBytes for Boolean: true → [1], false → [0]
get_bytes_bool(true) -> fable_utils:new_byte_array([1]);
get_bytes_bool(false) -> fable_utils:new_byte_array([0]).

%% ToBoolean: check if byte at index is non-zero
to_boolean({byte_array, _, _} = BA, StartIndex) ->
    to_boolean(list_to_binary(fable_utils:byte_array_to_list(BA)), StartIndex);
to_boolean(Bytes, StartIndex) when is_binary(Bytes) ->
    binary:at(Bytes, StartIndex) =/= 0;
to_boolean(Bytes, StartIndex) when is_list(Bytes) ->
    to_boolean(list_to_binary(Bytes), StartIndex).

%% ToString: format byte array as uppercase hex with dashes
to_string({byte_array, _, _} = BA) ->
    to_string(fable_utils:byte_array_to_list(BA));
to_string(Bytes) when is_list(Bytes) ->
    HexParts = [io_lib:format("~2.16.0B", [B]) || B <- Bytes],
    iolist_to_binary(lists:join($-, HexParts));
to_string(Bytes) when is_binary(Bytes) ->
    to_string(binary_to_list(Bytes)).

to_string({byte_array, _, _} = BA, StartIndex) ->
    to_string(fable_utils:byte_array_to_list(BA), StartIndex);
to_string(Bytes, StartIndex) when is_list(Bytes) ->
    to_string(lists:nthtail(StartIndex, Bytes));
to_string(Bytes, StartIndex) when is_binary(Bytes) ->
    to_string(binary_to_list(Bytes), StartIndex).

to_string({byte_array, _, _} = BA, StartIndex, Count) ->
    to_string(fable_utils:byte_array_to_list(BA), StartIndex, Count);
to_string(Bytes, StartIndex, Count) when is_list(Bytes) ->
    to_string(lists:sublist(Bytes, StartIndex + 1, Count));
to_string(Bytes, StartIndex, Count) when is_binary(Bytes) ->
    to_string(binary_to_list(Bytes), StartIndex, Count).

%% Convert bytes (binary) starting at StartIndex to a signed integer of BitSize bits.
to_int({byte_array, _, _} = BA, StartIndex, BitSize) ->
    to_int(list_to_binary(fable_utils:byte_array_to_list(BA)), StartIndex, BitSize);
to_int(Bytes, StartIndex, BitSize) when is_binary(Bytes) ->
    <<_:StartIndex/binary, Value:BitSize/little-signed-integer, _/binary>> = Bytes,
    Value;
to_int(Bytes, StartIndex, BitSize) when is_list(Bytes) ->
    to_int(list_to_binary(Bytes), StartIndex, BitSize).

%% Convert bytes (binary) starting at StartIndex to an unsigned integer of BitSize bits.
to_uint({byte_array, _, _} = BA, StartIndex, BitSize) ->
    to_uint(list_to_binary(fable_utils:byte_array_to_list(BA)), StartIndex, BitSize);
to_uint(Bytes, StartIndex, BitSize) when is_binary(Bytes) ->
    <<_:StartIndex/binary, Value:BitSize/little-unsigned-integer, _/binary>> = Bytes,
    Value;
to_uint(Bytes, StartIndex, BitSize) when is_list(Bytes) ->
    to_uint(list_to_binary(Bytes), StartIndex, BitSize).

%% Convert bytes (binary) starting at StartIndex to a float of BitSize bits.
to_float({byte_array, _, _} = BA, StartIndex, BitSize) ->
    to_float(list_to_binary(fable_utils:byte_array_to_list(BA)), StartIndex, BitSize);
to_float(Bytes, StartIndex, BitSize) when is_binary(Bytes) ->
    <<_:StartIndex/binary, Value:BitSize/little-float, _/binary>> = Bytes,
    Value;
to_float(Bytes, StartIndex, BitSize) when is_list(Bytes) ->
    to_float(list_to_binary(Bytes), StartIndex, BitSize).
