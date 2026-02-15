-module(fable_bit_converter).
-export([get_bytes/2, to_int/3, to_uint/3, to_float/3]).

%% Convert a numeric value to a binary of bytes (little-endian, matching .NET convention).
get_bytes(Value, BitSize) when is_integer(Value) ->
    <<Value:BitSize/little-signed-integer>>;
get_bytes(Value, 32) when is_float(Value) ->
    <<Value:32/little-float>>;
get_bytes(Value, 64) when is_float(Value) ->
    <<Value:64/little-float>>.

%% Convert bytes (binary) starting at StartIndex to a signed integer of BitSize bits.
to_int(Bytes, StartIndex, BitSize) when is_binary(Bytes) ->
    <<_:StartIndex/binary, Value:BitSize/little-signed-integer, _/binary>> = Bytes,
    Value;
to_int(Bytes, StartIndex, BitSize) when is_list(Bytes) ->
    to_int(list_to_binary(Bytes), StartIndex, BitSize).

%% Convert bytes (binary) starting at StartIndex to an unsigned integer of BitSize bits.
to_uint(Bytes, StartIndex, BitSize) when is_binary(Bytes) ->
    <<_:StartIndex/binary, Value:BitSize/little-unsigned-integer, _/binary>> = Bytes,
    Value;
to_uint(Bytes, StartIndex, BitSize) when is_list(Bytes) ->
    to_uint(list_to_binary(Bytes), StartIndex, BitSize).

%% Convert bytes (binary) starting at StartIndex to a float of BitSize bits.
to_float(Bytes, StartIndex, BitSize) when is_binary(Bytes) ->
    <<_:StartIndex/binary, Value:BitSize/little-float, _/binary>> = Bytes,
    Value;
to_float(Bytes, StartIndex, BitSize) when is_list(Bytes) ->
    to_float(list_to_binary(Bytes), StartIndex, BitSize).
