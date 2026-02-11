-module(fable_bit_converter).
-export([get_bytes/2, to_int/3, to_uint/3, to_float/3]).

%% Convert a numeric value to a list of bytes (big-endian, matching BEAM convention).
%% BitConverter.IsLittleEndian returns false in Beam, so callers that check
%% endianness and conditionally reverse will get the right result.
get_bytes(Value, BitSize) when is_integer(Value) ->
    Bin = <<Value:BitSize/big-signed-integer>>,
    binary_to_list(Bin);
get_bytes(Value, 32) when is_float(Value) ->
    Bin = <<Value:32/big-float>>,
    binary_to_list(Bin);
get_bytes(Value, 64) when is_float(Value) ->
    Bin = <<Value:64/big-float>>,
    binary_to_list(Bin).

%% Convert bytes (list) starting at StartIndex to a signed integer of BitSize bits.
to_int(Bytes, StartIndex, BitSize) ->
    ByteCount = BitSize div 8,
    SubList = lists:sublist(Bytes, StartIndex + 1, ByteCount),
    Bin = list_to_binary(SubList),
    <<Value:BitSize/big-signed-integer>> = Bin,
    Value.

%% Convert bytes (list) starting at StartIndex to an unsigned integer of BitSize bits.
to_uint(Bytes, StartIndex, BitSize) ->
    ByteCount = BitSize div 8,
    SubList = lists:sublist(Bytes, StartIndex + 1, ByteCount),
    Bin = list_to_binary(SubList),
    <<Value:BitSize/big-unsigned-integer>> = Bin,
    Value.

%% Convert bytes (list) starting at StartIndex to a float of BitSize bits.
to_float(Bytes, StartIndex, BitSize) ->
    ByteCount = BitSize div 8,
    SubList = lists:sublist(Bytes, StartIndex + 1, ByteCount),
    Bin = list_to_binary(SubList),
    <<Value:BitSize/big-float>> = Bin,
    Value.
