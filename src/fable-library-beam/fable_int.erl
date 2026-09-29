-module(fable_int).
-export([
    wrap_i8/1,
    wrap_i16/1,
    wrap_i32/1,
    wrap_i64/1,
    wrap_u8/1,
    wrap_u16/1,
    wrap_u32/1,
    wrap_u64/1,
    log2/1,
    bigint_from_byte_array/1,
    bigint_to_byte_array/1,
    bigint_pow/2,
    bigint_gcd/2
]).

-spec wrap_i8(integer()) -> integer().
-spec wrap_i16(integer()) -> integer().
-spec wrap_i32(integer()) -> integer().
-spec wrap_i64(integer()) -> integer().
-spec wrap_u8(integer()) -> non_neg_integer().
-spec wrap_u16(integer()) -> non_neg_integer().
-spec wrap_u32(integer()) -> non_neg_integer().
-spec wrap_u64(integer()) -> non_neg_integer().
-spec log2(integer()) -> non_neg_integer().
-spec bigint_from_byte_array(tuple()) -> integer().
-spec bigint_to_byte_array(integer()) -> tuple().
-spec bigint_pow(integer(), integer()) -> integer().
-spec bigint_gcd(integer(), integer()) -> non_neg_integer().

%% Fixed-width (two's complement) integer semantics for .NET sized integers.
%%
%% Erlang integers are arbitrary precision and never overflow, while .NET's
%% int8..int64/uint8..uint64 wrap. Every operation that can leave the width
%% (+, -, *, bsl, negation, narrowing conversions) is routed through these by
%% the compiler, so hash/PRNG/checksum code that relies on wraparound produces
%% bit-identical results here.
%%
%% Constructing `<<N:Bits>>` keeps the low Bits bits of N (two's complement for
%% negative N); matching it back with the signedness of the target type gives
%% the wrapped value. The in-range guard is the overwhelmingly common case and
%% short-circuits before any binary is built.

wrap_i8(N) when N >= -16#80, N =< 16#7F -> N;
wrap_i8(N) ->
    <<V:8/signed>> = <<N:8>>,
    V.

wrap_i16(N) when N >= -16#8000, N =< 16#7FFF -> N;
wrap_i16(N) ->
    <<V:16/signed>> = <<N:16>>,
    V.

wrap_i32(N) when N >= -16#80000000, N =< 16#7FFFFFFF -> N;
wrap_i32(N) ->
    <<V:32/signed>> = <<N:32>>,
    V.

wrap_i64(N) when N >= -16#8000000000000000, N =< 16#7FFFFFFFFFFFFFFF -> N;
wrap_i64(N) ->
    <<V:64/signed>> = <<N:64>>,
    V.

wrap_u8(N) when N >= 0, N =< 16#FF -> N;
wrap_u8(N) ->
    <<V:8/unsigned>> = <<N:8>>,
    V.

wrap_u16(N) when N >= 0, N =< 16#FFFF -> N;
wrap_u16(N) ->
    <<V:16/unsigned>> = <<N:16>>,
    V.

wrap_u32(N) when N >= 0, N =< 16#FFFFFFFF -> N;
wrap_u32(N) ->
    <<V:32/unsigned>> = <<N:32>>,
    V.

wrap_u64(N) when N >= 0, N =< 16#FFFFFFFFFFFFFFFF -> N;
wrap_u64(N) ->
    <<V:64/unsigned>> = <<N:64>>,
    V.

%% Compute integer log2 from bit length rather than floating point so UInt64 and
%% arbitrary-precision values stay exact above the IEEE-754 integer precision limit.
log2(N) when N < 0 ->
    erlang:error(#{
        exn_type => argument_out_of_range_exception,
        message => <<"Non-negative number required. (Parameter 'value')">>
    });
log2(0) ->
    0;
log2(N) ->
    Bin = binary:encode_unsigned(N),
    <<MostSignificantByte, _/binary>> = Bin,
    (byte_size(Bin) - 1) * 8 + log2_byte(MostSignificantByte).

log2_byte(N) when N >= 16#80 -> 7;
log2_byte(N) when N >= 16#40 -> 6;
log2_byte(N) when N >= 16#20 -> 5;
log2_byte(N) when N >= 16#10 -> 4;
log2_byte(N) when N >= 16#08 -> 3;
log2_byte(N) when N >= 16#04 -> 2;
log2_byte(N) when N >= 16#02 -> 1;
log2_byte(_) -> 0.

%% decision: encode BigInteger bytes with integer shifts -- width calculation must stay exact.
%% invariant: byte arrays use .NET's minimal little-endian two's-complement representation.
bigint_to_byte_array(0) ->
    fable_utils:new_byte_array([0]);
bigint_to_byte_array(N) ->
    fable_utils:new_byte_array(bigint_to_byte_list(N, [])).

bigint_to_byte_list(N, Acc) ->
    Byte = N band 16#FF,
    Next = N bsr 8,
    SignBitSet = Byte band 16#80 =/= 0,
    case (Next =:= 0 andalso not SignBitSet) orelse (Next =:= -1 andalso SignBitSet) of
        true -> lists:reverse([Byte | Acc]);
        false -> bigint_to_byte_list(Next, [Byte | Acc])
    end.

bigint_from_byte_array(BytesValue) ->
    Bytes = fable_utils:byte_array_to_list(BytesValue),
    case Bytes of
        [] ->
            0;
        _ ->
            Unsigned = binary:decode_unsigned(list_to_binary(Bytes), little),
            Last = lists:last(Bytes),
            case Last band 16#80 of
                0 -> Unsigned;
                _ -> Unsigned - (1 bsl (length(Bytes) * 8))
            end
    end.

%% decision: exponentiation stays in the integer domain -- math:pow/2 silently rounds large values.
%% invariant: bigint_pow/2 returns the exact integer result for every non-negative exponent.
bigint_pow(_Base, Exponent) when Exponent < 0 ->
    erlang:error(#{
        exn_type => argument_out_of_range_exception,
        message => <<"The number must be greater than or equal to zero. (Parameter 'exponent')">>
    });
bigint_pow(Base, Exponent) ->
    bigint_pow(Base, Exponent, 1).

bigint_pow(_Base, 0, Acc) ->
    Acc;
bigint_pow(Base, Exponent, Acc) when Exponent band 1 =:= 1 ->
    bigint_pow(Base * Base, Exponent bsr 1, Acc * Base);
bigint_pow(Base, Exponent, Acc) ->
    bigint_pow(Base * Base, Exponent bsr 1, Acc).

bigint_gcd(A, B) ->
    bigint_gcd_positive(erlang:abs(A), erlang:abs(B)).

bigint_gcd_positive(A, 0) ->
    A;
bigint_gcd_positive(A, B) ->
    bigint_gcd_positive(B, A rem B).
