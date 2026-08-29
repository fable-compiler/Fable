-module(fable_int).
-export([
    wrap_i8/1,
    wrap_i16/1,
    wrap_i32/1,
    wrap_i64/1,
    wrap_u8/1,
    wrap_u16/1,
    wrap_u32/1,
    wrap_u64/1
]).

-spec wrap_i8(integer()) -> integer().
-spec wrap_i16(integer()) -> integer().
-spec wrap_i32(integer()) -> integer().
-spec wrap_i64(integer()) -> integer().
-spec wrap_u8(integer()) -> non_neg_integer().
-spec wrap_u16(integer()) -> non_neg_integer().
-spec wrap_u32(integer()) -> non_neg_integer().
-spec wrap_u64(integer()) -> non_neg_integer().

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
