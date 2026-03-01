-module(fable_random).

-export([
    new/0,
    new_seeded/1,
    next/0,
    next/1,
    next/2,
    next_double/0,
    next_bytes/1
]).

-spec new() -> ok.
-spec new_seeded(integer()) -> ok.
-spec next() -> non_neg_integer().
-spec next(pos_integer()) -> non_neg_integer().
-spec next(integer(), integer()) -> integer().
-spec next_double() -> float().
-spec next_bytes(tuple() | reference()) -> ok.

%% System.Random() — no-op, uses default process-level PRNG state.
new() ->
    ok.

%% System.Random(seed) — seed the process-level PRNG for deterministic output.
new_seeded(Seed) ->
    rand:seed(exsss, Seed),
    ok.

%% Random.Next() — returns a non-negative integer less than Int32.MaxValue.
next() ->
    rand:uniform(2147483647) - 1.

%% Random.Next(maxValue) — returns a non-negative integer less than maxValue.
next(MaxValue) when MaxValue > 0 ->
    rand:uniform(MaxValue) - 1;
next(_) ->
    erlang:error(badarg).

%% Random.Next(minValue, maxValue) — returns an integer in [minValue, maxValue).
next(MinValue, MaxValue) when MaxValue > MinValue ->
    MinValue + rand:uniform(MaxValue - MinValue) - 1;
next(_, _) ->
    erlang:error(badarg).

%% Random.NextDouble() — returns a float in [0.0, 1.0).
next_double() ->
    rand:uniform().

%% Random.NextBytes(byte[]) — fills a byte array with random bytes.
%% Uses the seeded rand state so results are deterministic after seeding.
%% Accepts both direct byte_array tuples and process dictionary references.
next_bytes({byte_array, Size, AtomicsRef}) ->
    fill_random_bytes(AtomicsRef, Size, 1);
next_bytes(PdRef) when is_reference(PdRef) ->
    next_bytes(get(PdRef)).

fill_random_bytes(_AtomicsRef, 0, _Idx) ->
    ok;
fill_random_bytes(AtomicsRef, Remaining, Idx) ->
    atomics:put(AtomicsRef, Idx, rand:uniform(256) - 1),
    fill_random_bytes(AtomicsRef, Remaining - 1, Idx + 1).
