-module(fable_random).

-export([
    new/0,
    new_seeded/1,
    next/1,
    next/2,
    next/3,
    next_double/1,
    next_bytes/2
]).

%% All functions take a Ref (process dictionary key) as the first argument,
%% so each Random instance has its own independent PRNG state.

%% System.Random() — create a new instance with a random seed.
-spec new() -> reference().
new() ->
    Ref = make_ref(),
    put(Ref, rand:seed_s(exsss)),
    Ref.

%% System.Random(seed) — create a new instance with a deterministic seed.
-spec new_seeded(integer()) -> reference().
new_seeded(Seed) ->
    Ref = make_ref(),
    put(Ref, rand:seed_s(exsss, Seed)),
    Ref.

%% Random.Next() — returns a non-negative integer less than Int32.MaxValue.
-spec next(reference()) -> non_neg_integer().
next(Ref) ->
    {Val, NewState} = rand:uniform_s(2147483647, get(Ref)),
    put(Ref, NewState),
    Val - 1.

%% Random.Next(maxValue) — returns a non-negative integer less than maxValue.
-spec next(reference(), integer()) -> non_neg_integer().
next(_Ref, 0) ->
    0;
next(Ref, MaxValue) when MaxValue > 0 ->
    {Val, NewState} = rand:uniform_s(MaxValue, get(Ref)),
    put(Ref, NewState),
    Val - 1;
next(_, _) ->
    erlang:error(badarg).

%% Random.Next(minValue, maxValue) — returns an integer in [minValue, maxValue).
-spec next(reference(), integer(), integer()) -> integer().
next(Ref, MinValue, MaxValue) when MaxValue > MinValue ->
    Range = MaxValue - MinValue,
    {Val, NewState} = rand:uniform_s(Range, get(Ref)),
    put(Ref, NewState),
    MinValue + Val - 1;
next(_, _, _) ->
    erlang:error(badarg).

%% Random.NextDouble() — returns a float in [0.0, 1.0).
-spec next_double(reference()) -> float().
next_double(Ref) ->
    {Val, NewState} = rand:uniform_s(get(Ref)),
    put(Ref, NewState),
    Val.

%% Random.NextBytes(byte[]) — fills a byte array with random bytes.
-spec next_bytes(reference(), tuple() | reference()) -> ok.
next_bytes(Ref, {byte_array, Size, AtomicsRef}) ->
    fill_random_bytes(Ref, AtomicsRef, Size, 1);
next_bytes(Ref, PdRef) when is_reference(PdRef) ->
    next_bytes(Ref, get(PdRef)).

fill_random_bytes(_Ref, _AtomicsRef, 0, _Idx) ->
    ok;
fill_random_bytes(Ref, AtomicsRef, Remaining, Idx) ->
    {Val, NewState} = rand:uniform_s(256, get(Ref)),
    put(Ref, NewState),
    atomics:put(AtomicsRef, Idx, Val - 1),
    fill_random_bytes(Ref, AtomicsRef, Remaining - 1, Idx + 1).
