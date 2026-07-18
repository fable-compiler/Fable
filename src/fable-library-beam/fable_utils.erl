-module(fable_utils).
-export([
    iface_get/2,
    iface_get/3,
    field_get/2,
    inst_state/1,
    apply_curried/2,
    make_eta/2,
    make_curry/2,
    fun_ref_eq/2,
    new_ref/1,
    safe_dispose/1,
    get_enumerator/1,
    move_next/1,
    get_current/1,
    pos_infinity/0,
    neg_infinity/0,
    nan/0,
    is_infinity/1,
    new_lazy/1,
    new_lazy_from_value/1,
    force_lazy/1,
    is_value_created/1,
    using/2,
    to_list/1,
    enumerate_to_list/1,
    new_byte_array/1,
    new_byte_array_zeroed/1,
    new_byte_array_filled/2,
    byte_array_to_list/1,
    is_byte_array/1,
    byte_array_get/2,
    byte_array_set/3,
    byte_array_length/1,
    div_rem/3,
    assert_equal/2,
    assert_equal/3,
    assert_not_equal/2,
    assert_not_equal/3
]).

-spec iface_get(atom(), map() | reference() | {fable_import_all, atom()}) -> term().
-spec iface_get(atom(), non_neg_integer(), {fable_import_all, atom()}) -> term().
-spec field_get(atom(), map() | reference()) -> term().
-spec inst_state(map() | reference()) -> map().
-spec apply_curried(fun(), list()) -> term().
-spec make_eta(fun(), 2..7) -> fun().
-spec make_curry(fun(), 2..7) -> fun().
-spec fun_ref_eq(term(), term()) -> boolean().
-spec new_ref(term()) -> reference().
-spec safe_dispose(term()) -> ok.
-spec get_enumerator(list() | reference() | map() | term()) -> reference().
-spec move_next(map() | reference()) -> boolean().
-spec get_current(map() | reference()) -> term().
-spec pos_infinity() -> float().
-spec neg_infinity() -> float().
-spec nan() -> nan.
-spec is_infinity(term()) -> boolean().
-spec new_lazy(fun()) -> reference().
-spec new_lazy_from_value(term()) -> reference().
-spec force_lazy(reference()) -> term().
-spec is_value_created(reference()) -> boolean().
-spec using(term(), fun()) -> term().
-spec to_list(term()) -> list().
-spec enumerate_to_list(term()) -> list().
-spec new_byte_array(list() | binary() | tuple()) -> tuple().
-spec new_byte_array_zeroed(non_neg_integer()) -> tuple().
-spec new_byte_array_filled(non_neg_integer(), non_neg_integer()) -> tuple().
-spec byte_array_to_list(tuple()) -> list().
-spec is_byte_array(term()) -> boolean().
-spec byte_array_get(tuple() | reference(), non_neg_integer()) -> non_neg_integer().
-spec byte_array_set(tuple() | reference(), non_neg_integer(), non_neg_integer()) -> ok.
-spec byte_array_length(tuple() | reference()) -> non_neg_integer().
-spec div_rem(integer(), integer(), reference()) -> integer().
-spec assert_equal(term(), term()) -> ok.
-spec assert_equal(term(), term(), binary() | undefined) -> ok.
-spec assert_not_equal(term(), term()) -> ok.
-spec assert_not_equal(term(), term(), binary() | undefined) -> ok.

%% Interface dispatch: works for both object expressions (maps) and class instances (refs).
%% Class interface property getters are stored as {getter, Fun} tagged thunks — call Fun().
%% ObjectExpr property getters are stored as plain values — return directly.
%% ImportAll modules are tagged as {fable_import_all, ModuleAtom} and dispatched via
%% erlang:make_fun/3. The 3-arity overload passes explicit arity to avoid ambiguity
%% when a module exports the same name at multiple arities (e.g., maps:get/2 and get/3).
iface_get(Name, Arity, {fable_import_all, Mod}) ->
    erlang:make_fun(Mod, Name, Arity);
iface_get(Name, _Arity, Obj) ->
    iface_get(Name, Obj).

iface_get(Name, {fable_import_all, Mod}) ->
    Exports = Mod:module_info(exports),
    case lists:keyfind(Name, 1, Exports) of
        {Name, Arity} -> erlang:make_fun(Mod, Name, Arity);
        false -> erlang:error({no_export, Mod, Name})
    end;
iface_get(Name, Obj) when is_map(Obj) -> iface_unwrap(maps:get(Name, Obj));
iface_get(Name, Ref) -> iface_unwrap(maps:get(Name, get(Ref))).

iface_unwrap({getter, Fun}) -> Fun();
iface_unwrap(Val) -> Val.

%% Read an instance field, supporting both representations of a class instance:
%% an immutable self-contained map (process-portable, like object expressions) and
%% a process-dict ref (used for classes with mutable instance fields — single-process).
%% Decoupling the read site from the representation lets the constructor alone decide
%% which form to emit.
field_get(Field, Obj) when is_map(Obj) -> maps:get(Field, Obj);
field_get(Field, Ref) -> maps:get(Field, get(Ref)).

%% Resolve a class instance to its state map, supporting both representations.
%% Used when merging a base class's state into a derived class: the base
%% constructor may return a self-contained map or a process-dict ref.
inst_state(Obj) when is_map(Obj) -> Obj;
inst_state(Ref) -> get(Ref).

%% Create a new process dictionary ref cell with the given initial value.
%% Encapsulates make_ref() + put() to avoid variable name collisions in nested constructs.
new_ref(Value) ->
    Ref = make_ref(),
    put(Ref, Value),
    Ref.

%% Safely call Dispose on an object that may or may not implement IDisposable.
%% Handles both ref-based class instances and map-based object expressions.
safe_dispose(Obj) when is_reference(Obj) ->
    safe_dispose(get(Obj));
safe_dispose(Obj) when is_map(Obj) ->
    case maps:is_key(dispose, Obj) of
        true ->
            Fn = maps:get(dispose, Obj),
            case erlang:fun_info(Fn, arity) of
                {arity, 0} -> Fn();
                {arity, _} -> Fn(ok)
            end;
        false ->
            ok
    end;
safe_dispose(_) ->
    ok.

%% Apply a list of args one at a time to a curried function.
%% Used by CurriedApply when the target is a qualified call returning a curried function.
apply_curried(Fun, []) -> Fun;
apply_curried(Fun, [Arg | Rest]) -> apply_curried(Fun(Arg), Rest).

%% Build an N-arity uncurrying (eta) adapter around curried function F.
%%
%% Fable inserts `fun(B0, ..., Bn) -> ((F(B0))...)(Bn) end` at argument sites to normalise a
%% curried function value to the arity an uncurried slot expects. Each such adapter is a fresh
%% closure, so two adapters built over the *same* F at different sites are never `=:=` — which
%% breaks LanguagePrimitives.PhysicalEquality (e.g. removing a stored callback by identity).
%% We build the adapter here, capturing F inside a tagged `{fable_eta_adapter, F, N}` marker so
%% fun_ref_eq/2 can recover F and treat all adapters over the same F as reference-equal. The marker
%% is the adapter's ONLY captured variable, which keeps the fun_info/env recovery below unambiguous.
%%
%% Adapter cancellation (mirrors the Python target's curry.py memoization): if F is itself a
%% same-arity curry adapter (built by make_curry/2 over some G), then uncurry ∘ curry = identity, so
%% we hand back the original G unchanged rather than stacking a second wrapper. This keeps
%% round-tripped values as the *same* fun (native `=:=` holds, no wrapper cost) and stops adapters
%% accumulating. The arity guard avoids a `badarity` if a value were ever round-tripped at a
%% different arity (does not happen in practice — a value has one arity — but is cheap insurance).
make_eta(F, N) ->
    case adapter_marker(F) of
        {fable_curry_adapter, G, N} -> G;
        _ -> make_eta_marked({fable_eta_adapter, F, N}, N)
    end.

make_eta_marked(M, 2) -> fun(B0, B1) -> apply_curried(erlang:element(2, M), [B0, B1]) end;
make_eta_marked(M, 3) -> fun(B0, B1, B2) -> apply_curried(erlang:element(2, M), [B0, B1, B2]) end;
make_eta_marked(M, 4) -> fun(B0, B1, B2, B3) -> apply_curried(erlang:element(2, M), [B0, B1, B2, B3]) end;
make_eta_marked(M, 5) ->
    fun(B0, B1, B2, B3, B4) -> apply_curried(erlang:element(2, M), [B0, B1, B2, B3, B4]) end;
make_eta_marked(M, 6) ->
    fun(B0, B1, B2, B3, B4, B5) -> apply_curried(erlang:element(2, M), [B0, B1, B2, B3, B4, B5]) end;
make_eta_marked(M, 7) ->
    fun(B0, B1, B2, B3, B4, B5, B6) -> apply_curried(erlang:element(2, M), [B0, B1, B2, B3, B4, B5, B6]) end.

%% Re-curry an uncurried N-arity function F into a chain of 1-arity funs.
%%
%% Fable inserts a `Curry` node to normalise an uncurried function (applied with all args at once)
%% back to curried form. The default lowering builds `fun(A0) -> fun(A1) -> F(A0, A1) end end` fresh
%% at each site, so two curry adapters over the *same* F are never `=:=` — which breaks
%% LanguagePrimitives.PhysicalEquality. We build the adapter here, capturing F inside a tagged marker
%% so fun_ref_eq/2 can recover F and treat all adapters over it as reference-equal. F is applied with
%% all args at once (erlang:apply), matching the default multi-arg lowering exactly, so this is
%% behaviour-identical bar the marker. The `{fable_curry_adapter, F, N}` marker is the OUTERMOST
%% fun's only captured variable (referenced only in the innermost body), keeping the fun_info/env
%% recovery unambiguous; a partially-applied intermediate captures the collected args too, so it is
%% correctly left distinct.
%%
%% Adapter cancellation (see make_eta/2): if F is itself a same-arity eta adapter over some G, then
%% curry ∘ uncurry = identity, so we return the original G unchanged instead of wrapping again.
make_curry(F, N) ->
    case adapter_marker(F) of
        {fable_eta_adapter, G, N} -> G;
        _ -> make_curry_marked({fable_curry_adapter, F, N}, N)
    end.

make_curry_marked(M, 2) ->
    fun(A0) -> fun(A1) -> erlang:apply(erlang:element(2, M), [A0, A1]) end end;
make_curry_marked(M, 3) ->
    fun(A0) -> fun(A1) -> fun(A2) -> erlang:apply(erlang:element(2, M), [A0, A1, A2]) end end end;
make_curry_marked(M, 4) ->
    fun(A0) ->
        fun(A1) -> fun(A2) -> fun(A3) -> erlang:apply(erlang:element(2, M), [A0, A1, A2, A3]) end end end
    end;
make_curry_marked(M, 5) ->
    fun(A0) ->
        fun(A1) ->
            fun(A2) -> fun(A3) -> fun(A4) -> erlang:apply(erlang:element(2, M), [A0, A1, A2, A3, A4]) end end end
        end
    end;
make_curry_marked(M, 6) ->
    fun(A0) ->
        fun(A1) ->
            fun(A2) ->
                fun(A3) ->
                    fun(A4) -> fun(A5) -> erlang:apply(erlang:element(2, M), [A0, A1, A2, A3, A4, A5]) end end
                end
            end
        end
    end;
make_curry_marked(M, 7) ->
    fun(A0) ->
        fun(A1) ->
            fun(A2) ->
                fun(A3) ->
                    fun(A4) ->
                        fun(A5) ->
                            fun(A6) -> erlang:apply(erlang:element(2, M), [A0, A1, A2, A3, A4, A5, A6]) end
                        end
                    end
                end
            end
        end
    end.

%% Reference identity for function values (LanguagePrimitives.PhysicalEquality / ReferenceEquals).
%%
%% Fable-BEAM represents the *same* F# function value with different Erlang funs at different
%% sites: sometimes uncurried (a single N-arity fun), sometimes re-curried into a nested 1-arity
%% adapter (`fun(A0) -> fun(A1) -> F(A0, A1) end end`), and sometimes wrapped in an N-arity
%% uncurrying (eta) adapter built by make_eta/2 to satisfy an uncurried slot. Erlang compares funs
%% by closure identity, so these representations are never `=:=` even though they denote one value
%% — which would make PhysicalEquality wrongly return `false`.
%%
%% We normalise to the underlying function before comparing: both the eta adapter (make_eta/2) and
%% the curry adapter (make_curry/2) capture exactly their `{fable_eta_adapter, F, N}` /
%% `{fable_curry_adapter, F, N}` marker as their outermost fun's only captured variable — unwrap to
%% F. This is precise: ONLY Fable's own compiler-generated adapters carry a marker, so an unrelated
%% closure (including a hand-written eta expansion like `fun x -> g x`) is left intact and still
%% compares unequal — this is reference identity, not structural equality. Non-function values pass
%% through untouched, so this is a safe drop-in for `=:=`. (Adapter cancellation in make_eta/2 and
%% make_curry/2 already collapses round-trips to the same fun; this handles the remaining case where
%% a value is stored in one representation and compared against the other without a round-trip.)
fun_ref_eq(A, B) -> unwrap_fun(A) =:= unwrap_fun(B).

unwrap_fun(F) when is_function(F) ->
    case adapter_marker(F) of
        {_Tag, Inner, _N} -> unwrap_fun(Inner);
        none -> F
    end;
unwrap_fun(V) -> V.

%% Return the `{fable_eta_adapter, F, N}` / `{fable_curry_adapter, F, N}` marker of a
%% Fable-generated curry/eta adapter, or `none` for anything else. An adapter captures its marker as
%% its outermost fun's single environment entry, so a one-element env holding a tagged 3-tuple is an
%% unambiguous signature — no other closure Fable emits carries it.
adapter_marker(F) when is_function(F) ->
    case erlang:fun_info(F, env) of
        {env, [{fable_eta_adapter, _, _} = M]} -> M;
        {env, [{fable_curry_adapter, _, _} = M]} -> M;
        _ -> none
    end;
adapter_marker(_) -> none.

%% Enumerator support for for-in loops over lists.
%% Enumerator is a process dict ref pointing to #{items => List, current => undefined}.
get_enumerator(List) when is_list(List) ->
    Ref = make_ref(),
    put(Ref, #{items => List, current => undefined}),
    Ref;
get_enumerator(Ref) when is_reference(Ref) ->
    Stored = get(Ref),
    case is_map(Stored) of
        true ->
            %% Could be a lazy seq object (has get_enumerator key) or a class instance
            case maps:is_key(get_enumerator, Stored) of
                true ->
                    %% Lazy seq object: call its get_enumerator function
                    (maps:get(get_enumerator, Stored))();
                false ->
                    %% Other map-based ref: try keys (e.g., HashSet-like)
                    get_enumerator(maps:keys(Stored))
            end;
        false ->
            case is_list(Stored) of
                true ->
                    %% Array ref: enumerate the list
                    get_enumerator(Stored);
                false ->
                    %% Fallback
                    get_enumerator(lists:flatten([Stored]))
            end
    end;
get_enumerator(Map) when is_map(Map) ->
    case maps:is_key(get_enumerator, Map) of
        true ->
            %% Lazy seq object (plain map, not ref-wrapped): call its get_enumerator
            (maps:get(get_enumerator, Map))();
        false ->
            %% HashSet internal map: iterate over keys
            get_enumerator(maps:keys(Map))
    end;
get_enumerator({group_collection, Items, _Names}) ->
    %% Regex GroupCollection: iterate over the group items list
    get_enumerator(Items);
get_enumerator(Bin) when is_binary(Bin) ->
    %% String binary used as seq<char>: enumerate Unicode codepoints.
    get_enumerator(unicode:characters_to_list(Bin));
get_enumerator(Other) ->
    %% Fallback: treat as list
    get_enumerator(lists:flatten([Other])).

%% A compiled enumerator class can be represented either as a self-contained map
%% (immutable instance fields) or as a process-dict ref; resolve to the state map.
move_next(Enum) when is_map(Enum) ->
    %% Map-represented compiled enumerator: the instance map is the state.
    (maps:get(system_collections_i_enumerator_move_next, Enum))();
move_next(EnumRef) ->
    State = get(EnumRef),
    case maps:is_key(items, State) of
        true ->
            %% Simple list-based enumerator
            case maps:get(items, State) of
                [H | T] ->
                    put(EnumRef, State#{items := T, current := H}),
                    true;
                [] ->
                    false
            end;
        false ->
            %% Compiled seq.erl enumerator (has move_next function)
            (maps:get(system_collections_i_enumerator_move_next, State))()
    end.

get_current(Enum) when is_map(Enum) ->
    %% Map-represented compiled enumerator (immutable instance fields): the instance
    %% map is the state.
    current_of_state(Enum);
get_current(EnumRef) ->
    State = get(EnumRef),
    case maps:is_key(current, State) of
        true ->
            %% Simple list-based enumerator
            maps:get(current, State);
        false ->
            current_of_state(State)
    end.

%% Resolve the "current" value from a compiled enumerator's state map. Two shapes
%% occur and the representation (map vs ref) is orthogonal to which one applies:
%%   - compiled seq.erl enumerators store a `field_current` thunk;
%%   - a class implementing IEnumerator stores its Current getter under the interface
%%     key as a {getter, Fun} pair (generic IEnumerator<T> and/or non-generic).
current_of_state(State) ->
    case maps:is_key(field_current, State) of
        true ->
            (maps:get(field_current, State))(ok);
        false ->
            case maps:is_key(system_collections_generic_i_enumerator_1_get_current, State) of
                true -> iface_unwrap(maps:get(system_collections_generic_i_enumerator_1_get_current, State));
                false -> iface_unwrap(maps:get(system_collections_i_enumerator_get_current, State))
            end
    end.

%% IEEE 754 special float values.
%% Erlang BEAM VM doesn't support infinity/NaN as float values.
%% Use max finite float as stand-in for infinity, atom nan for NaN.
pos_infinity() -> 1.7976931348623157e308.
neg_infinity() -> -1.7976931348623157e308.
nan() -> nan.
is_infinity(X) when is_float(X) -> X >= 1.7976931348623157e308 orelse X =< -1.7976931348623157e308;
is_infinity(_) -> false.

%% Lazy<T> with memoization via process dictionary.
new_lazy(Factory) ->
    Ref = make_ref(),
    put(Ref, #{factory => Factory, is_value_created => false}),
    Ref.

%% Create an already-evaluated lazy value.
new_lazy_from_value(Value) ->
    Ref = make_ref(),
    put(Ref, #{is_value_created => true, value => Value}),
    Ref.

force_lazy(LazyRef) ->
    State = get(LazyRef),
    case maps:get(is_value_created, State) of
        true ->
            maps:get(value, State);
        false ->
            Factory = maps:get(factory, State),
            Value = Factory(ok),
            put(LazyRef, State#{is_value_created := true, value => Value}),
            Value
    end.

is_value_created(LazyRef) ->
    maps:get(is_value_created, get(LazyRef)).

%% F# `use` / `using` — execute action with resource, dispose after.
using(Resource, Action) ->
    try
        Action(Resource)
    after
        safe_dispose(Resource)
    end.

%% Convert any value to a list — derefs array refs, atomics byte arrays, and lazy seq objects.
to_list(V) when is_list(V) -> V;
to_list(V) when is_reference(V) ->
    case is_byte_array(V) of
        true ->
            byte_array_to_list(V);
        false ->
            Stored = get(V),
            case is_list(Stored) of
                % array ref
                true ->
                    Stored;
                false ->
                    %% Lazy seq object (ref to map with get_enumerator) — enumerate to list
                    enumerate_to_list(V)
            end
    end;
to_list(V) when is_map(V) ->
    case maps:is_key(get_enumerator, V) of
        true ->
            %% Lazy seq object (plain map, not ref-wrapped)
            enumerate_to_list(V);
        false ->
            %% HashSet (map) — return keys as list
            maps:keys(V)
    end;
to_list(V) when is_binary(V) ->
    %% String binary → list of Unicode codepoints
    unicode:characters_to_list(V);
to_list(V) ->
    V.

%% Enumerate a lazy seq object to a plain list using get_enumerator/move_next/get_current.
enumerate_to_list(Seq) ->
    E = get_enumerator(Seq),
    try
        enumerate_to_list_loop(E, [])
    after
        safe_dispose(E)
    end.

enumerate_to_list_loop(E, Acc) ->
    case move_next(E) of
        true ->
            enumerate_to_list_loop(E, [get_current(E) | Acc]);
        false ->
            lists:reverse(Acc)
    end.

%% Atomics-backed byte arrays for O(1) read/write.
new_byte_array({byte_array, _, _} = BA) ->
    %% Already a byte array — copy it
    new_byte_array(byte_array_to_list(BA));
new_byte_array(Bin) when is_binary(Bin) ->
    %% Binary to byte array
    new_byte_array(binary_to_list(Bin));
new_byte_array(Values) when is_list(Values) ->
    Len = erlang:length(Values),
    case Len of
        0 ->
            {byte_array, 0, atomics:new(1, [{signed, false}])};
        _ ->
            Ref = atomics:new(Len, [{signed, false}]),
            populate_byte_array(Ref, 1, Values),
            {byte_array, Len, Ref}
    end.

%% Create a zero-initialized byte array of given size (atomics are zero by default).
new_byte_array_zeroed(0) ->
    {byte_array, 0, atomics:new(1, [{signed, false}])};
new_byte_array_zeroed(Len) ->
    {byte_array, Len, atomics:new(Len, [{signed, false}])}.

%% Create a byte array of given size filled with a constant value.
%% For value 0, delegates to new_byte_array_zeroed (atomics are zero by default).
new_byte_array_filled(Len, 0) ->
    new_byte_array_zeroed(Len);
new_byte_array_filled(0, _Value) ->
    {byte_array, 0, atomics:new(1, [{signed, false}])};
new_byte_array_filled(Len, Value) ->
    Ref = atomics:new(Len, [{signed, false}]),
    fill_byte_array(Ref, 1, Len, Value),
    {byte_array, Len, Ref}.

fill_byte_array(_Ref, Idx, Len, _Value) when Idx > Len -> ok;
fill_byte_array(Ref, Idx, Len, Value) ->
    atomics:put(Ref, Idx, Value),
    fill_byte_array(Ref, Idx + 1, Len, Value).

populate_byte_array(_, _, []) ->
    ok;
populate_byte_array(Ref, Idx, [V | Rest]) ->
    atomics:put(Ref, Idx, V),
    populate_byte_array(Ref, Idx + 1, Rest).

byte_array_to_list({byte_array, 0, _}) -> [];
byte_array_to_list({byte_array, Size, Ref}) -> [atomics:get(Ref, I) || I <- lists:seq(1, Size)].

is_byte_array({byte_array, _, _}) -> true;
is_byte_array(_) -> false.

%% O(1) indexed get on byte array (0-based F# index).
byte_array_get({byte_array, _, Ref}, Idx) ->
    atomics:get(Ref, Idx + 1);
byte_array_get(PdRef, Idx) when is_reference(PdRef) ->
    byte_array_get(get(PdRef), Idx).

%% O(1) indexed set on byte array (0-based F# index).
byte_array_set({byte_array, _, Ref}, Idx, Value) ->
    atomics:put(Ref, Idx + 1, Value);
byte_array_set(PdRef, Idx, Value) when is_reference(PdRef) ->
    byte_array_set(get(PdRef), Idx, Value).

%% Length of byte array.
byte_array_length({byte_array, Size, _}) ->
    Size;
byte_array_length(PdRef) when is_reference(PdRef) ->
    byte_array_length(get(PdRef)).

%% Math.DivRem with out-ref: computes quotient and sets remainder via process dictionary.
div_rem(X, Y, RemRef) ->
    put(RemRef, X rem Y),
    X div Y.

%% Fable.Core.Testing.Assert. Mirrors fable-library-ts Util:assertEqual/assertNotEqual: on
%% failure raise, rather than return a boolean, so a failing assertion actually fails the test.
%% The raised term is a map carrying `message` plus the two operands, which is the shape
%% Fable2Beam unwraps into an F# exception -- so `e.Message` reads cleanly in `try ... with`,
%% and a test runner can still pattern-match `actual`/`expected` off the map.
assert_equal(Actual, Expected) ->
    assert_equal(Actual, Expected, undefined).

assert_equal(Actual, Expected, Msg) ->
    case fable_comparison:equals(Actual, Expected) of
        true -> ok;
        false -> assert_failed(Msg, <<"Expected">>, Actual, Expected)
    end.

assert_not_equal(Actual, Expected) ->
    assert_not_equal(Actual, Expected, undefined).

assert_not_equal(Actual, Expected, Msg) ->
    case fable_comparison:equals(Actual, Expected) of
        false -> ok;
        true -> assert_failed(Msg, <<"Expected not equal to">>, Actual, Expected)
    end.

assert_failed(Msg, Label, Actual, Expected) ->
    Message =
        case Msg of
            undefined ->
                iolist_to_binary([
                    Label, ": ", format_term(Expected), " - Actual: ", format_term(Actual)
                ]);
            _ ->
                Msg
        end,
    erlang:error(#{message => Message, actual => Actual, expected => Expected}).

format_term(V) when is_binary(V) -> V;
format_term(V) -> iolist_to_binary(io_lib:format("~p", [V])).
