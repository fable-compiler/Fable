-module(fable_option).
-export([
    default_value/2, default_with/2, map/2, bind/2,
    or_else/2, or_else_with/2, iter/2,
    map2/3, map3/4,
    contains/2, filter/2,
    fold/3, fold_back/3,
    to_array/1, to_list/1, flatten/1,
    count/1, for_all/2, exists/2,
    some/1, value/1
]).

%% Unwrap an option value: {some, V} -> V, plain V -> V.
%% Used internally to extract the inner value before passing to user callbacks.
unwrap({some, V}) -> V;
unwrap(V) -> V.

%% Smart constructor: wraps values that would be ambiguous with None (undefined)
%% or represent nested options ({some, _}). Mirrors JS behavior where some(null) wraps.
some(undefined) -> {some, undefined};
some({some, _} = V) -> {some, V};
some(V) -> V.

value(undefined) -> erlang:error(<<"Option has no value">>);
value({some, V}) -> V;
value(V) -> V.

default_value(Opt, DefVal) ->
    case Opt of undefined -> DefVal; _ -> Opt end.

default_with(Opt, DefFn) ->
    case Opt of undefined -> DefFn(ok); _ -> Opt end.

map(Fn, Opt) ->
    case Opt of undefined -> undefined; _ -> some(Fn(unwrap(Opt))) end.

bind(Fn, Opt) ->
    case Opt of undefined -> undefined; _ -> Fn(unwrap(Opt)) end.

or_else(Opt, IfNone) ->
    case Opt of undefined -> IfNone; _ -> Opt end.

or_else_with(Opt, IfNoneFn) ->
    case Opt of undefined -> IfNoneFn(ok); _ -> Opt end.

iter(Fn, Opt) ->
    case Opt of undefined -> ok; _ -> Fn(unwrap(Opt)), ok end.

map2(Fn, Opt1, Opt2) ->
    case {Opt1, Opt2} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        _ -> some((Fn(unwrap(Opt1)))(unwrap(Opt2)))
    end.

map3(Fn, Opt1, Opt2, Opt3) ->
    case {Opt1, Opt2, Opt3} of
        {undefined, _, _} -> undefined;
        {_, undefined, _} -> undefined;
        {_, _, undefined} -> undefined;
        _ -> some(((Fn(unwrap(Opt1)))(unwrap(Opt2)))(unwrap(Opt3)))
    end.

contains(Value, Opt) ->
    case Opt of undefined -> false; _ -> unwrap(Opt) =:= Value end.

filter(Fn, Opt) ->
    case Opt of
        undefined -> undefined;
        _ -> case Fn(unwrap(Opt)) of true -> Opt; false -> undefined end
    end.

fold(Fn, State, Opt) ->
    case Opt of undefined -> State; _ -> (Fn(State))(unwrap(Opt)) end.

fold_back(Fn, Opt, State) ->
    case Opt of undefined -> State; _ -> (Fn(unwrap(Opt)))(State) end.

to_array(Opt) ->
    case Opt of undefined -> []; _ -> [unwrap(Opt)] end.

to_list(Opt) ->
    case Opt of undefined -> []; _ -> [unwrap(Opt)] end.

flatten(Opt) ->
    case Opt of undefined -> undefined; _ -> unwrap(Opt) end.

count(Opt) ->
    case Opt of undefined -> 0; _ -> 1 end.

for_all(Fn, Opt) ->
    case Opt of undefined -> true; _ -> Fn(unwrap(Opt)) end.

exists(Fn, Opt) ->
    case Opt of undefined -> false; _ -> Fn(unwrap(Opt)) end.
