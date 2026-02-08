-module(fable_option).
-export([
    default_value/2, default_with/2, map/2, bind/2,
    or_else/2, or_else_with/2, iter/2,
    map2/3, map3/4,
    contains/2, filter/2,
    fold/3, fold_back/3,
    to_array/1, to_list/1, flatten/1,
    count/1, for_all/2, exists/2
]).

default_value(Opt, DefVal) ->
    case Opt of undefined -> DefVal; _ -> Opt end.

default_with(Opt, DefFn) ->
    case Opt of undefined -> DefFn(ok); _ -> Opt end.

map(Fn, Opt) ->
    case Opt of undefined -> undefined; _ -> Fn(Opt) end.

bind(Fn, Opt) ->
    case Opt of undefined -> undefined; _ -> Fn(Opt) end.

or_else(Opt, IfNone) ->
    case Opt of undefined -> IfNone; _ -> Opt end.

or_else_with(Opt, IfNoneFn) ->
    case Opt of undefined -> IfNoneFn(ok); _ -> Opt end.

iter(Fn, Opt) ->
    case Opt of undefined -> ok; _ -> Fn(Opt), ok end.

map2(Fn, Opt1, Opt2) ->
    case {Opt1, Opt2} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        _ -> (Fn(Opt1))(Opt2)
    end.

map3(Fn, Opt1, Opt2, Opt3) ->
    case {Opt1, Opt2, Opt3} of
        {undefined, _, _} -> undefined;
        {_, undefined, _} -> undefined;
        {_, _, undefined} -> undefined;
        _ -> ((Fn(Opt1))(Opt2))(Opt3)
    end.

contains(Value, Opt) ->
    case Opt of undefined -> false; _ -> Opt =:= Value end.

filter(Fn, Opt) ->
    case Opt of
        undefined -> undefined;
        _ -> case Fn(Opt) of true -> Opt; false -> undefined end
    end.

fold(Fn, State, Opt) ->
    case Opt of undefined -> State; _ -> (Fn(State))(Opt) end.

fold_back(Fn, Opt, State) ->
    case Opt of undefined -> State; _ -> (Fn(Opt))(State) end.

to_array(Opt) ->
    case Opt of undefined -> []; _ -> [Opt] end.

to_list(Opt) ->
    case Opt of undefined -> []; _ -> [Opt] end.

flatten(Opt) ->
    case Opt of undefined -> undefined; _ -> Opt end.

count(Opt) ->
    case Opt of undefined -> 0; _ -> 1 end.

for_all(Fn, Opt) ->
    case Opt of undefined -> true; _ -> Fn(Opt) end.

exists(Fn, Opt) ->
    case Opt of undefined -> false; _ -> Fn(Opt) end.
