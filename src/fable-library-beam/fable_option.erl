-module(fable_option).
-export([default_value/2, default_with/2, map/2, bind/2]).

default_value(Opt, DefVal) ->
    case Opt of undefined -> DefVal; _ -> Opt end.

default_with(Opt, DefFn) ->
    case Opt of undefined -> DefFn(ok); _ -> Opt end.

map(Fn, Opt) ->
    case Opt of undefined -> undefined; _ -> Fn(Opt) end.

bind(Fn, Opt) ->
    case Opt of undefined -> undefined; _ -> Fn(Opt) end.
