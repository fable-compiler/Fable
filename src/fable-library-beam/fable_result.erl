-module(fable_result).
-export([map/2, map_error/2, bind/2, is_ok/1, is_error/1,
         contains/2, count/1, default_value/2, default_with/2,
         exists/2, fold/3, fold_back/3, forall/2, iter/2,
         to_array/1, to_list/1, to_option/1]).

%% Ok = {0, Value}, Error = {1, ErrValue}

map(Fn, {0, V}) -> {0, Fn(V)};
map(_Fn, Err) -> Err.

map_error(_Fn, {0, _V} = Ok) -> Ok;
map_error(Fn, {1, E}) -> {1, Fn(E)}.

bind(Fn, {0, V}) -> Fn(V);
bind(_Fn, Err) -> Err.

is_ok({0, _V}) -> true;
is_ok(_) -> false.

is_error({1, _E}) -> true;
is_error(_) -> false.

contains(Value, {0, V}) -> V =:= Value;
contains(_Value, _) -> false.

count({0, _V}) -> 1;
count(_) -> 0.

default_value(_DefVal, {0, V}) -> V;
default_value(DefVal, _) -> DefVal.

default_with(_DefFn, {0, V}) -> V;
default_with(DefFn, {1, E}) -> DefFn(E).

exists(Fn, {0, V}) -> Fn(V);
exists(_Fn, _) -> false.

%% fold: folder state result -> state
%% Curried: fun s x -> ... => fun(S) -> fun(X) -> ... end end
fold(Fn, State, {0, V}) -> (Fn(State))(V);
fold(_Fn, State, _) -> State.

%% foldBack: folder result state -> state
%% Curried: fun x s -> ... => fun(X) -> fun(S) -> ... end end
fold_back(Fn, {0, V}, State) -> (Fn(V))(State);
fold_back(_Fn, _, State) -> State.

forall(Fn, {0, V}) -> Fn(V);
forall(_Fn, _) -> true.

iter(Fn, {0, V}) -> Fn(V), ok;
iter(_Fn, _) -> ok.

to_array({0, V}) -> [V];
to_array(_) -> [].

to_list({0, V}) -> [V];
to_list(_) -> [].

%% None = undefined, Some(x) = x
to_option({0, V}) -> V;
to_option(_) -> undefined.
