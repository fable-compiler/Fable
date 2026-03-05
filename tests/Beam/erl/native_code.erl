-module(native_code).
-export([add5/1, add7/1, greet/1, multiply/2, get_name/0, add_values/2, concat_strings/2]).

add5(X) ->
    X + 5.

add7(X) ->
    X + 7.

greet(Name) ->
    iolist_to_binary([<<"Hello, ">>, Name, <<"!">>]).

multiply(X, Y) ->
    X * Y.

get_name() ->
    <<"native_code">>.

add_values(X, Y) ->
    X + Y.

concat_strings(A, B) ->
    iolist_to_binary([A, B]).
