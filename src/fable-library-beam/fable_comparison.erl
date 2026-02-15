-module(fable_comparison).
-export([compare/2]).

compare(A, B) when A < B -> -1;
compare(A, B) when A > B -> 1;
compare(_, _) -> 0.
