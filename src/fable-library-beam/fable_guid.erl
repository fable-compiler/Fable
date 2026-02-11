-module(fable_guid).
-export([new_guid/0, parse/1, from_bytes/1, to_byte_array/1, to_string_format/2]).

%% Generate a new random UUID v4.
%% Format: "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx" where y is [89ab].
new_guid() ->
    %% Generate 16 random bytes
    Bytes = crypto:strong_rand_bytes(16),
    <<A:32, B:16, _:4, C:12, _:2, D:14, E:48>> = Bytes,
    %% Set version 4 and variant bits
    format_uuid(A, B, 16#4, C, 2#10, D, E).

format_uuid(A, B, Ver, C, Var, D, E) ->
    list_to_binary(
        io_lib:format("~8.16.0b-~4.16.0b-~1.16.0b~3.16.0b-~1.16.0b~3.16.0b-~12.16.0b",
                       [A, B, Ver, C, Var, D, E])).

%% Construct a GUID from a 16-byte array (list of integers).
%% .NET uses mixed-endian: first 3 groups are little-endian, last 2 are big-endian.
from_bytes(Bytes) when is_list(Bytes), length(Bytes) =:= 16 ->
    [B0,B1,B2,B3, B4,B5, B6,B7, B8,B9, B10,B11,B12,B13,B14,B15] = Bytes,
    %% Group 1 (4 bytes, little-endian in .NET)
    A = (B3 bsl 24) bor (B2 bsl 16) bor (B1 bsl 8) bor B0,
    %% Group 2 (2 bytes, little-endian)
    B = (B5 bsl 8) bor B4,
    %% Group 3 (2 bytes, little-endian)
    C = (B7 bsl 8) bor B6,
    %% Group 4 (2 bytes, big-endian)
    D = (B8 bsl 8) bor B9,
    %% Group 5 (6 bytes, big-endian)
    E = (B10 bsl 40) bor (B11 bsl 32) bor (B12 bsl 24) bor (B13 bsl 16) bor (B14 bsl 8) bor B15,
    list_to_binary(
        io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                       [A, B, C, D, E])).

%% Parse a GUID string, accepting various formats:
%% "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" (D format)
%% "{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}" (B format)
%% "(xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx)" (P format)
%% "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" (N format)
parse(Bin) when is_binary(Bin) ->
    Str = string:trim(binary_to_list(Bin)),
    Hex = extract_hex(Str),
    case length(Hex) of
        32 ->
            %% Normalize to D format (lowercase with dashes)
            {A, Rest1} = lists:split(8, Hex),
            {B, Rest2} = lists:split(4, Rest1),
            {C, Rest3} = lists:split(4, Rest2),
            {D, E} = lists:split(4, Rest3),
            list_to_binary(string:lowercase(A ++ "-" ++ B ++ "-" ++ C ++ "-" ++ D ++ "-" ++ E));
        _ ->
            erlang:error({badarg, <<"Invalid GUID format">>})
    end.

extract_hex(Str) ->
    lists:filter(fun(C) ->
        (C >= $0 andalso C =< $9) orelse
        (C >= $a andalso C =< $f) orelse
        (C >= $A andalso C =< $F)
    end, Str).

%% Convert a GUID string to a byte array (16 bytes).
%% .NET uses a mixed-endian format for GUIDs.
to_byte_array(Guid) when is_binary(Guid) ->
    Hex = extract_hex(binary_to_list(Guid)),
    bytes_from_hex(Hex, []).

bytes_from_hex([], Acc) -> lists:reverse(Acc);
bytes_from_hex([H1, H2 | Rest], Acc) ->
    Byte = list_to_integer([H1, H2], 16),
    bytes_from_hex(Rest, [Byte | Acc]).

%% Format a GUID with a specific format string.
%% N = no dashes, D = dashes (default), B = braces+dashes, P = parens+dashes
to_string_format(Guid, Format) when is_binary(Guid), is_binary(Format) ->
    case Format of
        <<"N">> -> list_to_binary(extract_hex(binary_to_list(Guid)));
        <<"D">> -> Guid;
        <<"B">> -> <<"{", Guid/binary, "}">>;
        <<"P">> -> <<"(", Guid/binary, ")">>;
        _ -> erlang:error({badarg, <<"Unsupported GUID format">>})
    end.
