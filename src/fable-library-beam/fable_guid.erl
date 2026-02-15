-module(fable_guid).
-export([new_guid/0, parse/1, from_bytes/1, to_byte_array/1, to_string_format/2]).

%% Generate a new random UUID v4.
%% Format: "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx" where y is [89ab].
new_guid() ->
    <<A:32, B:16, C0:16, D0:16, E:48>> = crypto:strong_rand_bytes(16),
    %% Set version 4: top 4 bits of C = 0100
    C = (C0 band 16#0FFF) bor 16#4000,
    %% Set variant: top 2 bits of D = 10
    D = (D0 band 16#3FFF) bor 16#8000,
    list_to_binary(
        io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                       [A, B, C, D, E])).

%% Construct a GUID from a 16-byte array (binary or list of integers).
%% .NET uses mixed-endian: first 3 groups are little-endian, last 2 are big-endian.
from_bytes(Bytes) when is_binary(Bytes), byte_size(Bytes) =:= 16 ->
    <<B0,B1,B2,B3, B4,B5, B6,B7, B8,B9, B10,B11,B12,B13,B14,B15>> = Bytes,
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

%% Convert a GUID string to a byte array (16 bytes binary).
%% .NET uses mixed-endian: first 3 groups little-endian, last 2 big-endian.
to_byte_array(Guid) when is_binary(Guid) ->
    Hex = extract_hex(binary_to_list(Guid)),
    AllBytes = hex_to_bytes(Hex, []),
    %% AllBytes is [B0..B15] in big-endian order from the GUID string
    [B0,B1,B2,B3, B4,B5, B6,B7, B8,B9, B10,B11,B12,B13,B14,B15] = AllBytes,
    %% Group 1 (4 bytes): little-endian swap
    %% Group 2 (2 bytes): little-endian swap
    %% Group 3 (2 bytes): little-endian swap
    %% Groups 4+5 (8 bytes): big-endian (no swap)
    list_to_binary([B3,B2,B1,B0, B5,B4, B7,B6, B8,B9, B10,B11,B12,B13,B14,B15]).

hex_to_bytes([], Acc) -> lists:reverse(Acc);
hex_to_bytes([H1, H2 | Rest], Acc) ->
    Byte = list_to_integer([H1, H2], 16),
    hex_to_bytes(Rest, [Byte | Acc]).

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
