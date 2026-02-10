-module(fable_regex).
-export([
    create/1, create/2,
    is_match/2, is_match/3,
    match/2, match/3,
    matches/2, matches/3,
    replace/3, replace/4, replace/5,
    replace_evaluator/3, replace_evaluator/4, replace_evaluator/5,
    split/2, split/3, split/4,
    escape/1, unescape/1,
    get_value/1, get_index/1, get_length/1, get_success/1,
    get_groups/1, get_item/2, get_count/1, get_options/1
]).

%% Regex object: #{pattern => Pat, compiled => MP, options => Opts}
%% Match result: #{success => true, value => Bin, index => Idx, length => Len, groups => [Group, ...], input => Input}
%% Group: #{success => Bool, value => Bin, index => Idx, length => Len}

%% --- Constructor ---

create(Pattern) ->
    create(Pattern, 0).

create(Pattern, Options) ->
    ErlOpts = options_to_erl(Options),
    {ok, MP} = re:compile(Pattern, [unicode | ErlOpts]),
    #{pattern => Pattern, compiled => MP, options => Options}.

%% --- IsMatch ---

is_match(InputOrRegex, PatternOrInput) ->
    case is_map(InputOrRegex) andalso maps:is_key(compiled, InputOrRegex) of
        true ->
            %% Instance call: Regex.IsMatch(input)
            Regex = InputOrRegex,
            Input = PatternOrInput,
            is_match_impl(Input, Regex, 0);
        false ->
            %% Static call: Regex.IsMatch(input, pattern)
            Input = InputOrRegex,
            Pattern = PatternOrInput,
            Regex = create(Pattern),
            is_match_impl(Input, Regex, 0)
    end.

is_match(RegexOrInput, InputOrPattern, OptsOrOffset) ->
    case is_map(RegexOrInput) andalso maps:is_key(compiled, RegexOrInput) of
        true ->
            %% Instance call: regex.IsMatch(input, offset)
            Regex = RegexOrInput,
            Input = InputOrPattern,
            Offset = OptsOrOffset,
            is_match_impl(Input, Regex, Offset);
        false ->
            %% Static call: Regex.IsMatch(input, pattern, options)
            Input = RegexOrInput,
            Pattern = InputOrPattern,
            Options = OptsOrOffset,
            Regex = create(Pattern, Options),
            is_match_impl(Input, Regex, 0)
    end.

is_match_impl(Input, #{compiled := MP}, Offset) ->
    ByteOffset = byte_offset(Input, Offset),
    case re:run(Input, MP, [{offset, ByteOffset}]) of
        {match, _} -> true;
        nomatch -> false
    end.

%% --- Match ---

match(InputOrRegex, PatternOrInput) ->
    case is_map(InputOrRegex) andalso maps:is_key(compiled, InputOrRegex) of
        true ->
            Regex = InputOrRegex,
            Input = PatternOrInput,
            match_impl(Input, Regex, 0);
        false ->
            Input = InputOrRegex,
            Pattern = PatternOrInput,
            Regex = create(Pattern),
            match_impl(Input, Regex, 0)
    end.

match(RegexOrInput, InputOrPattern, OptsOrOffset) ->
    case is_map(RegexOrInput) andalso maps:is_key(compiled, RegexOrInput) of
        true ->
            Regex = RegexOrInput,
            Input = InputOrPattern,
            Offset = OptsOrOffset,
            match_impl(Input, Regex, Offset);
        false ->
            Input = RegexOrInput,
            Pattern = InputOrPattern,
            Options = OptsOrOffset,
            Regex = create(Pattern, Options),
            match_impl(Input, Regex, 0)
    end.

match_impl(Input, #{compiled := MP} = Regex, Offset) ->
    ByteOffset = byte_offset(Input, Offset),
    case re:run(Input, MP, [{offset, ByteOffset}, {capture, all, index}]) of
        {match, Captured} ->
            build_match(Input, Captured, Regex);
        nomatch ->
            NumGroups = count_groups(Regex),
            failed_match(NumGroups)
    end.

%% --- Matches ---

matches(InputOrRegex, PatternOrInput) ->
    case is_map(InputOrRegex) andalso maps:is_key(compiled, InputOrRegex) of
        true ->
            Regex = InputOrRegex,
            Input = PatternOrInput,
            matches_impl(Input, Regex, 0);
        false ->
            Input = InputOrRegex,
            Pattern = PatternOrInput,
            Regex = create(Pattern),
            matches_impl(Input, Regex, 0)
    end.

matches(RegexOrInput, InputOrPattern, OptsOrOffset) ->
    case is_map(RegexOrInput) andalso maps:is_key(compiled, RegexOrInput) of
        true ->
            Regex = RegexOrInput,
            Input = InputOrPattern,
            Offset = OptsOrOffset,
            matches_impl(Input, Regex, Offset);
        false ->
            Input = RegexOrInput,
            Pattern = InputOrPattern,
            Options = OptsOrOffset,
            Regex = create(Pattern, Options),
            matches_impl(Input, Regex, 0)
    end.

matches_impl(Input, #{compiled := MP} = Regex, Offset) ->
    ByteOffset = byte_offset(Input, Offset),
    case re:run(Input, MP, [global, {offset, ByteOffset}, {capture, all, index}]) of
        {match, AllCaptures} ->
            lists:map(fun(Captured) -> build_match(Input, Captured, Regex) end, AllCaptures);
        nomatch ->
            []
    end.

%% --- Replace ---

replace(InputOrRegex, PatternOrInput, Replacement) ->
    case is_map(InputOrRegex) andalso maps:is_key(compiled, InputOrRegex) of
        true ->
            Regex = InputOrRegex,
            Input = PatternOrInput,
            replace_impl(Input, Regex, Replacement, 0, 0);
        false ->
            Input = InputOrRegex,
            Pattern = PatternOrInput,
            Regex = create(Pattern),
            replace_impl(Input, Regex, Replacement, 0, 0)
    end.

replace(RegexOrInput, InputOrPattern, ReplacementOrOpts, CountOrReplacement) ->
    case is_map(RegexOrInput) andalso maps:is_key(compiled, RegexOrInput) of
        true ->
            %% Instance: regex.Replace(input, replacement, count)
            Regex = RegexOrInput,
            Input = InputOrPattern,
            Replacement = ReplacementOrOpts,
            Count = CountOrReplacement,
            replace_impl(Input, Regex, Replacement, Count, 0);
        false ->
            %% Static: Regex.Replace(input, pattern, replacement, options)
            Input = RegexOrInput,
            Pattern = InputOrPattern,
            Replacement = ReplacementOrOpts,
            Options = CountOrReplacement,
            Regex = create(Pattern, Options),
            replace_impl(Input, Regex, Replacement, 0, 0)
    end.

replace(Regex, Input, Replacement, Count, Offset) ->
    replace_impl(Input, Regex, Replacement, Count, Offset).

replace_impl(Input, #{compiled := MP}, Replacement, Count, Offset) ->
    ByteOffset = byte_offset(Input, Offset),
    ErlReplacement = convert_replacement(Replacement),
    Opts = [{offset, ByteOffset}, {return, binary}],
    Opts2 = case Count of
        0 -> [global | Opts];
        _ -> Opts
    end,
    case Count > 1 of
        true ->
            %% re:replace doesn't support count > 1 directly, do it manually
            replace_n(Input, MP, ErlReplacement, Count, ByteOffset);
        false ->
            iolist_to_binary(re:replace(Input, MP, ErlReplacement, Opts2))
    end.

replace_n(Input, MP, Replacement, Count, Offset) ->
    replace_n(Input, MP, Replacement, Count, Offset, 0).

replace_n(Input, _MP, _Replacement, Count, _Offset, Done) when Done >= Count ->
    Input;
replace_n(Input, MP, Replacement, Count, Offset, Done) ->
    case re:run(Input, MP, [{offset, Offset}, {capture, all, index}]) of
        {match, [{Start, Len} | _Groups]} ->
            Before = binary:part(Input, 0, Start),
            After = binary:part(Input, Start + Len, byte_size(Input) - Start - Len),
            %% Expand replacement references
            Expanded = expand_replacement(Replacement, Input, MP, Start, Offset),
            NewInput = <<Before/binary, Expanded/binary, After/binary>>,
            NewOffset = byte_size(Before) + byte_size(Expanded),
            replace_n(NewInput, MP, Replacement, Count, NewOffset, Done + 1);
        nomatch ->
            Input
    end.

expand_replacement(Replacement, _Input, _MP, _MatchStart, _Offset) ->
    %% For simple replacements, just return as-is
    %% The re:replace already handles \N references
    Replacement.

%% --- Replace with evaluator ---

replace_evaluator(InputOrRegex, PatternOrInput, Evaluator) ->
    case is_map(InputOrRegex) andalso maps:is_key(compiled, InputOrRegex) of
        true ->
            Regex = InputOrRegex,
            Input = PatternOrInput,
            replace_evaluator_impl(Input, Regex, Evaluator, 0, 0);
        false ->
            Input = InputOrRegex,
            Pattern = PatternOrInput,
            Regex = create(Pattern),
            replace_evaluator_impl(Input, Regex, Evaluator, 0, 0)
    end.

replace_evaluator(Regex, Input, Evaluator, Count) ->
    replace_evaluator_impl(Input, Regex, Evaluator, Count, 0).

replace_evaluator(Regex, Input, Evaluator, Count, Offset) ->
    replace_evaluator_impl(Input, Regex, Evaluator, Count, Offset).

replace_evaluator_impl(Input, #{compiled := MP} = Regex, Evaluator, Count, Offset) ->
    ByteOffset = byte_offset(Input, Offset),
    AllMatches = case re:run(Input, MP, [global, {offset, ByteOffset}, {capture, all, index}]) of
        {match, Ms} -> Ms;
        nomatch -> []
    end,
    MatchesToProcess = case Count of
        0 -> AllMatches;
        _ -> lists:sublist(AllMatches, Count)
    end,
    %% Process matches from right to left to preserve indices
    Sorted = lists:reverse(MatchesToProcess),
    lists:foldl(fun(Captured, Acc) ->
        M = build_match(Acc, Captured, Regex),
        ReplacementText = Evaluator(M),
        [{MatchStart, MatchLen} | _] = Captured,
        Before = binary:part(Acc, 0, MatchStart),
        After = binary:part(Acc, MatchStart + MatchLen, byte_size(Acc) - MatchStart - MatchLen),
        <<Before/binary, ReplacementText/binary, After/binary>>
    end, Input, Sorted).

%% --- Split ---

split(InputOrRegex, PatternOrInput) ->
    case is_map(InputOrRegex) andalso maps:is_key(compiled, InputOrRegex) of
        true ->
            Regex = InputOrRegex,
            Input = PatternOrInput,
            split_impl(Input, Regex, 0, 0);
        false ->
            Input = InputOrRegex,
            Pattern = PatternOrInput,
            Regex = create(Pattern),
            split_impl(Input, Regex, 0, 0)
    end.

split(RegexOrInput, InputOrPattern, CountOrOpts) ->
    case is_map(RegexOrInput) andalso maps:is_key(compiled, RegexOrInput) of
        true ->
            Regex = RegexOrInput,
            Input = InputOrPattern,
            Count = CountOrOpts,
            split_impl(Input, Regex, Count, 0);
        false ->
            Input = RegexOrInput,
            Pattern = InputOrPattern,
            Options = CountOrOpts,
            Regex = create(Pattern, Options),
            split_impl(Input, Regex, 0, 0)
    end.

split(Regex, Input, Count, Offset) ->
    split_impl(Input, Regex, Count, Offset).

split_impl(Input, #{compiled := MP}, Count, Offset) ->
    ByteOffset = byte_offset(Input, Offset),
    InputPart = case ByteOffset of
        0 -> Input;
        _ -> binary:part(Input, ByteOffset, byte_size(Input) - ByteOffset)
    end,
    Prefix = case ByteOffset of
        0 -> <<>>;
        _ -> binary:part(Input, 0, ByteOffset)
    end,
    Parts = re:split(InputPart, MP, [{return, binary}]),
    Result = case Prefix of
        <<>> -> Parts;
        _ -> [<<Prefix/binary, (hd(Parts))/binary>> | tl(Parts)]
    end,
    case Count of
        0 -> Result;
        _ ->
            case length(Result) > Count of
                true ->
                    {First, Rest} = lists:split(Count - 1, Result),
                    Joined = iolist_to_binary(lists:join(<<>>, Rest)),
                    First ++ [Joined];
                false ->
                    Result
            end
    end.

%% --- Escape / Unescape ---

escape(Str) ->
    escape_chars(Str, <<>>).

escape_chars(<<>>, Acc) -> Acc;
escape_chars(<<C/utf8, Rest/binary>>, Acc) ->
    case is_special_char(C) of
        true -> escape_chars(Rest, <<Acc/binary, $\\, C/utf8>>);
        false -> escape_chars(Rest, <<Acc/binary, C/utf8>>)
    end.

is_special_char(C) ->
    lists:member(C, [$\\, $^, $$, $., $|, $?, $*, $+, $(, $), $[, $], ${, $}]).

unescape(Str) ->
    unescape_chars(Str, <<>>).

unescape_chars(<<>>, Acc) -> Acc;
unescape_chars(<<$\\, C/utf8, Rest/binary>>, Acc) ->
    unescape_chars(Rest, <<Acc/binary, C/utf8>>);
unescape_chars(<<C/utf8, Rest/binary>>, Acc) ->
    unescape_chars(Rest, <<Acc/binary, C/utf8>>).

%% --- Property accessors ---

get_value(#{value := V}) -> V.
get_index(#{index := I}) -> I.
get_length(#{length := L}) -> L.
get_success(#{success := S}) -> S.

get_groups(#{groups := G}) -> G.

get_item(Collection, Index) when is_list(Collection) ->
    lists:nth(Index + 1, Collection).

get_count(Collection) when is_list(Collection) ->
    length(Collection).

get_options(#{options := O}) -> O.

%% --- Internal helpers ---

options_to_erl(Options) when is_integer(Options) ->
    Flags = [],
    F1 = case Options band 1 of 1 -> [caseless | Flags]; _ -> Flags end,     %% IgnoreCase
    F2 = case Options band 2 of 2 -> [multiline | F1]; _ -> F1 end,          %% Multiline
    F3 = case Options band 16 of 16 -> [dotall | F2]; _ -> F2 end,           %% Singleline
    F3.

byte_offset(_Input, CharOffset) when CharOffset =< 0 -> 0;
byte_offset(Input, CharOffset) ->
    %% Convert character offset to byte offset for UTF-8 binaries
    byte_offset_impl(Input, CharOffset, 0).

byte_offset_impl(_Input, 0, BytePos) -> BytePos;
byte_offset_impl(Input, CharsLeft, BytePos) when BytePos < byte_size(Input) ->
    <<_:BytePos/binary, C/utf8, _/binary>> = Input,
    CharBytes = char_byte_size(C),
    byte_offset_impl(Input, CharsLeft - 1, BytePos + CharBytes);
byte_offset_impl(_Input, _CharsLeft, BytePos) -> BytePos.

char_byte_size(C) when C < 16#80 -> 1;
char_byte_size(C) when C < 16#800 -> 2;
char_byte_size(C) when C < 16#10000 -> 3;
char_byte_size(_) -> 4.

count_groups(#{pattern := Pattern}) ->
    count_parens(Pattern, 0, false).

build_match(Input, Captured, Regex) ->
    [{MatchStart, MatchLen} | GroupCaptures] = Captured,
    MatchValue = case MatchLen of
        0 -> <<>>;
        _ -> binary:part(Input, MatchStart, MatchLen)
    end,
    %% Character index (not byte index)
    CharIndex = char_index(Input, MatchStart),
    CharLength = string:length(MatchValue),
    %% Build group 0 (the full match) plus sub-groups
    Group0 = #{success => true, value => MatchValue, index => CharIndex, length => CharLength},
    NumExpectedGroups = count_groups(Regex),
    BuiltGroups = build_groups(Input, GroupCaptures),
    %% Pad with failed groups if re:run returned fewer captures than pattern groups
    PaddedGroups = pad_groups(BuiltGroups, NumExpectedGroups),
    Groups = [Group0 | PaddedGroups],
    #{success => true, value => MatchValue, index => CharIndex, length => CharLength,
      groups => Groups, input => Input}.

build_groups(_Input, []) -> [];
build_groups(Input, [{-1, 0} | Rest]) ->
    [#{success => false, value => <<>>, index => -1, length => 0} | build_groups(Input, Rest)];
build_groups(Input, [{Start, Len} | Rest]) ->
    Value = case Len of
        0 -> <<>>;
        _ -> binary:part(Input, Start, Len)
    end,
    CharIndex = char_index(Input, Start),
    CharLength = string:length(Value),
    [#{success => true, value => Value, index => CharIndex, length => CharLength} | build_groups(Input, Rest)].

pad_groups(Groups, ExpectedCount) ->
    Len = length(Groups),
    case Len >= ExpectedCount of
        true -> Groups;
        false ->
            FailedGroup = #{success => false, value => <<>>, index => -1, length => 0},
            Groups ++ lists:duplicate(ExpectedCount - Len, FailedGroup)
    end.

count_parens(<<>>, Count, _Escaped) -> Count;
count_parens(<<$\\, _, Rest/binary>>, Count, _Escaped) ->
    %% Skip escaped character
    count_parens(Rest, Count, false);
count_parens(<<$[, Rest/binary>>, Count, _Escaped) ->
    %% Skip character class contents
    skip_char_class(Rest, Count);
count_parens(<<$(, $?, Rest/binary>>, Count, _Escaped) ->
    %% Non-capturing group (?:...) or other extensions - don't count
    %% But named groups (?P<name>...) and (?<name>...) do count
    case Rest of
        <<$<, Rest2/binary>> ->
            %% (?<name>...) - named group, counts
            count_parens(Rest2, Count + 1, false);
        <<$P, $<, Rest2/binary>> ->
            %% (?P<name>...) - named group, counts
            count_parens(Rest2, Count + 1, false);
        _ ->
            %% (?:...) or (?=...) etc. - don't count
            count_parens(Rest, Count, false)
    end;
count_parens(<<$(, Rest/binary>>, Count, _Escaped) ->
    count_parens(Rest, Count + 1, false);
count_parens(<<_, Rest/binary>>, Count, _Escaped) ->
    count_parens(Rest, Count, false).

skip_char_class(<<>>, Count) -> Count;
skip_char_class(<<$\\, _, Rest/binary>>, Count) ->
    skip_char_class(Rest, Count);
skip_char_class(<<$], Rest/binary>>, Count) ->
    count_parens(Rest, Count, false);
skip_char_class(<<_, Rest/binary>>, Count) ->
    skip_char_class(Rest, Count).

char_index(_Input, 0) -> 0;
char_index(Input, BytePos) ->
    Prefix = binary:part(Input, 0, BytePos),
    string:length(Prefix).

failed_match(NumGroups) ->
    FailedGroup = #{success => false, value => <<>>, index => -1, length => 0},
    Groups = lists:duplicate(NumGroups + 1, FailedGroup),
    #{success => false, value => <<>>, index => -1, length => 0, groups => Groups, input => <<>>}.

%% Convert .NET replacement syntax ($1, $2, $0, $$) to Erlang re:replace syntax (\1, \2, \0, $)
convert_replacement(Bin) ->
    convert_replacement(Bin, <<>>).

convert_replacement(<<>>, Acc) -> Acc;
convert_replacement(<<"$$", Rest/binary>>, Acc) ->
    %% $$ → literal $
    convert_replacement(Rest, <<Acc/binary, $$>>);
convert_replacement(<<"$0", Rest/binary>>, Acc) ->
    %% $0 → & (whole match in Erlang re:replace)
    convert_replacement_digits_zero(Rest, <<Acc/binary, $&>>);
convert_replacement(<<"$", D, Rest/binary>>, Acc) when D >= $1, D =< $9 ->
    %% $N → \N for group references
    convert_replacement_digits(Rest, <<Acc/binary, $\\, D>>);
convert_replacement(<<$&, Rest/binary>>, Acc) ->
    %% Escape literal & since it's special in re:replace
    convert_replacement(Rest, <<Acc/binary, $\\, $&>>);
convert_replacement(<<$\\, Rest/binary>>, Acc) ->
    %% Escape literal \ since it's special in re:replace
    convert_replacement(Rest, <<Acc/binary, $\\, $\\>>);
convert_replacement(<<C/utf8, Rest/binary>>, Acc) ->
    convert_replacement(Rest, <<Acc/binary, C/utf8>>).

%% After $0, consume remaining digits but ignore them (whole match is just &)
convert_replacement_digits_zero(<<D, Rest/binary>>, Acc) when D >= $0, D =< $9 ->
    %% Multi-digit $0N - treat as & followed by literal digits
    convert_replacement_digits_zero(Rest, <<Acc/binary, D>>);
convert_replacement_digits_zero(Rest, Acc) ->
    convert_replacement(Rest, Acc).

convert_replacement_digits(<<D, Rest/binary>>, Acc) when D >= $0, D =< $9 ->
    convert_replacement_digits(Rest, <<Acc/binary, D>>);
convert_replacement_digits(Rest, Acc) ->
    convert_replacement(Rest, Acc).
