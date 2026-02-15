-module(fable_char).
-export([
    to_upper/1, to_lower/1, to_string/1,
    is_letter/1, is_digit/1, is_letter_or_digit/1,
    is_upper/1, is_lower/1, is_number/1,
    is_whitespace/1, is_control/1,
    is_punctuation/1, is_separator/1, is_symbol/1,
    is_high_surrogate/1, is_low_surrogate/1,
    is_surrogate/1, is_surrogate_pair/2,
    char_at/2, parse/1, try_parse/2,
    get_unicode_category/1
]).

to_upper(C) when C >= $a, C =< $z -> C - 32;
to_upper(C) -> C.

to_lower(C) when C >= $A, C =< $Z -> C + 32;
to_lower(C) -> C.

to_string(C) -> <<C/utf8>>.

is_letter(C) when C >= $a, C =< $z -> true;
is_letter(C) when C >= $A, C =< $Z -> true;
is_letter(C) when C >= 16#00C0, C =< 16#00D6 -> true;
is_letter(C) when C >= 16#00D8, C =< 16#00F6 -> true;
is_letter(C) when C >= 16#00F8, C =< 16#02FF -> true;
is_letter(C) when C >= 16#0370, C =< 16#037D -> true;
is_letter(C) when C >= 16#037F, C =< 16#1FFF -> true;
is_letter(C) when C >= 16#200C, C =< 16#200D -> true;
is_letter(C) when C >= 16#2070, C =< 16#218F -> true;
is_letter(C) when C >= 16#2C00, C =< 16#2FEF -> true;
is_letter(C) when C >= 16#3001, C =< 16#D7FF -> true;
is_letter(C) when C >= 16#F900, C =< 16#FDCF -> true;
is_letter(C) when C >= 16#FDF0, C =< 16#FFFD -> true;
is_letter(C) when C >= 16#10000, C =< 16#EFFFF -> true;
is_letter(_) -> false.

is_digit(C) when C >= $0, C =< $9 -> true;
is_digit(_) -> false.

is_letter_or_digit(C) -> is_letter(C) orelse is_digit(C).

is_upper(C) when C >= $A, C =< $Z -> true;
is_upper(_) -> false.

is_lower(C) when C >= $a, C =< $z -> true;
is_lower(_) -> false.

is_number(C) when C >= $0, C =< $9 -> true;
is_number(_) -> false.

is_whitespace(C) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r;
                      C =:= 16#000B; C =:= 16#000C; C =:= 16#0085;
                      C =:= 16#00A0; C =:= 16#1680;
                      C >= 16#2000, C =< 16#200A;
                      C =:= 16#2028; C =:= 16#2029;
                      C =:= 16#202F; C =:= 16#205F; C =:= 16#3000 -> true;
is_whitespace(_) -> false.

is_control(C) when C >= 0, C =< 16#001F -> true;
is_control(C) when C >= 16#007F, C =< 16#009F -> true;
is_control(_) -> false.

is_punctuation(C) when C >= $!, C =< $# -> true;
is_punctuation(C) when C >= $%, C =< $* -> true;
is_punctuation(C) when C =:= $,; C =:= $-; C =:= $.; C =:= $/ -> true;
is_punctuation(C) when C =:= $:; C =:= $; -> true;
is_punctuation(C) when C =:= $?; C =:= $@ -> true;
is_punctuation(C) when C >= $[, C =< $] -> true;
is_punctuation(C) when C =:= $_; C =:= ${ ; C =:= $}  -> true;
is_punctuation(_) -> false.

is_separator(C) when C =:= $\s; C =:= 16#00A0; C =:= 16#1680;
                     C >= 16#2000, C =< 16#200A;
                     C =:= 16#2028; C =:= 16#2029;
                     C =:= 16#202F; C =:= 16#205F; C =:= 16#3000 -> true;
is_separator(_) -> false.

is_symbol(C) when C =:= $$; C =:= $+; C =:= $<; C =:= $=; C =:= $>;
                  C =:= $^; C =:= $`; C =:= $|; C =:= $~ -> true;
is_symbol(_) -> false.

%% Extract character at index from a binary string
char_at(Str, Idx) ->
    binary:at(Str, Idx).

%% UTF-16 surrogate detection (for .NET compatibility)
%% High surrogates: U+D800..U+DBFF
is_high_surrogate(C) when C >= 16#D800, C =< 16#DBFF -> true;
is_high_surrogate(_) -> false.

%% Low surrogates: U+DC00..U+DFFF
is_low_surrogate(C) when C >= 16#DC00, C =< 16#DFFF -> true;
is_low_surrogate(_) -> false.

%% Any surrogate: U+D800..U+DFFF
is_surrogate(C) when C >= 16#D800, C =< 16#DFFF -> true;
is_surrogate(_) -> false.

%% Surrogate pair: first is high surrogate, second is low surrogate
is_surrogate_pair(Hi, Lo) -> is_high_surrogate(Hi) andalso is_low_surrogate(Lo).

%% Parse a single-character string
parse(<<C/utf8>>) -> C;
parse(_) -> erlang:error(<<"String must be exactly one character long.">>).

%% TryParse a single-character string (uses process dict out-param)
try_parse(<<C/utf8>>, OutRef) ->
    put(OutRef, C),
    true;
try_parse(_, OutRef) ->
    put(OutRef, 0),
    false.

%% Get Unicode category (simplified - returns integer matching UnicodeCategory enum)
%% 0=UppercaseLetter, 1=LowercaseLetter, 2=TitlecaseLetter, 3=ModifierLetter,
%% 4=OtherLetter, 5=NonSpacingMark, 8=DecimalDigitNumber, 11=SpaceSeparator,
%% 18=DashPunctuation, 24=MathSymbol, 25=CurrencySymbol, 14=ConnectorPunctuation
get_unicode_category(C) when C >= $A, C =< $Z -> 0;
get_unicode_category(C) when C >= $a, C =< $z -> 1;
get_unicode_category(C) when C >= $0, C =< $9 -> 8;
get_unicode_category(C) when C =:= $\s -> 11;
get_unicode_category(C) when C =:= $- -> 18;
get_unicode_category(C) when C =:= $+; C =:= $<; C =:= $=; C =:= $>;
                             C =:= $|; C =:= $~; C =:= $^ -> 24;
get_unicode_category(C) when C =:= $$ -> 25;
get_unicode_category(C) when C =:= $_ -> 14;
get_unicode_category(_) -> 28. %% OtherNotAssigned
