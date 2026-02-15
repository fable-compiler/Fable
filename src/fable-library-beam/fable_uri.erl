-module(fable_uri).
-export([
    create/1, create/2, create/3,
    try_create/3, try_create/4,
    is_absolute_uri/1, scheme/1, host/1, absolute_path/1,
    absolute_uri/1, path_and_query/1, query/1, fragment/1,
    original_string/1, port/1,
    to_string/1
]).

%% Uri is represented as a map:
%% #{
%%   original_string => binary(),
%%   is_absolute_uri => boolean(),
%%   scheme => binary(),
%%   host => binary(),
%%   port => integer() | undefined,
%%   path => binary(),
%%   query => binary(),      %% without "?" prefix
%%   fragment => binary(),   %% without "#" prefix
%%   normalized => binary()  %% normalized full URI string
%% }
%%
%% UriKind: 0 = RelativeOrAbsolute, 1 = Absolute, 2 = Relative

-define(URI_KIND_RELATIVE_OR_ABSOLUTE, 0).
-define(URI_KIND_ABSOLUTE, 1).
-define(URI_KIND_RELATIVE, 2).

%% ============================================================
%% Constructors
%% ============================================================

%% create(UriString) - parse as absolute, error if not
create(UriStr) when is_binary(UriStr) ->
    create(UriStr, ?URI_KIND_ABSOLUTE).

%% create(BaseUriOrString, KindOrRelative)
%% Overloaded: create(string, UriKind) or create(Uri, string) or create(Uri, Uri)
create(UriStr, Kind) when is_binary(UriStr), is_integer(Kind) ->
    %% create(string, UriKind)
    S = binary_to_list(UriStr),
    Parsed = uri_string:parse(S),
    IsAbsolute = is_map_key(scheme, Parsed) andalso maps:get(scheme, Parsed) =/= [],
    case Kind of
        ?URI_KIND_ABSOLUTE when not IsAbsolute ->
            erlang:error(<<"Invalid URI: The format of the URI could not be determined.">>);
        ?URI_KIND_RELATIVE when IsAbsolute ->
            erlang:error(<<"Invalid URI: The format of the URI could not be determined.">>);
        _ ->
            build_uri(UriStr, Parsed, IsAbsolute)
    end;
create(BaseUri, RelStr) when is_map(BaseUri), is_binary(RelStr) ->
    %% create(Uri, string) - resolve relative string against base URI
    case maps:get(is_absolute_uri, BaseUri) of
        false ->
            erlang:error(<<"Invalid URI: base URI must be absolute.">>);
        true ->
            BaseStr = binary_to_list(maps:get(original_string, BaseUri)),
            Resolved = uri_string:resolve(binary_to_list(RelStr), BaseStr),
            create(list_to_binary(Resolved), ?URI_KIND_ABSOLUTE)
    end;
create(BaseUri, RelUri) when is_map(BaseUri), is_map(RelUri) ->
    %% create(Uri, Uri) - resolve relative Uri against base URI
    case maps:get(is_absolute_uri, BaseUri) of
        false ->
            erlang:error(<<"Invalid URI: base URI must be absolute.">>);
        true ->
            case maps:get(is_absolute_uri, RelUri) of
                true ->
                    %% If relative is actually absolute, just return it
                    RelUri;
                false ->
                    RelStr = maps:get(original_string, RelUri),
                    create(BaseUri, RelStr)
            end
    end.

%% create(BaseUri, RelativeUri, _) - 3-arg form (not commonly used but for completeness)
create(BaseUri, RelUri, _Kind) ->
    create(BaseUri, RelUri).

%% ============================================================
%% TryCreate
%% ============================================================

%% try_create(string, UriKind, OutRef)
try_create(UriStr, Kind, OutRef) when is_binary(UriStr), is_integer(Kind) ->
    try
        Uri = create(UriStr, Kind),
        put(OutRef, Uri),
        true
    catch
        _:_ -> false
    end;

%% try_create(BaseUri, RelativeUriOrString, OutRef)
try_create(BaseUri, RelOrStr, OutRef) when is_map(BaseUri) ->
    try
        Uri = create(BaseUri, RelOrStr),
        put(OutRef, Uri),
        true
    catch
        _:_ -> false
    end.

%% try_create with 4 args: try_create(BaseUri, RelUri, _Kind, OutRef) or similar
try_create(A, B, C, OutRef) ->
    try
        Uri = create(A, B, C),
        put(OutRef, Uri),
        true
    catch
        _:_ -> false
    end.

%% ============================================================
%% Properties
%% ============================================================

is_absolute_uri(#{is_absolute_uri := V}) -> V.

scheme(#{scheme := V}) -> V;
scheme(_) -> <<>>.

host(#{host := V}) -> V;
host(_) -> <<>>.

port(#{port := V}) -> V;
port(_) -> undefined.

absolute_path(Uri) ->
    case maps:get(is_absolute_uri, Uri) of
        false -> erlang:error(<<"This operation is not supported for a relative URI.">>);
        true -> maps:get(path, Uri, <<"/">>)
    end.

absolute_uri(Uri) ->
    case maps:get(is_absolute_uri, Uri) of
        false -> erlang:error(<<"This operation is not supported for a relative URI.">>);
        true -> maps:get(absolute_uri_str, Uri)
    end.

path_and_query(Uri) ->
    case maps:get(is_absolute_uri, Uri) of
        false -> erlang:error(<<"This operation is not supported for a relative URI.">>);
        true ->
            Path = maps:get(path, Uri, <<"/">>),
            Q = maps:get(query, Uri, <<>>),
            case Q of
                <<>> -> Path;
                _ -> iolist_to_binary([Path, <<"?">>, Q])
            end
    end.

query(Uri) ->
    Q = maps:get(query, Uri, <<>>),
    case Q of
        <<>> -> <<>>;
        _ -> iolist_to_binary([<<"?">>, Q])
    end.

fragment(Uri) ->
    F = maps:get(fragment, Uri, <<>>),
    case F of
        <<>> -> <<>>;
        _ -> iolist_to_binary([<<"#">>, F])
    end.

original_string(#{original_string := V}) -> V.

%% ============================================================
%% ToString
%% ============================================================

to_string(#{is_absolute_uri := true, normalized := V}) -> V;
to_string(#{is_absolute_uri := false, original_string := V}) -> V.

%% ============================================================
%% Internal helpers
%% ============================================================

build_uri(OrigStr, Parsed, true) ->
    %% Absolute URI
    Scheme = list_to_binary(string:lowercase(get_parsed(scheme, Parsed, ""))),
    Host = list_to_binary(get_parsed(host, Parsed, "")),
    Port = case maps:find(port, Parsed) of
        {ok, P} -> P;
        error -> undefined
    end,
    Path = list_to_binary(get_parsed(path, Parsed, "/")),
    Query = list_to_binary(get_parsed(query, Parsed, "")),
    Fragment = list_to_binary(get_parsed(fragment, Parsed, "")),
    %% Build absolute URI string (with original casing for scheme in the URI)
    AbsUri = recompose_absolute(Scheme, Host, Port, Path, Query, Fragment),
    %% Normalized string for ToString: lowercase scheme, decode percent-encoding in path
    NormPath = list_to_binary(uri_string:percent_decode(binary_to_list(Path))),
    Normalized = recompose_display(Scheme, Host, Port, NormPath, Query, Fragment),
    #{
        original_string => OrigStr,
        is_absolute_uri => true,
        scheme => Scheme,
        host => Host,
        port => Port,
        path => Path,
        query => Query,
        fragment => Fragment,
        absolute_uri_str => AbsUri,
        normalized => Normalized
    };
build_uri(OrigStr, _Parsed, false) ->
    %% Relative URI
    #{
        original_string => OrigStr,
        is_absolute_uri => false
    }.

get_parsed(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> Default
    end.

%% Recompose absolute URI (for AbsoluteUri property)
recompose_absolute(Scheme, Host, Port, Path, Query, Fragment) ->
    PortStr = case Port of
        undefined -> <<>>;
        _ ->
            %% Omit default ports
            case is_default_port(Scheme, Port) of
                true -> <<>>;
                false -> iolist_to_binary([<<":">>, integer_to_binary(Port)])
            end
    end,
    QueryStr = case Query of
        <<>> -> <<>>;
        _ -> iolist_to_binary([<<"?">>, Query])
    end,
    FragStr = case Fragment of
        <<>> -> <<>>;
        _ -> iolist_to_binary([<<"#">>, Fragment])
    end,
    iolist_to_binary([Scheme, <<"://">>, Host, PortStr, Path, QueryStr, FragStr]).

%% Recompose for display (ToString) - with decoded path
recompose_display(Scheme, Host, Port, DecodedPath, Query, Fragment) ->
    PortStr = case Port of
        undefined -> <<>>;
        _ ->
            case is_default_port(Scheme, Port) of
                true -> <<>>;
                false -> iolist_to_binary([<<":">>, integer_to_binary(Port)])
            end
    end,
    QueryStr = case Query of
        <<>> -> <<>>;
        _ -> iolist_to_binary([<<"?">>, Query])
    end,
    FragStr = case Fragment of
        <<>> -> <<>>;
        _ -> iolist_to_binary([<<"#">>, Fragment])
    end,
    iolist_to_binary([Scheme, <<"://">>, Host, PortStr, DecodedPath, QueryStr, FragStr]).

is_default_port(<<"http">>, 80) -> true;
is_default_port(<<"https">>, 443) -> true;
is_default_port(<<"ftp">>, 21) -> true;
is_default_port(_, _) -> false.
