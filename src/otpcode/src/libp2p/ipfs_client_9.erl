-module(ipfs_client_9).
-author("Zaryn Technologies").
-export([p2p_stream_close/1, p2p_stream_close/2, p2p_stream_ls/0, p2p_stream_ls/1, p2p_stream_ls/2, routing_get/1, routing_get/2, routing_provide/1,
routing_provide/2, routing_provide/3, routing_put/2, routing_put/3, routing_put/4, routing_reprovide/0, routing_reprovide/1]).

-define(RPC_API, "http://localhost:5001/api").
-define(DEFAULT_TIMEOUT, 30000).

%% @doc Close P2P stream(s) with default timeout
p2p_stream_close(Options) ->
    p2p_stream_close(Options, ?DEFAULT_TIMEOUT).

%% @doc Close P2P stream(s) with custom timeout
p2p_stream_close(Options, Timeout) ->
    try
        StreamId = proplists:get_value(arg, Options),
        CloseAll = proplists:get_value(all, Options, false),

        case {StreamId, CloseAll} of
            {undefined, false} -> 
                {error, missing_arguments};
            _ ->
                QueryParams = lists:filtermap(
                    fun({Key, Value}) ->
                        case Value of
                            undefined -> false;
                            _ -> {true, {Key, Value}}
                        end
                    end, [{arg, StreamId}, {all, CloseAll}]),

                QueryString = build_query_string(QueryParams),
                Url = ?RPC_API ++ "/v0/p2p/stream/close" ++ QueryString,

                case httpc:request(post, 
                                 {Url, [], "text/plain", ""},
                                 [{timeout, Timeout}],
                                 []) of
                    {ok, {{_, 200, _}, _, ResponseBody}} ->
                        {ok, ResponseBody};
                    {ok, {{_, StatusCode, _}, _, Body}} ->
                        {error, {status_code, StatusCode, Body}};
                    {error, Reason} ->
                        {error, Reason}
                end
        end
    catch
        error ->
            error 
    end.

%% @doc List active P2P streams with default options
p2p_stream_ls() ->
    p2p_stream_ls([], ?DEFAULT_TIMEOUT).

%% @doc List active P2P streams with options
p2p_stream_ls(Options) when is_list(Options) ->
    p2p_stream_ls(Options, ?DEFAULT_TIMEOUT);
p2p_stream_ls(Timeout) when is_integer(Timeout) ->
    p2p_stream_ls([], Timeout).

%% @doc List active P2P streams with options and timeout
p2p_stream_ls(Options, Timeout) ->
    try
        Headers = proplists:get_value(headers, Options, false),

        QueryString = case Headers of
            true -> "?headers=true";
            false -> ""
        end,
        Url = ?RPC_API ++ "/v0/p2p/stream/ls" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                case jiffy:decode(ResponseBody, [return_maps]) of
                    #{<<"Streams">> := Streams} ->
                        Formatted = lists:map(fun(S) -> 
                            #{id => maps:get(<<"HandlerID">>, S, undefined),
                              protocol => maps:get(<<"Protocol">>, S, undefined),
                              local_address => maps:get(<<"OriginAddress">>, S, undefined),
                              remote_address => maps:get(<<"TargetAddress">>, S, undefined)}
                        end, Streams),
                        {ok, Formatted};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Query routing system for a key's value
%% Required: Key to lookup (string or binary)
routing_get(Key) ->
    routing_get(Key, ?DEFAULT_TIMEOUT).

routing_get(Key, Timeout) ->
    try
        QueryString = build_query_string([{arg, Key}]),
        Url = ?RPC_API ++ "/v0/routing/get" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                decode_routing_response(ResponseBody);
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Announce provided values to the network
%% Required: Key(s) to provide (string, binary, or list)
%% Options: 
%%   - verbose: boolean()
%%   - recursive: boolean()
routing_provide(Keys) ->
    routing_provide(Keys, [], ?DEFAULT_TIMEOUT).

routing_provide(Keys, Options) when is_list(Options) ->
    routing_provide(Keys, Options, ?DEFAULT_TIMEOUT);
routing_provide(Keys, Timeout) when is_integer(Timeout) ->
    routing_provide(Keys, [], Timeout).

routing_provide(Keys, Options, Timeout) ->
    try
        Verbose = proplists:get_value(verbose, Options, false),
        Recursive = proplists:get_value(recursive, Options, false),

        % Handle single key or list of keys
        Args = case is_list(Keys) of
            true -> lists:map(fun(K) -> {arg, K} end, Keys);
            false -> [{arg, Keys}]
        end,

        ExtraParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, [{verbose, Verbose}, {recursive, Recursive}]),

        QueryString = build_query_string(Args ++ ExtraParams),
        Url = ?RPC_API ++ "/v0/routing/provide" ++ QueryString,

        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                decode_routing_response(ResponseBody);
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Store a key/value pair in the routing system
%% Required: 
%%   - Key: string() or binary()
%%   - ValueData: binary() file data to store
%% Options:
%%   - allow_offline: boolean()
routing_put(Key, ValueData) ->
    routing_put(Key, ValueData, [], ?DEFAULT_TIMEOUT).

routing_put(Key, ValueData, Options) when is_list(Options) ->
    routing_put(Key, ValueData, Options, ?DEFAULT_TIMEOUT);
routing_put(Key, ValueData, Timeout) when is_integer(Timeout) ->
    routing_put(Key, ValueData, [], Timeout).

routing_put(Key, ValueData, Options, Timeout) ->
    try
        AllowOffline = proplists:get_value(allow_offline, Options, false),

        QueryString = build_query_string([{arg, Key}, {allow_offline, AllowOffline}]),
        Url = ?RPC_API ++ "/v0/routing/put" ++ QueryString,

        Boundary = "------IPFSBoundary" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        BodyParts = [
            "--" ++ Boundary ++ "\r\n",
            "Content-Disposition: form-data; name=\"value-file\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            ValueData,
            "\r\n--" ++ Boundary ++ "--\r\n"
        ],
        Body = iolist_to_binary(BodyParts),

        case httpc:request(post, 
                         {Url, [{"Content-Type", ContentType}], ContentType, Body},
                         [{timeout, Timeout}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                decode_routing_response(ResponseBody);
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Trigger reprovider with default timeout
routing_reprovide() ->
    routing_reprovide(?DEFAULT_TIMEOUT).

%% @doc Trigger reprovider with custom timeout
routing_reprovide(Timeout) ->
    try
        Url = ?RPC_API ++ "/v0/routing/reprovide",

        case httpc:request(post, 
                         {Url, [], "text/plain", ""},
                         [{timeout, Timeout}],
                         []) of
            {ok, {{_, 200, _}, _, ResponseBody}} ->
                {ok, ResponseBody};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% Helper to decode routing responses
decode_routing_response(ResponseBody) ->
    try jiffy:decode(ResponseBody, [return_maps]) of
        #{<<"ID">> := PeerId, <<"Type">> := Type} = Map ->
            Result = #{
                id => PeerId,
                type => Type,
                extra => maps:get(<<"Extra">>, Map, undefined),
                responses => decode_routing_responses(maps:get(<<"Responses">>, Map, []))
            },
            {ok, Result};
        Other ->
            {error, {unexpected_response, Other}}
    catch
        error:Reason ->
            {error, {decode_failed, Reason}}
    end.

decode_routing_responses(Responses) ->
    lists:map(fun(#{<<"ID">> := Id, <<"Addrs">> := Addrs}) ->
        #{id => Id, addresses => Addrs}
    end, Responses).

build_query_string(Options) ->
    build_query_string(Options, "").

build_query_string([], Acc) ->
    Acc;
build_query_string([{Key, Value}|Rest], Acc) ->
    Separator = case Acc of
        "" -> "?";
        _ -> "&"
    end,
    NewAcc = Acc ++ Separator ++ to_string(Key) ++ "=" ++ to_string(Value),
    build_query_string(Rest, NewAcc).

to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_string(Value) when is_list(Value) ->
    Value;
to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(true) ->
    "true";
to_string(false) ->
    "false".
