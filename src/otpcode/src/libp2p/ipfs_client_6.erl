-module(ipfs_client_6).
-author("Zaryn Technologies").
-export([refs/1, refs/2, refs_local/0, repo_gc/0, repo_gc/1, repo_ls/0, repo_stat/0, repo_stat/1, repo_verify/0, repo_version/0, repo_version/1,
resolve/1, resolve/2, routing_findpeer/1, routing_findpeer/2, routing_findprovs/1, routing_findprovs/2, shutdown/0, stats_bitswap/0, stats_bitswap/1,
stats_bw/0, stats_bw/1, stats_dht/0, stats_dht/1]).

-define(RPC_API, "http://localhost:5001/api").

%% @doc List links (references) from an object
%% Required:
%%   - Path: Path to the object to list refs from
%% Options:
%%   - format (string): Edge format string (default: "<dst>")
%%   - edges (boolean): Emit edge format (default: false)
%%   - unique (boolean): Omit duplicate refs (default: false)
%%   - recursive (boolean): List links recursively (default: false)
%%   - max_depth (integer): Max recursion depth (default: -1)
%% Example:
%% {ok, Refs} = ipfs_client_6:refs("/ipfs/Qm...", [{recursive, true}, {max_depth, 2}]).
refs(Path) ->
    refs(Path, []).

refs(Path, Options) when is_list(Path) orelse is_binary(Path), is_list(Options) ->
    try
        Defaults = [
            {format, "<dst>"},
            {edges, false},
            {unique, false},
            {recursive, false},
            {'max-depth', -1}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =:= edges; Key =:= unique; Key =:= recursive -> false;
                    -1 when Key =:= 'max-depth' -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, Path}|QueryParams]),
        Url = ?RPC_API ++ "/v0/refs" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                Results = parse_ndjson(Body),
                {ok, Results};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc List all local references
%% Example:
%% {ok, LocalRefs} = ipfs_client_6:refs_local().
refs_local() ->
    try
        Url = ?RPC_API ++ "/v0/refs/local",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                Results = parse_ndjson(Body),
                {ok, Results};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Perform a garbage collection sweep on the repo
%% Options:
%%   - stream_errors (boolean): Stream errors (default: false)
%%   - quiet (boolean): Minimal output (default: false)
%%   - silent (boolean): No output (default: false)
%% Example:
%% {ok, GCResults} = ipfs_client_6:repo_gc([{quiet, true}]).
repo_gc() ->
    repo_gc([]).

repo_gc(Options) when is_list(Options) ->
    try
        Defaults = [
            {'stream-errors', false},
            {quiet, false},
            {silent, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/repo/gc" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 300000}], 
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                Results = parse_ndjson(Body),
                {ok, Results};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc List all local references in the repo
%% Example:
%% {ok, RepoRefs} = ipfs_client_6:repo_ls().
repo_ls() ->
    try
        Url = ?RPC_API ++ "/v0/repo/ls",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                Results = parse_ndjson(Body),
                {ok, Results};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Get stats for the currently used repo
%% Options:
%%   - size_only (boolean): Only report RepoSize and StorageMax (default: false)
%%   - human (boolean): Print sizes in human readable format (default: false)
%% Example:
%% {ok, RepoStats} = ipfs_client_6:repo_stat([{human, true}]).
repo_stat() ->
    repo_stat([]).

repo_stat(Options) when is_list(Options) ->
    try
        Defaults = [
            {'size-only', false},
            {human, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/repo/stat" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"NumObjects">> := _} = Result -> 
                        {ok, Result};
                    #{<<"SizeStat">> := _} = Result ->
                        {ok, Result};
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

%% @doc Verify all blocks in repo are not corrupted
%% Example:
%% {ok, Verification} = ipfs_client_6:repo_verify().
repo_verify() ->
    try
        Url = ?RPC_API ++ "/v0/repo/verify",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 600000}], 
                         [{body_format, binary}]) of 
            {ok, {{_, 200, _}, _, Body}} ->
                Results = parse_verify_response(Body),
                {ok, Results};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Show the repo version
%% Options:
%%   - quiet (boolean): Minimal output (default: false)
%% Example:
%% {ok, #{version := Version}} = ipfs_client_6:repo_version().
repo_version() ->
    repo_version([]).

repo_version(Options) when is_list(Options) ->
    try
        Defaults = [
            {quiet, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/repo/version" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Version">> := Version} ->
                        {ok, #{version => Version}};
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

%% @doc Resolve names to IPFS
%% Required:
%%   - Name: The name to resolve (IPNS or IPFS path)
%% Options:
%%   - recursive (boolean): Resolve recursively (default: true)
%%   - dht_record_count (integer): DHT records to request
%%   - dht_timeout (string): DHT timeout duration (e.g. "30s")
%% Example:
%% {ok, #{path := Path}} = ipfs_client_6:resolve("ipns://example.com", [{recursive, false}]).
resolve(Name) ->
    resolve(Name, []).

resolve(Name, Options) when is_list(Name) orelse is_binary(Name), is_list(Options) ->
    try
        Defaults = [
            {recursive, true},
            {'dht-record-count', undefined},
            {'dht-timeout', undefined}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false when Key =:= recursive -> {true, {Key, false}};
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, Name}|QueryParams]),
        Url = ?RPC_API ++ "/v0/resolve" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 60000}], 
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Path">> := Path} ->
                        {ok, #{path => Path}};
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

%% @doc Find the multiaddresses associated with a Peer ID
%% Required:
%%   - PeerID: The ID of the peer to search for
%% Options:
%%   - verbose (boolean): Print extra information (default: false)
%% Example:
%% {ok, PeerInfo} = ipfs_client_6:routing_findpeer("12D3KooW...", [{verbose, true}]).
routing_findpeer(PeerID) ->
    routing_findpeer(PeerID, []).

routing_findpeer(PeerID, Options) when is_list(PeerID) orelse is_binary(PeerID), is_list(Options) ->
    try
        Defaults = [
            {verbose, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, PeerID}|QueryParams]),
        Url = ?RPC_API ++ "/v0/routing/findpeer" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Responses">> := _} = Result -> 
                        {ok, Result};
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

%% @doc Find peers that can provide a specific value
%% Required:
%%   - Key: The key to find providers for (typically a CID)
%% Options:
%%   - verbose (boolean): Print extra information (default: false)
%%   - num_providers (integer): Number of providers to find (default: 20)
%% Example:
%% {ok, Providers} = ipfs_client_6:routing_findprovs("Qm...", [{num_providers, 5}]).
routing_findprovs(Key) ->
    routing_findprovs(Key, []).

routing_findprovs(Key, Options) when is_list(Key) orelse is_binary(Key), is_list(Options) ->
    try
        Defaults = [
            {verbose, false},
            {'num-providers', 20}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({K, Value}) -> 
                case Value of
                    false when K =:= verbose -> false;
                    20 when K =:= 'num-providers' -> false;
                    _ -> {true, {K, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, Key}|QueryParams]),
        Url = ?RPC_API ++ "/v0/routing/findprovs" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 60000}], 
                         [{stream, self}]) of 
            {ok, {{_, 200, _}, _, _}} ->
                collect_provider_results([]);
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Shut down the IPFS daemon
%% Example:
%% ok = ipfs_client_6:shutdown().
shutdown() ->
    try
        Url = ?RPC_API ++ "/v0/shutdown",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 5000}], 
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, _}} ->
                ok;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Show bitswap diagnostic information
%% Options:
%%   - verbose (boolean): Print extra information (default: false)
%%   - human (boolean): Human-readable sizes (default: false)
%% Example:
%% {ok, BitswapStats} = ipfs_client_6:stats_bitswap([{human, true}]).
stats_bitswap() ->
    stats_bitswap([]).

stats_bitswap(Options) when is_list(Options) ->
    try
        Defaults = [
            {verbose, false},
            {human, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/stats/bitswap" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"BlocksReceived">> := _} = Result -> 
                        {ok, Result};
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

%% @doc Print IPFS bandwidth information
%% Options:
%%   - peer (string): Filter by peer ID
%%   - proto (string): Filter by protocol
%%   - poll (boolean): Continuously poll bandwidth (default: false)
%%   - interval (string): Polling interval (default: "1s")
%% Example:
%% {ok, BwStats} = ipfs_client_6:stats_bw([{peer, "12D3KooW..."}, {poll, true}, {interval, "5s"}]).
stats_bw() ->
    stats_bw([]).

stats_bw(Options) when is_list(Options) ->
    try
        Defaults = [
            {peer, undefined},
            {proto, undefined},
            {poll, false},
            {interval, "1s"}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false when Key =:= poll -> false;
                    "1s" when Key =:= interval -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/stats/bw" ++ QueryString,
        
        case proplists:get_value(poll, MergedOpts, false) of
            true ->
                case httpc:request(post, 
                                 {Url, [], "application/json", ""},
                                 [{timeout, infinity}],
                                 [{stream, self}]) of
                    {ok, {{_, 200, _}, _, _}} ->
                        collect_bw_stats([]);
                    {ok, {{_, StatusCode, _}, _, Body}} ->
                        {error, {status_code, StatusCode, Body}};
                    {error, Reason} ->
                        {error, Reason}
                end;
            false ->
                case httpc:request(post, 
                                 {Url, [], "application/json", ""},
                                 [{timeout, 30000}],
                                 [{body_format, binary}]) of
                    {ok, {{_, 200, _}, _, Body}} ->
                        case jiffy:decode(Body, [return_maps]) of
                            #{<<"TotalIn">> := _} = Result ->
                                {ok, Result};
                            Other ->
                                {error, {unexpected_response, Other}}
                        end;
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

%% Helper to collect streaming bandwidth stats
collect_bw_stats(Acc) ->
    receive
        {http, {_, stream_start, _}} ->
            collect_bw_stats(Acc);
        {http, {_, stream, Data}} ->
            try
                Decoded = jsx:decode(Data, [return_maps]),
                collect_bw_stats([Decoded|Acc])
            catch
                _:_ -> 
                    collect_bw_stats(Acc)
            end;
        {http, {_, stream_end, _}} ->
            {ok, lists:reverse(Acc)};
        {http, {_, {error, Reason}}} ->
            {error, Reason}
    after
        300000 -> % 5 minute 
            {error, bw_polling_timeout}
    end.

%% @doc Returns DHT statistics
%% Options:
%%   - dht (string): Which DHT to query (wan, lan, wanserver, lanserver)
%% Example:
%% {ok, DhtStats} = ipfs_client_6:stats_dht([{dht, "wan"}]).
stats_dht() ->
    stats_dht([]).

stats_dht(Options) when is_list(Options) ->
    try
        Defaults = [
            {dht, undefined}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/stats/dht" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                Results = parse_dht_response(Body),
                case Results of
                    [] -> {error, empty_dht_response};
                    _ -> {ok, Results}
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

%% Helper to parse DHT stats response (newline-delimited JSON)
parse_dht_response(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    lists:filtermap(
        fun(Line) ->
            try
                case jsx:decode(Line, [return_maps]) of
                    #{<<"Buckets">> := _} = Result -> 
                        {true, Result};
                    #{<<"Name">> := _} = Result -> 
                        {true, Result};
                    _ -> false
                end
            catch
                _:_ -> false
            end
        end,
        Lines).

collect_provider_results(Acc) ->
    receive
        {http, {_, stream_start, _}} ->
            collect_provider_results(Acc);
        {http, {_, stream, Data}} ->
            try
                Decoded = jiffy:decode(Data, [return_maps]),
                collect_provider_results([Decoded|Acc])
            catch
                _:_ -> 
                    collect_provider_results(Acc)
            end;
        {http, {_, stream_end, _}} ->
            {ok, lists:reverse(Acc)};
        {http, {_, {error, Reason}}} ->
            {error, Reason}
    after
        60000 -> % 1 minute 
            {error, provider_lookup_timeout}
    end.

parse_verify_response(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    lists:filtermap(
        fun(Line) ->
            try
                case jiffy:decode(Line, [return_maps]) of
                    #{<<"Msg">> := _} = Result -> 
                        {true, Result};
                    #{<<"Progress">> := _} = Result -> 
                        {true, Result};
                    _ -> false
                end
            catch
                _:_ -> false
            end
        end,
        Lines).

parse_ndjson(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    lists:filtermap(
        fun(Line) ->
            try
                case jiffy:decode(Line, [return_maps]) of
                    #{<<"Ref">> := _} = Result -> 
                        {true, Result};
                    _ -> false
                end
            catch
                _:_ -> false
            end
        end,
        Lines).

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

merge_options(Defaults, Options) ->
    lists:ukeymerge(1, 
        lists:ukeysort(1, Options), 
        lists:ukeysort(1, Defaults)).