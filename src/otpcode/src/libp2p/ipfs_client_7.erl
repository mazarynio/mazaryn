-module(ipfs_client_7).
-author("Zaryn Technologies").
-export([stats_provide/0, stats_repo/0, stats_repo/1, swarm_addrs/0, swarm_addrs_listen/0, swarm_addrs_local/0, swarm_addrs_local/1, swarm_connect/1,
swarm_disconnect/1, swarm_filters/0, swarm_filters_add/1, swarm_filters_rm/1, swarm_peering_add/1, swarm_peering_ls/0, swarm_peering_rm/1, swarm_peers/0,
swarm_peers/1, version/0, version_check/0, version_check/1]).

-define(RPC_API, "http://localhost:5001/api").

%% @doc Returns statistics about the node's (re)provider system
%% Example:
%% {ok, ProvideStats} = ipfs_client_7:stats_provide().
stats_provide() ->
    try
        Url = ?RPC_API ++ "/v0/stats/provide",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body, [return_maps]) of
                    #{<<"AvgProvideDuration">> := _} = Result -> 
                        {ok, Result};
                    #{<<"ReproviderStats">> := Stats} -> 
                        {ok, Stats};
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
%% @doc Get stats for the currently used repo
%% Options:
%%   - size_only (boolean): Only report size info (default: false)
%%   - human (boolean): Human-readable sizes (default: false)
%% Example:
%% {ok, RepoStats} = ipfs_client_7:stats_repo([{human, true}]).
stats_repo() ->
    stats_repo([]).

stats_repo(Options) when is_list(Options) ->
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
        Url = ?RPC_API ++ "/v0/stats/repo" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body, [return_maps]) of
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

%% @doc List known swarm addresses
%% Example:
%% {ok, #{addrs := Addrs}} = ipfs_client_7:swarm_addrs().
swarm_addrs() ->
    try
        Url = ?RPC_API ++ "/v0/swarm/addrs",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Addrs">> := Addrs} ->
                        {ok, #{addrs => Addrs}};
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

%% @doc List interface listening addresses
%% Example:
%% {ok, #{listening := Addrs}} = ipfs_client_7:swarm_addrs_listen().
swarm_addrs_listen() ->
    try
        Url = ?RPC_API ++ "/v0/swarm/addrs/listen",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Strings">> := Strings} ->
                        {ok, #{listening => Strings}};
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

%% @doc List local addresses
%% Options:
%%   - id (boolean): Include peer ID in addresses (default: false)
%% Example:
%% {ok, #{addresses := Addrs}} = ipfs_client_7:swarm_addrs_local([{id, true}]).
swarm_addrs_local() ->
    swarm_addrs_local([]).

swarm_addrs_local(Options) when is_list(Options) ->
    try
        Defaults = [
            {id, false}
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
        Url = ?RPC_API ++ "/v0/swarm/addrs/local" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Strings">> := Strings} ->
                        {ok, #{addresses => Strings}};
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

%% @doc Connect to a given peer
%% Required:
%%   - Address: Multiaddress of peer to connect to (including peer ID)
%% Example:
%% ok = ipfs_client_7:swarm_connect("/ip4/1.2.3.4/tcp/4001/p2p/QmPeerID").
swarm_connect(Address) when is_list(Address) orelse is_binary(Address) ->
    try
        QueryString = build_query_string([{arg, Address}]),
        Url = ?RPC_API ++ "/v0/swarm/connect" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
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

%% @doc Close connection to a given peer
%% Required:
%%   - Address: Multiaddress of peer to disconnect from
%% Example:
%% ok = ipfs_client_7:swarm_disconnect("/ip4/1.2.3.4/tcp/4001/p2p/QmPeerID").
swarm_disconnect(Address) when is_list(Address) orelse is_binary(Address) ->
    try
        QueryString = build_query_string([{arg, Address}]),
        Url = ?RPC_API ++ "/v0/swarm/disconnect" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
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

%% @doc List current address filters
%% Example:
%% {ok, #{filters := Filters}} = ipfs_client_7:swarm_filters().
swarm_filters() ->
    try
        Url = ?RPC_API ++ "/v0/swarm/filters",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Strings">> := Strings} ->
                        {ok, #{filters => Strings}};
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

%% @doc Add an address filter
%% Required:
%%   - Address: Multiaddr to add to filters
%% Example:
%% ok = ipfs_client_7:swarm_filters_add("/ip4/1.2.3.4").
swarm_filters_add(Address) when is_list(Address) orelse is_binary(Address) ->
    try
        QueryString = build_query_string([{arg, Address}]),
        Url = ?RPC_API ++ "/v0/swarm/filters/add" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, _}} ->
                ok;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        erro ->
            error 
    end.

%% @doc Remove an address filter
%% Required:
%%   - Address: Multiaddr to remove from filters
%% Example:
%% ok = ipfs_client_7:swarm_filters_rm("/ip4/1.2.3.4").
swarm_filters_rm(Address) when is_list(Address) orelse is_binary(Address) ->
    try
        QueryString = build_query_string([{arg, Address}]),
        Url = ?RPC_API ++ "/v0/swarm/filters/rm" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
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

%% @doc Add peer to peering subsystem
%% Required:
%%   - Address: Multiaddress of peer to add (including peer ID)
%% Example:
%% {ok, #{id := PeerID, status := Status}} = ipfs_client_7:swarm_peering_add("/ip4/1.2.3.4/tcp/4001/p2p/QmPeerID").
swarm_peering_add(Address) when is_list(Address) orelse is_binary(Address) ->
    try
        QueryString = build_query_string([{arg, Address}]),
        Url = ?RPC_API ++ "/v0/swarm/peering/add" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body, [return_maps]) of
                    #{<<"ID">> := ID, <<"Status">> := Status} ->
                        {ok, #{id => ID, status => Status}};
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

%% @doc List peers in peering subsystem
%% Example:
%% {ok, #{peers := Peers}} = ipfs_client_7:swarm_peering_ls().
swarm_peering_ls() ->
    try
        Url = ?RPC_API ++ "/v0/swarm/peering/ls",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Peers">> := Peers} ->
                        {ok, #{peers => Peers}};
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

%% @doc Remove peer from peering subsystem
%% Required:
%%   - PeerID: ID of peer to remove
%% Example:
%% {ok, #{id := PeerID, status := Status}} = ipfs_client_7:swarm_peering_rm("QmPeerID").
swarm_peering_rm(PeerID) when is_list(PeerID) orelse is_binary(PeerID) ->
    try
        QueryString = build_query_string([{arg, PeerID}]),
        Url = ?RPC_API ++ "/v0/swarm/peering/rm" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"ID">> := ID, <<"Status">> := Status} ->
                        {ok, #{id => ID, status => Status}};
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

%% @doc List peers with open connections
%% Options:
%%   - verbose (boolean): Extra information (default: false)
%%   - streams (boolean): Include stream info (default: false)
%%   - latency (boolean): Include latency info (default: false)
%%   - direction (boolean): Include direction info (default: false)
%%   - identify (boolean): Include identify info (default: false)
%% Example:
%% {ok, #{peers := Peers}} = ipfs_client_7:swarm_peers([{latency, true}, {identify, true}]).
swarm_peers() ->
    swarm_peers([]).

swarm_peers(Options) when is_list(Options) ->
    try
        Defaults = [
            {verbose, false},
            {streams, false},
            {latency, false},
            {direction, false},
            {identify, false}
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
        Url = ?RPC_API ++ "/v0/swarm/peers" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Peers">> := Peers} ->
                        {ok, #{peers => Peers}};
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

%% @doc Show IPFS version information
%% Options:
%%   - number (boolean): Only show version number (default: false)
%%   - commit (boolean): Include commit hash (default: false)
%%   - repo (boolean): Include repo version (default: false)
%%   - all (boolean): Show all information (default: false)
%% Example:
%% {ok, VersionInfo} = ipfs_client_7:version([{commit, true}, {repo, true}]).
version() ->
    version([]).

version(Options) when is_list(Options) ->
    try
        Defaults = [
            {number, false},
            {commit, false},
            {repo, false},
            {all, false}
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
        Url = ?RPC_API ++ "/v0/version" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Version">> := _} = Result -> 
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

%% @doc Check version against connected peers
%% Options:
%%   - min_percent (integer): Minimum percentage for update warning (1-100, default: 5)
%% Example:
%% {ok, VersionCheck} = ipfs_client_7:version_check([{min_percent, 10}]).
version_check() ->
    version_check([]).

version_check(Options) when is_list(Options) ->
    try
        Defaults = [
            {'min-percent', 5}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    5 when Key =:= 'min-percent' -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/version/check" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"RunningVersion">> := _} = Result -> 
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