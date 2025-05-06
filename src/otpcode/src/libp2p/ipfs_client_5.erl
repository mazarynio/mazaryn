-module(ipfs_client_5).
-author("Zaryn Technologies").
-export([name_publish/1, name_publish/2, name_resolve/0, name_resolve/1, pin_add/1, pin_add/2, pin_ls/0, pin_ls/1, 
         pin_remote_add/2, pin_remote_add/3, pin_remote_ls/1, pin_remote_rm/1, pin_remote_rm/2, 
         pin_remote_service_add/3, pin_remote_service_ls/0, pin_remote_service_ls/1, pin_remote_service_rm/1,
         pin_rm/1, pin_rm/2, pin_update/2, pin_update/3, pin_verify/0, pin_verify/1, ping/1, ping/2]).

-define(RPC_API, "http://localhost:5001/api").

%% @doc Publish IPNS names concurrently and asynchronously, but return the result synchronously.
%% Returns {ok, #{name => Binary, value => Binary}} or {error, Reason}.
%% {ok, #{name := Name}} = ipfs_client_5:name_publish("/ipfs/Qm...").
name_publish(IPFSPath) ->
    name_publish(IPFSPath, []).

name_publish(IPFSPath, Options) when is_list(IPFSPath); is_binary(IPFSPath) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        Result = sync_name_publish(IPFSPath, Options),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, {ok, Map}} when is_map(Map), is_map_key(name, Map), is_map_key(value, Map) ->
            {ok, Map};
        {Ref, {error, Reason}} ->
            {error, Reason}
    after 900000 -> 
        {error, ipns_timeout}
    end.

sync_name_publish(IPFSPath, Options) ->
    try
        Defaults = [
            {key, "self"},
            {resolve, false},              
            {lifetime, "24h"},             
            {ttl, "1m"},                   
            {quieter, true},               
            {v1compat, true},
            {'allow-offline', false},      
            {'ipns-base', "base58btc"},    
            {'nocache', true}              
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =:= resolve -> {true, {Key, false}};
                    false when Key =:= v1compat -> {true, {Key, false}};
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, IPFSPath}|QueryParams]),
        Url = ?RPC_API ++ "/v0/name/publish" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [
                             {timeout, 30000},
                             {connect_timeout, 5000}
                         ],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Name">> := Name, <<"Value">> := Value} ->
                        {ok, #{name => binary_to_list(Name), value => binary_to_list(Value)}};
                    Other ->
                        {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ -> {error, internal_error}
    end.

%% @doc Helper function to merge options with defaults.
merge_options(Provided, Defaults) ->
    lists:ukeymerge(1, lists:ukeysort(1, Provided), lists:ukeysort(1, Defaults)).

%% @doc Helper function to build query string from parameters.
build_query_string(Params) ->
    case Params of
        [] -> "";
        _ ->
            "?" ++
            string:join(
                lists:map(
                    fun({Key, Value}) ->
                        KeyStr = atom_to_list(Key),
                        ValueStr = uri_string:quote(
                            if
                                is_atom(Value) -> atom_to_list(Value);
                                is_boolean(Value) -> atom_to_list(Value);
                                is_integer(Value) -> integer_to_list(Value);
                                true -> Value
                            end),
                        KeyStr ++ "=" ++ ValueStr
                    end,
                    Params
                ),
                "&"
            )
    end.

%% @doc Resolve IPNS names
%% Options can include:
%%   - arg (string): IPNS name to resolve (default: local peerID)
%%   - recursive (boolean): Resolve recursively (default: true)
%%   - nocache (boolean): Bypass cache
%%   - dht_record_count (integer): DHT records to request (default: 16)
%%   - dht_timeout (string): DHT timeout duration (default: "1m0s")
%%   - stream (boolean): Stream results
name_resolve() ->
    name_resolve([]).

name_resolve(Options) when is_list(Options) ->
    try
        Defaults = [
            {arg, undefined},
            {recursive, true},
            {nocache, true},               
            {'dht-record-count', 32},     
            {'dht-timeout', "30s"},       
            {stream, false}
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
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/name/resolve" ++ QueryString,
        
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

%% @doc Pin objects to local storage
%% Options can include:
%%   - recursive (boolean): Recursively pin linked objects (default: true)
%%   - name (string): Optional name for the pin
%%   - progress (boolean): Show progress (default: false)
%% {ok, Result} = ipfs_client_5:pin_add("/ipfs/Qm...", [{name, "my-pin"},{progress, true}]).
pin_add(IPFSPath) -> 
    pin_add(IPFSPath, []).

pin_add(IPFSPath, Options) when is_list(IPFSPath) orelse is_binary(IPFSPath), is_list(Options) ->
    try
        Defaults = [
            {recursive, true},
            {name, undefined},
            {progress, false}
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
            
        QueryString = build_query_string([{arg, IPFSPath}|QueryParams]),
        Url = ?RPC_API ++ "/v0/pin/add" ++ QueryString,
            
        case httpc:request(post, 
                            {Url, [], "application/json", ""},
                            [{timeout, 30000}],
                            [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    Result = #{<<"Pins">> := _} -> 
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

%% @doc List objects pinned to local storage
%% Options can include:
%%   - type (string): Pin type ("direct", "indirect", "recursive", "all") (default: "all")
%%   - quiet (boolean): Output only CIDs (default: false)
%%   - name (string): Filter pins by name (partial match, case-sensitive)
%%   - stream (boolean): Stream results (default: false)
%%   - names (boolean): Include pin names in output (default: false)
%% {ok, Result} = ipfs_client_5:pin_ls([{type, "recursive"},{names, true}]).
pin_ls() ->
    pin_ls([]).


%% {ok, Pins} = ipfs_client_5:pin_ls([{type, "recursive"}, {names, true}]).
pin_ls(Options) when is_list(Options) ->
    try
        Defaults = [
            {arg, undefined},
            {type, "all"},
            {quiet, false},
            {name, undefined},
            {stream, false},
            {names, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false when Key =:= quiet; Key =:= stream; Key =:= names -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/pin/ls" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 60000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Keys">> := _} = Result -> 
                        {ok, Result};
                    #{<<"Cid">> := _} = Result -> 
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

%% @doc Pin object to remote pinning service
%% Required:
%%   - IPFSPath: CID or Path to be pinned
%%   - Service: Name of the remote pinning service
%% Options can include:
%%   - name (string): Optional name for the pin
%%   - background (boolean): Add to queue and return immediately (default: false)
%% Example: 
%% {ok, Result} = ipfs_client_5:pin_remote_add("/ipfs/Qm...", "my-service", [{name, "backup"}]).
pin_remote_add(IPFSPath, Service) ->
    pin_remote_add(IPFSPath, Service, []).

pin_remote_add(IPFSPath, Service, Options) when is_list(IPFSPath) orelse is_binary(IPFSPath), 
                                               is_list(Service) orelse is_binary(Service),
                                               is_list(Options) ->
    try
        Defaults = [
            {name, undefined},
            {background, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false when Key =:= background -> false;
                    _ -> {true, {Key, Value}}
                end
            end, [{service, Service}|MergedOpts]),
        
        QueryString = build_query_string([{arg, IPFSPath}|QueryParams]),
        Url = ?RPC_API ++ "/v0/pin/remote/add" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Cid">> := _, <<"Name">> := _, <<"Status">> := _} = Result ->
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

%% @doc List objects pinned to remote pinning service
%% Options can include:
%%   - service (string): Name of the remote pinning service (mandatory)
%%   - name (string): Filter pins by exact name match
%%   - cid (list): Filter by CIDs (list of strings)
%%   - status (list): Filter by statuses (queued,pinning,pinned,failed)
%% Example:
%% {ok, Result} = ipfs_client_5:pin_remote_ls([{service, "my-service"}, {status, ["pinned", "pinning"]}]).
pin_remote_ls(Service) when is_list(Service) orelse is_binary(Service) ->
    pin_remote_ls(Service, []).

pin_remote_ls(Service, Options) when is_list(Service) orelse is_binary(Service),
                                    is_list(Options) ->
    try
        Defaults = [
            {name, undefined},
            {cid, undefined},
            {status, ["pinned"]}
        ],
        
        case Service of
            "" -> {error, missing_service_name};
            _ ->
                MergedOpts = merge_options(Options, Defaults),
                
                QueryParams = lists:filtermap(
                    fun({Key, Value}) ->
                        case Value of
                            undefined -> false;
                            [] -> false;
                            _ when Key =:= cid; Key =:= status ->
                                ValueList = [to_string(V) || V <- Value],
                                {true, {Key, string:join(ValueList, ",")}};
                            _ ->
                                {true, {Key, Value}}
                        end
                    end, [{service, Service}|MergedOpts]),
                
                QueryString = build_query_string(QueryParams),
                Url = ?RPC_API ++ "/v0/pin/remote/ls" ++ QueryString,
                
                case httpc:request(post, 
                                 {Url, [], "application/json", ""},
                                 [{timeout, 60000}],
                                 [{body_format, binary}]) of
                    {ok, {{_, 200, _}, _, Body}} ->
                        case jiffy:decode(Body, [return_maps]) of
                            #{<<"Cid">> := _, <<"Name">> := _, <<"Status">> := _} = Result -> 
                                {ok, Result};
                            List when is_list(List) -> 
                                {ok, List};
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

%% @doc Remove pins from remote pinning service
%% Required:
%%   - Service: Name of the remote pinning service
%% Options can include:
%%   - name (string): Remove pins with exact name match
%%   - cid (list): Remove pins for specified CIDs
%%   - status (list): Remove pins with specified statuses (default: ["pinned"])
%%   - force (boolean): Allow removal without confirmation (default: false)
%% Example:
%% ok = ipfs_client_5:pin_remote_rm("my-service", [{cid, ["Qm..."]}, {force, true}]).
pin_remote_rm(Service) when is_list(Service) orelse is_binary(Service) ->
    pin_remote_rm(Service, []).

pin_remote_rm(Service, Options) when is_list(Service) orelse is_binary(Service),
                                    is_list(Options) ->
    try
        Defaults = [
            {name, undefined},
            {cid, undefined},
            {status, ["pinned"]},
            {force, false}
        ],
        
        case Service of
            "" -> {error, missing_service_name};
            _ ->
                MergedOpts = merge_options(Options, Defaults),
                
                QueryParams = lists:filtermap(
                    fun({Key, Value}) ->
                        case Value of
                            undefined -> false;
                            [] -> false;
                            false when Key =:= force -> false;
                            _ when Key =:= cid; Key =:= status ->
                                ValueList = [to_string(V) || V <- Value],
                                {true, {Key, string:join(ValueList, ",")}};
                            _ ->
                                {true, {Key, Value}}
                        end
                    end, [{service, Service}|MergedOpts]),
                
                QueryString = build_query_string(QueryParams),
                Url = ?RPC_API ++ "/v0/pin/remote/rm" ++ QueryString,
                
                case httpc:request(post, 
                                 {Url, [], "application/json", ""},
                                 [{timeout, 60000}],
                                 [{body_format, binary}]) of
                    {ok, {{_, 200, _}, _, _Body}} ->
                        ok;
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

%% @doc Add remote pinning service
%% Required:
%%   - Name: Service name
%%   - Endpoint: Service endpoint URL
%%   - Key: Service API key
%% Example:
%% ok = ipfs_client_5:pin_remote_service_add("my-service", "https://example.com", "api-key").
pin_remote_service_add(Name, Endpoint, Key) when (is_list(Name) orelse is_binary(Name)),
                                               (is_list(Endpoint) orelse is_binary(Endpoint)),
                                               (is_list(Key) orelse is_binary(Key)) ->
    try
        Args = [
            {arg, Name},
            {arg, Endpoint},
            {arg, Key}
        ],
        QueryString = build_query_string(Args),
        Url = ?RPC_API ++ "/v0/pin/remote/service/add" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, _Body}} ->
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

%% @doc List remote pinning services
%% Options:
%%   - stat (boolean): Include pin count statistics (default: false)
%% Example:
%% {ok, Services} = ipfs_client_5:pin_remote_service_ls([{stat, true}]).
pin_remote_service_ls() ->
    pin_remote_service_ls([]).

pin_remote_service_ls(Options) when is_list(Options) ->
    try
        Defaults = [
            {stat, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =:= stat -> false;
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/pin/remote/service/ls" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"RemoteServices">> := Services} ->
                        {ok, Services};
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

%% @doc Remove remote pinning service
%% Required:
%%   - Name: Name of service to remove
%% Example:
%% ok = ipfs_client_5:pin_remote_service_rm("my-service").
pin_remote_service_rm(Name) when is_list(Name) orelse is_binary(Name) ->
    try
        QueryString = build_query_string([{arg, Name}]),
        Url = ?RPC_API ++ "/v0/pin/remote/service/rm" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, _Body}} ->
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

%% @doc Remove object from pin-list
%% Required:
%%   - IPFSPath: Path to object(s) to be unpinned
%% Options:
%%   - recursive (boolean): Recursively unpin linked objects (default: true)
%% Example:
%% {ok, #{pins := Pins}} = ipfs_client_5:pin_rm("/ipfs/Qm...", [{recursive, false}]).
pin_rm(IPFSPath) ->
    pin_rm(IPFSPath, []).

pin_rm(IPFSPath, Options) when is_list(IPFSPath) orelse is_binary(IPFSPath), is_list(Options) ->
    try
        Defaults = [
            {recursive, true}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =:= recursive -> {true, {Key, false}};
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, IPFSPath}|QueryParams]),
        Url = ?RPC_API ++ "/v0/pin/rm" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Pins">> := Pins} ->
                        {ok, #{pins => Pins}};
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

%% @doc Update a recursive pin
%% Required:
%%   - FromPath: Path to old pinned object
%%   - ToPath: Path to new object to pin
%% Options:
%%   - unpin (boolean): Remove the old pin (default: true)
%% Example:
%% {ok, #{pins := Pins}} = ipfs_client_5:pin_update("/ipfs/QmOld", "/ipfs/QmNew", [{unpin, false}]).
pin_update(FromPath, ToPath) ->
    pin_update(FromPath, ToPath, []).

pin_update(FromPath, ToPath, Options) when (is_list(FromPath) orelse is_binary(FromPath)),
                                         (is_list(ToPath) orelse is_binary(ToPath)),
                                         is_list(Options) ->
    try
        Defaults = [
            {unpin, true}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =:= unpin -> {true, {Key, false}};
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, FromPath}, {arg, ToPath}|QueryParams]),
        Url = ?RPC_API ++ "/v0/pin/update" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Pins">> := Pins} ->
                        {ok, #{pins => Pins}};
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

%% @doc Verify that recursive pins are complete
%% Options:
%%   - verbose (boolean): Include hashes of non-broken pins (default: false)
%%   - quiet (boolean): Only show hashes of broken pins (default: false)
%% Example:
%% {ok, VerificationResults} = ipfs_client_5:pin_verify([{verbose, true}]).
pin_verify() ->
    pin_verify([]).

pin_verify(Options) when is_list(Options) ->
    try
        Defaults = [
            {verbose, false},
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
        Url = ?RPC_API ++ "/v0/pin/verify" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 120000}],  
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

parse_ndjson(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    lists:filtermap(
        fun(Line) ->
            try
                {true, jiffy:decode(Line, [return_maps])}
            catch
                _:_ -> false
            end
        end,
        Lines).

%% @doc Send echo request packets to IPFS hosts
%% Required:
%%   - PeerID: ID of peer to ping
%% Options:
%%   - count (integer): Number of pings to send (default: 10)
%% Example:
%% {ok, PingResults} = ipfs_client_5:ping("QmPeerID", [{count, 5}]).
ping(PeerID) ->
    ping(PeerID, []).

ping(PeerID, Options) when is_list(PeerID) orelse is_binary(PeerID), is_list(Options) ->
    try
        Defaults = [
            {count, 10},
            {debug, false}  
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        Debug = proplists:get_value(debug, MergedOpts, false),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Key of
                    debug -> false;  
                    _ -> 
                        case Value of
                            undefined -> false;
                            _ -> {true, {Key, Value}}
                        end
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, PeerID}|QueryParams]),
        Url = ?RPC_API ++ "/v0/ping" ++ QueryString,
        
        case Debug of
            true -> io:format("DEBUG: Calling URL: ~s~n", [Url]);
            _ -> ok
        end,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case Debug of
                    true -> io:format("DEBUG: Raw response: ~p~n", [Body]);
                    _ -> ok
                end,
                Results = parse_ping_response(Body),
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

parse_ping_response(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    lists:filtermap(
        fun(Line) ->
            try
                case jiffy:decode(Line, [return_maps]) of
                    #{<<"Text">> := _} = Result -> 
                        {true, Result};
                    _ -> false
                end
            catch
                _:_ -> false
            end
        end,
        Lines).


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
