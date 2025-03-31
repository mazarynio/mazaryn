-module(ipfs_client_2).
-author("Zaryn Technologies").
-export([merge_options/2, cid_hashes/0, cid_hashes/1, cid_hashes/2, commands/0, commands/1, config_get/1, config_set/2, config_set/3, config_profile_apply/1,
config_profile_apply/2, config_replace/1, config_show/0, dag_export/1, dag_export/2, dag_get/1, dag_get/2, dag_import/2, dag_put/1, dag_put/2, dag_resolve/1,
dag_stat/1, dag_stat/2]).

-define(RPC_API, "http://localhost:5001/api").

%% List available multihashes
cid_hashes() ->
    cid_hashes(false, false).

%% {ok, FilteredHashes} = ipfs_client_2:cid_hashes([{numeric, true}, {supported, true}]).
cid_hashes(Options) when is_list(Options) ->
    Numeric = proplists:get_value(numeric, Options, false),
    Supported = proplists:get_value(supported, Options, false),
    cid_hashes(Numeric, Supported).

cid_hashes(Numeric, Supported) ->
    try
        QueryString = build_query_string([
            {numeric, Numeric},
            {supported, Supported}
        ]),
        Url = ?RPC_API ++ "/v0/cid/hashes" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, jsx:decode(Body)};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% List all available commands
commands() ->
    commands(false).

%% {ok, DetailedCommands} = ipfs_client_2:commands(true).
commands(Flags) when is_boolean(Flags) ->
    try
        QueryString = build_query_string([{flags, Flags}]),
        Url = ?RPC_API ++ "/v0/commands" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, jsx:decode(Body)};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% Get config value
%% {ok, Key, Value} = ipfs_client_2:config_get("Addresses.API").
config_get(Key) when is_list(Key) orelse is_binary(Key) ->
    try
        QueryString = build_query_string([{arg, Key}]),
        Url = ?RPC_API ++ "/v0/config" ++ QueryString,
        
        case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Key">> := K, <<"Value">> := V} -> {ok, K, V};
                    Other -> {error, {unexpected_response, Other}}
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

%% Set config value
%% {ok, Key, NewValue} = ipfs_client_2:config_set("Datastore.StorageMax", "10GB").
%% {ok, Key, NewValue} = ipfs_client:config_set("Discovery.MDNS.Enabled", "true", [{bool, true}]).
config_set(Key, Value) ->
    config_set(Key, Value, []).

config_set(Key, Value, Options) when is_list(Key) orelse is_binary(Key) ->
    try
        BaseParams = [{arg, Key}, {arg, Value}],
        Params = case {proplists:get_value(bool, Options, false),
                      proplists:get_value(json, Options, false)} of
            {true, _} -> [{bool, true} | BaseParams];
            {_, true} -> [{json, true} | BaseParams];
            _ -> BaseParams
        end,
        
        QueryString = build_query_string(Params),
        Url = ?RPC_API ++ "/v0/config" ++ QueryString,
        
        case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Key">> := K, <<"Value">> := V} -> {ok, K, V};
                    Other -> {error, {unexpected_response, Other}}
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

%% Apply configuration profile
%% {ok, OldConfig, NewConfig} = ipfs_client_2:config_profile_apply("server").
config_profile_apply(Profile) ->
    config_profile_apply(Profile, false).

%% {ok, OldConfig, WouldBeNewConfig} = ipfs_client_2:config_profile_apply("lowpower", true).
config_profile_apply(Profile, DryRun) when is_list(Profile) orelse is_binary(Profile) ->
    try
        QueryString = build_query_string([
            {arg, Profile},
            {'dry-run', DryRun}
        ]),
        Url = ?RPC_API ++ "/v0/config/profile/apply" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"OldCfg">> := OldCfg, <<"NewCfg">> := NewCfg} ->
                        {ok, OldCfg, NewCfg};
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

%% Replace entire config with new file
%% ipfs_client_2:config_replace("rebar.json")
config_replace(FilePath) ->
    try
        {ok, FileData} = file:read_file(FilePath),
        
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        Filename = filename:basename(FilePath),
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"", Filename, "\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            FileData, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        Url = ?RPC_API ++ "/v0/config/replace",
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
                         [],
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

%% Show complete config
config_show() ->
    try
        Url = ?RPC_API ++ "/v0/config/show",
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, jsx:decode(Body)};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% Export DAG as CAR file
dag_export(RootCID) ->
    dag_export(RootCID, true).  

%% ipfs_client:dag_export("QmRgo4DFH373E9yKWrf6G8DnxyoT1oAN2zZxdd5QgS9pJb", false).
dag_export(RootCID, Progress) when is_boolean(Progress) ->
    try
        QueryString = build_query_string([
            {arg, RootCID},
            {progress, Progress}
        ]),
        Url = ?RPC_API ++ "/v0/dag/export" ++ QueryString,
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [
                             {body_format, binary}, 
                             {stream, "./export.car"}
                         ]) of
            {ok, saved_to_file} ->
                {ok, "./export.car"};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% Get DAG node with default output codec (dag-json)
%% {ok, DagJson} = ipfs_client_2:dag_get("bafybeigdyrzt5sfp7udm7hu76uh7y26nf3efuylqabf3oclgtqy55fbzdi").
dag_get(Ref) ->
    dag_get(Ref, "dag-json").

%% Get DAG node with custom output codec
%% {ok, DagCbor} = ipfs_client_2:dag_get(CID, "dag-cbor").
dag_get(Ref, OutputCodec) when is_list(Ref) orelse is_binary(Ref) ->
    try
        QueryString = build_query_string([
            {arg, Ref},
            {'output-codec', OutputCodec}
        ]),
        Url = ?RPC_API ++ "/v0/dag/get" ++ QueryString,
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, Headers, Body}} ->
                case proplists:get_value("content-type", Headers) of
                    "text/plain" ++ _ -> 
                        {ok, Body};
                    _ -> 
                        {error, {unexpected_content_type, Headers}}
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

%% Import CAR file with custom options
%% {ok, Root, Stats} = ipfs_client:dag_import("/path/to/file.car").
%% {ok, Root, Stats} = ipfs_client:dag_import("/path/to/file.car", [{<<"pin-roots">>, false}, {<<"stats">>, true}]).
dag_import(FilePath, Options) when is_list(FilePath) orelse is_binary(FilePath) ->
    try
        %% Default options
        Defaults = [
            {<<"pin-roots">>, true},
            {<<"silent">>, false},
            {<<"stats">>, false},
            {<<"allow-big-block">>, false}
        ],
        MergedOpts = merge_options(Defaults, Options),
        
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        {ok, FileData} = file:read_file(FilePath),
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"", 
            filename:basename(FilePath), "\"\r\n",
            "Content-Type: application/vnd.ipld.car\r\n\r\n",
            FileData, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        QueryString = build_query_string(MergedOpts),
        Url = ?RPC_API ++ "/v0/dag/import" ++ QueryString,
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Root">> := Root} = Response ->
                        {ok, Root, maps:get(<<"Stats">>, Response, #{})};
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

%% Add DAG node with default options
%% Data = jsx:encode(#{<<"key">> => <<"value">>}).
%% {ok, CID} = ipfs_client_2:dag_put(Data).
%% {ok, FileData} = file:read_file("data.json").
%% {ok, CID} = ipfs_client:dag_put(FileData).
dag_put(Data) when is_binary(Data) ->
    dag_put(Data, []).

%% Add DAG node with custom options
dag_put(Data, Options) when is_binary(Data) ->
    try
        Defaults = [
            {<<"store-codec">>, <<"dag-cbor">>},
            {<<"input-codec">>, <<"dag-json">>},
            {<<"pin">>, false},
            {<<"allow-big-block">>, false}
        ],
        MergedOpts = merge_options(Defaults, Options),
        
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"data\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            Data, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        QueryString = build_query_string(MergedOpts),
        Url = ?RPC_API ++ "/v0/dag/put" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Cid">> := #{<<"/">> := CID}} ->
                        {ok, CID};
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

%% Resolve IPLD path to CID
%% {ok, CID, RemPath} = ipfs_client:dag_resolve("/ipfs/QmExample/some/path").
dag_resolve(Path) when is_list(Path) orelse is_binary(Path) ->
    try
        EncodedPath = uri_string:quote(Path),
        Url = ?RPC_API ++ "/v0/dag/resolve?arg=" ++ EncodedPath,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Cid">> := #{<<"/">> := CID}, <<"RemPath">> := RemPath} ->
                        {ok, CID, RemPath};
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

%% {ok, DagStats, Summary} = ipfs_client:dag_stat("bafybeigdyrzt5sfp7udm7hu76uh7y26nf3efuylqabf3oclgtqy55fbzdi").
dag_stat(RootCID) ->
    dag_stat(RootCID, true).

%% Get DAG stats with progress control
%% {ok, DagStats, Summary} = ipfs_client:dag_stat(CID, false).
dag_stat(RootCID, Progress) when is_list(RootCID) orelse is_binary(RootCID) ->
    try
        QueryString = build_query_string([
            {arg, RootCID},
            {progress, Progress}
        ]),
        Url = ?RPC_API ++ "/v0/dag/stat" ++ QueryString,
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, Headers, Body}} ->
                case proplists:get_value("content-type", Headers) of
                    "application/json" ++ _ ->
                        try jsx:decode(Body) of
                            #{<<"DagStats">> := DagStats} = FullResponse ->
                                {ok, DagStats, maps:remove(<<"DagStats">>, FullResponse)};
                            Other ->
                                {error, {unexpected_format, Other}}
                        catch
                            error:_ ->
                                {error, {invalid_json, Body}}
                        end;
                    ContentType ->
                        {error, {unexpected_content_type, ContentType}}
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