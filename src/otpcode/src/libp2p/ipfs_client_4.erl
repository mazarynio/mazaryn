-module(ipfs_client_4).
-author("Zaryn Technologies").
-export([get/1, get/2, id/0, id/1]).

-define(RPC_API, "http://localhost:5001/api").

%% @doc Download IPFS objects
%% Options can include:
%%   - output (string): Output file/directory path
%%   - archive (boolean): Output as TAR archive
%%   - compress (boolean): Use GZIP compression
%%   - compression_level (integer): Compression level (1-9)
%%   - progress (boolean): Show progress (default: true)
get(IPFSPath) ->
    get(IPFSPath, []).

get(IPFSPath, Options) when is_list(IPFSPath) orelse is_binary(IPFSPath), is_list(Options) ->
    try
        Defaults = [
            {output, undefined},
            {archive, false},
            {compress, false},
            {'compression-level', undefined},
            {progress, true}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false when Key =/= progress -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, IPFSPath}|QueryParams]),
        Url = ?RPC_API ++ "/v0/get" ++ QueryString,
        
        OutputPath = case proplists:get_value(output, Options) of
            undefined -> 
                binary_to_list(iolist_to_binary(["./", filename:basename(IPFSPath)]));
            Path -> 
                Path
        end,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [
                             {body_format, binary},
                             {stream, OutputPath}
                         ]) of
            {ok, saved_to_file} ->
                {ok, OutputPath};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Show IPFS node ID info
%% Options can include:
%%   - arg (string): Peer.ID to look up
%%   - format (string): Output format
%%   - peerid_base (string): Peer ID encoding (b58mh|base36|k|base32|...)
id() ->
    id([]).

id(Options) when is_list(Options) ->
    try
        Defaults = [
            {arg, undefined},
            {format, undefined},
            {'peerid-base', undefined}
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
        Url = ?RPC_API ++ "/v0/id" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, jsx:decode(Body, [return_maps])};
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