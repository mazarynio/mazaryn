-module(ipfs_client_3).
-author("Zaryn Technologies").
-export([files_chcid/0, files_chcid/1, files_chcid/2, files_cp/2, files_cp/3, files_flush/0, files_flush/1, files_ls/0, files_ls/1, files_mkdir/1,
files_mkdir/2, files_mv/2, files_read/1, files_read/2, files_rm/1, files_rm/2, files_stat/1, files_stat/2, files_write/2, files_write/3, filestore_dups/0,
filestore_ls/0, filestore_ls/1, filestore_verify/0, filestore_verify/1]).

-define(RPC_API, "http://localhost:5001/api").

%% ok = ipfs_client_3:files_chcid().
%% ok = ipfs_client_3:files_chcid("/my-path").
%% ok = ipfs_client_3:files_chcid("/my-path", [{cid_version, 1}]).
%% ok = ipfs_client_3:files_chcid("/my-path", [{hash, "sha3-256"}]).
files_chcid() ->
    files_chcid("/", []).

files_chcid(Path) when is_list(Path) orelse is_binary(Path) ->
    files_chcid(Path, []);
files_chcid(Options) when is_list(Options) ->
    files_chcid("/", Options).

files_chcid(Path, Options) when is_list(Path) orelse is_binary(Path), is_list(Options) ->
    try
        Defaults = [
            {arg, Path},
            {'cid-version', undefined},
            {hash, undefined}
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
        Url = ?RPC_API ++ "/v0/files/chcid" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
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

%% @doc Copy files/directories within MFS or from IPFS to MFS
%% Examples:
%% ok = ipfs_client_2:files_cp("/ipfs/QmSource", "/mfs-destination").
%% ok = ipfs_client_2:files_cp("/mfs-source", "/mfs-destination", [{parents, true}]).
files_cp(Source, Dest) ->
    files_cp(Source, Dest, []).

files_cp(Source, Dest, Options) when (is_list(Source) orelse is_binary(Source)) and 
                                    (is_list(Dest) orelse is_binary(Dest)) ->
    try
        Parents = proplists:get_value(parents, Options, false),
        QueryString = build_query_string([
            {arg, Source},
            {arg, Dest},
            {parents, Parents}
        ]),
        Url = ?RPC_API ++ "/v0/files/cp" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
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

%% @doc Flush path's data to disk
%% Examples:
%% {ok, Cid} = ipfs_client_2:files_flush().
%% {ok, Cid} = ipfs_client_2:files_flush("/my-path").
files_flush() ->
    files_flush("/").

files_flush(Path) when is_list(Path) orelse is_binary(Path) ->
    try
        QueryString = build_query_string([{arg, Path}]),
        Url = ?RPC_API ++ "/v0/files/flush" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body) of
                    #{<<"Cid">> := Cid} -> {ok, Cid};
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

%% @doc List directories in the local mutable namespace (MFS)
%% Options can include:
%%   - arg (string): Path to list (default: "/")
%%   - long (boolean): Use long listing format
%%   - U (boolean): Unsorted (directory order)
%% {ok, Listing} = ipfs_client_2:files_ls().
%% {ok, Listing} = ipfs_client_2:files_ls("/my-path").
%% {ok, DetailedListing} = ipfs_client_2:files_ls([{long, true}]).
files_ls() ->
    files_ls([]).

files_ls(Options) when is_list(Options) ->
    try
        Defaults = [
            {arg, "/"},
            {long, false},
            {'U', false},
            {timeout, 30000}  % 30 second timeout
        ],
        MergedOpts = merge_options_v2(Defaults, Options),
        
        HttpOptions = [{timeout, proplists:get_value(timeout, MergedOpts)}],
        
        QueryString = build_query_string_v2([
            {arg, proplists:get_value(arg, MergedOpts)},
            {long, proplists:get_value(long, MergedOpts)},
            {'U', proplists:get_value('U', MergedOpts)}
        ]),
        
        Url = ?RPC_API ++ "/v0/files/ls" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         HttpOptions,  
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body) of
                    #{<<"Entries">> := Entries} -> {ok, Entries};
                    Other -> {error, {unexpected_response, Other}}
                end;
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, {timeout, _}} ->
                {error, timeout};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Error ->
            {error, Error}
    end;

files_ls(Path) when is_list(Path), is_integer(hd(Path)) ->
    files_ls([{arg, Path}]);

files_ls(Path) when is_binary(Path) ->
    files_ls([{arg, binary_to_list(Path)}]).

%% ok = ipfs_client_2:files_mkdir("/new-dir").
%% ok = ipfs_client_2:files_mkdir("/path/to/new-dir", [{parents, true}]).
files_mkdir(Path) ->
    files_mkdir(Path, []).

%% ok = ipfs_client_2:files_mkdir("/path/to/new-dir", [{parents, true},{hash, "sha3-256"}]).
files_mkdir(Path, Options) when is_list(Path) orelse is_binary(Path), is_list(Options) ->
    try
        Defaults = [
            {parents, false},
            {'cid-version', undefined},
            {hash, undefined}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, Path}|QueryParams]),
        Url = ?RPC_API ++ "/v0/files/mkdir" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
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

%% @doc Move files/directories in MFS
%% ok = ipfs_client_2:files_mv("/source", "/destination").
files_mv(Source, Dest) when (is_list(Source) orelse is_binary(Source)) and 
                           (is_list(Dest) orelse is_binary(Dest)) ->
    try
        QueryString = build_query_string([
            {arg, Source},
            {arg, Dest}
        ]),
        Url = ?RPC_API ++ "/v0/files/mv" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
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

%% @doc Read a file from MFS
%% Options can include:
%%   - offset (integer): Byte offset to begin reading from
%%   - count (integer): Maximum number of bytes to read
%% {ok, FileContents} = ipfs_client_3:files_read("/path/to/file").
%% {ok, PartialContents} = ipfs_client_3:files_read("/file", [{offset, 10}, {count, 100}]).
files_read(Path) ->
    files_read(Path, []).

files_read(Path, Options) when is_list(Path) orelse is_binary(Path), is_list(Options) ->
    try
        Offset = proplists:get_value(offset, Options, undefined),
        Count = proplists:get_value(count, Options, undefined),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    _ -> {true, {Key, Value}}
                end
            end, [{offset, Offset}, {count, Count}]),
        
        QueryString = build_query_string([{arg, Path}|QueryParams]),
        Url = ?RPC_API ++ "/v0/files/read" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, Body};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Remove a file or directory from MFS
%% Options can include:
%%   - recursive (boolean): Recursively remove directories
%%   - force (boolean): Force removal
%% ok = ipfs_client_3:files_rm("/path/to/file").
%% ok = ipfs_client_3:files_rm("/directory", [{recursive, true}]).
%% ok = ipfs_client_3:files_rm("/locked/file", [{force, true}]).
files_rm(Path) ->
    files_rm(Path, []).

files_rm(Path, Options) when is_list(Path) orelse is_binary(Path), is_list(Options) ->
    try
        Recursive = proplists:get_value(recursive, Options, false),
        Force = proplists:get_value(force, Options, false),
        
        QueryString = build_query_string([
            {arg, Path},
            {recursive, Recursive},
            {force, Force}
        ]),
        Url = ?RPC_API ++ "/v0/files/rm" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
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

%% @doc Display file/directory status in MFS
%% Options can include:
%%   - format (string): Custom format string
%%   - hash (boolean): Return only hash
%%   - size (boolean): Return only cumulative size
%%   - with_local (boolean): Include locality information
%% Returns:
%%   - {ok, StatInfo} where StatInfo is a map with status information
%%   - {error, Reason} on failure
files_stat(Path) ->
    files_stat(Path, []).

files_stat(Path, Options) when is_list(Path) orelse is_binary(Path), is_list(Options) ->
    try
        Defaults = [
            {format, undefined},
            {hash, false},
            {size, false},
            {'with-local', false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        FinalOpts = case {proplists:get_value(hash, MergedOpts),
                         proplists:get_value(size, MergedOpts)} of
                        {true, _} -> 
                            [{hash, true}, {format, undefined}, {size, false}|MergedOpts];
                        {_, true} -> 
                            [{size, true}, {format, undefined}, {hash, false}|MergedOpts];
                        _ -> 
                            MergedOpts
                    end,
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false -> false;
                    true -> {true, {Key, true}};
                    _ -> {true, {Key, Value}}
                end
            end, FinalOpts),
        
        QueryString = build_query_string([{arg, Path}|QueryParams]),
        Url = ?RPC_API ++ "/v0/files/stat" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, jiffy:decode(Body, [return_maps])};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Write to a file in MFS
%% Options can include:
%%   - offset (integer): Byte offset to begin writing
%%   - create (boolean): Create file if not exists
%%   - parents (boolean): Create parent directories
%%   - truncate (boolean): Truncate file before writing
%%   - count (integer): Max bytes to write
%%   - raw_leaves (boolean): Use raw blocks for leaves
%%   - cid_version (integer): CID version to use
%%   - hash (string): Hash function to use
%% Returns:
%%   - ok on success
%%   - {error, Reason} on failure
files_write(Path, Data) ->
    files_write(Path, Data, []).

files_write(Path, Data, Options) when (is_list(Path) orelse is_binary(Path)) and 
                                    (is_binary(Data) orelse is_list(Data)), is_list(Options) ->
    try
        Defaults = [
            {offset, undefined},
            {create, false},
            {parents, false},
            {truncate, false},
            {count, undefined},
            {'raw-leaves', false},
            {'cid-version', undefined},
            {hash, undefined}
        ],
        
        FixedOptions = lists:map(
            fun
                ({cid_version, Value}) -> {'cid-version', Value};
                ({raw_leaves, Value}) -> {'raw-leaves', Value};
                (Other) -> Other
            end, Options),
        
        MergedOpts = merge_options_v3(FixedOptions, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false -> false;
                    true -> {true, {atom_to_string(Key), true}};
                    _ -> {true, {atom_to_string(Key), Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string_v3([{arg, Path}|QueryParams]),
        Url = ?RPC_API ++ "/v0/files/write" ++ QueryString,
        
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"data\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            Data, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
                         [],
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

atom_to_string(Atom) when is_atom(Atom) ->
    AtomStr = atom_to_list(Atom),
    re:replace(AtomStr, "-", "\\-", [global, {return, list}]);
atom_to_string(Other) ->
    Other.

%% @doc List blocks that exist in both filestore and standard block storage
%% Returns:
%%   - {ok, Results} where Results is a list of #{<<"Err">> => ..., <<"Ref">> => ...}
%%   - {error, Reason} on failure
filestore_dups() ->
    try
        Url = ?RPC_API ++ "/v0/filestore/dups",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],  
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, jiffy:decode(Body, [return_maps])};
            {ok, {{_, 500, _}, _, Body}} ->
                case jiffy:decode(Body) of
                    #{<<"Message">> := <<"filestore is not enabled", _/binary>>} ->
                        {error, filestore_disabled};
                    _ ->
                        {error, {status_code, 500, Body}}
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

%% @doc List objects in filestore
%% Options can include:
%%   - arg (string): CID of objects to list
%%   - file_order (boolean): Sort by backing file path
%% Returns:
%%   - {ok, Results} where Results is a list of filestore objects
%%   - {error, Reason} on failure
filestore_ls() ->
    filestore_ls([]).

filestore_ls(Options) when is_list(Options) ->
    try
        Defaults = [
            {arg, undefined},
            {'file-order', false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/filestore/ls" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, jiffy:decode(Body, [return_maps])};
            {ok, {{_, StatusCode, _}, _, Body}} ->
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error ->
            error 
    end.

%% @doc Verify objects in filestore
%% Options can include:
%%   - arg (string): CID of objects to verify
%%   - file_order (boolean): Verify in backing file order
%% Returns:
%%   - {ok, Results} where Results is a list of verification objects
%%   - {error, Reason} on failure
filestore_verify() ->
    filestore_verify([]).

filestore_verify(Options) when is_list(Options) ->
    try
        Defaults = [
            {arg, undefined},
            {'file-order', false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/filestore/verify" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, jiffy:decode(Body, [return_maps])};
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

merge_options_v2(Defaults, Options) ->
    ValidOptions = case is_list(Options) of
        true -> 
            [Option || Option <- Options, is_tuple(Option), tuple_size(Option) >= 2];
        false -> 
            []
    end,
    
    lists:ukeymerge(1, 
        lists:ukeysort(1, ValidOptions), 
        lists:ukeysort(1, Defaults)).

build_query_string_v2(Params) ->
    QueryParts = lists:map(
        fun({Key, Value}) when is_atom(Key) ->
            KeyStr = atom_to_list(Key),
            case Value of
                true -> KeyStr ++ "=true";
                false -> KeyStr ++ "=false";
                V when is_list(V) -> KeyStr ++ "=" ++ uri_string:quote(V);
                V when is_binary(V) -> KeyStr ++ "=" ++ uri_string:quote(binary_to_list(V));
                V when is_integer(V) -> KeyStr ++ "=" ++ integer_to_list(V);
                _ -> KeyStr
            end
        end, 
        Params),
    case QueryParts of
        [] -> "";
        _ -> "?" ++ string:join(QueryParts, "&")
    end.

merge_options_v3(Options, Defaults) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            lists:keystore(Key, 1, Acc, {Key, Value})
        end, Defaults, Options).

build_query_string_v3(Params) ->
    QueryParts = lists:map(
        fun
            ({Key, true}) ->
                io_lib:format("~s=true", [Key]);
            ({Key, Value}) when is_integer(Value) ->
                io_lib:format("~s=~B", [Key, Value]);
            ({Key, Value}) when is_list(Value) ->
                io_lib:format("~s=~s", [Key, uri_string:quote(Value)]);
            ({Key, Value}) when is_binary(Value) ->
                io_lib:format("~s=~s", [Key, uri_string:quote(binary_to_list(Value))]);
            ({Key, Value}) ->
                io_lib:format("~s=~s", [Key, Value])
        end, Params),
    "?" ++ string:join(QueryParts, "&").