-module(ipfs_client_4).
-author("Zaryn Technologies").
-export([get/1, get/2, id/0, id/1, key_gen/1, key_gen/2, key_import/2, key_import/3, key_list/0, key_list/1, key_rename/2, key_rename/3, key_rm/1, key_rm/2,
log_level/2, log_level/3, log_ls/0, ls/1, ls/2, multibase_decode/1, multibase_decode/2, multibase_encode/1, multibase_encode/2, multibase_list/0,
multibase_list/1, multibase_transcode/1, multibase_transcode/2]).

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

%% @doc Create a new keypair
%% Options can include:
%%   - type (string): Key type (rsa, ed25519)
%%   - size (integer): Key size
%%   - ipns_base (string): Key encoding (b58mh|base36|k|base32|...)
%% ipfs_client_4:key_gen("mykey").
key_gen(Name) ->
    key_gen(Name, []).

%% {ok, KeyInfo} = ipfs_client_4:key_gen("rsa-key", [{type, "rsa"},{size, 4096},{'ipns-base', "base58btc"}]).
key_gen(Name, Options) when is_list(Name) orelse is_binary(Name), is_list(Options) ->
    try
        Defaults = [
            {type, "ed25519"},
            {size, undefined},
            {'ipns-base', "base36"}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    undefined -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, Name}|QueryParams]),
        Url = ?RPC_API ++ "/v0/key/gen" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Id">> := Id, <<"Name">> := KeyName} ->
                        {ok, #{id => Id, name => KeyName}};
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

%% @doc Import a key from file
%% Options can include:
%%   - ipns_base (string): Key encoding (b58mh|base36|k|base32|...)
%%   - format (string): Import format (libp2p-protobuf-cleartext|pem-pkcs8-cleartext)
%%   - allow_any_key_type (boolean): Allow any key type
%% {ok, ImportedKey} = ipfs_client_4:key_import("imported", "/path/to/key.file").
key_import(Name, KeyFile) ->
    key_import(Name, KeyFile, []).

%% {ok, Key} = ipfs_client_4:key_import("special", "/keys/special.pem", [{format, "pem-pkcs8-cleartext"},{'allow-any-key-type', true}]).
key_import(Name, KeyFile, Options) when is_list(Name) orelse is_binary(Name), 
                                       is_list(KeyFile) orelse is_binary(KeyFile), 
                                       is_list(Options) ->
    try
        Defaults = [
            {'ipns-base', "base36"},
            {format, "libp2p-protobuf-cleartext"},
            {'allow-any-key-type', false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =/= 'allow-any-key-type' -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, Name}|QueryParams]),
        Url = ?RPC_API ++ "/v0/key/import" ++ QueryString,
        
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        {ok, KeyData} = file:read_file(KeyFile),
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"key\"; filename=\"", 
            filename:basename(KeyFile), "\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            KeyData, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Id">> := Id, <<"Name">> := KeyName} ->
                        {ok, #{id => Id, name => KeyName}};
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

%% @doc List all local keypairs
%% Options can include:
%%   - l (boolean): Show extra key information
%%   - ipns_base (string): Key encoding (b58mh|base36|k|base32|...)
key_list() ->
    key_list([]).

%% {ok, DetailedKeys} = ipfs_client_4:key_list([{l, true}]).
key_list(Options) when is_list(Options) ->
    try
        Defaults = [
            {l, false},
            {'ipns-base', "base36"}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =/= l -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string(QueryParams),
        Url = ?RPC_API ++ "/v0/key/list" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Keys">> := Keys} ->
                        {ok, Keys};
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

%% @doc Rename a keypair
%% Options can include:
%%   - force (boolean): Overwrite existing key
%%   - ipns_base (string): Key encoding (b58mh|base36|k|base32|...)
%% ipfs_client_4:key_rename("old-name", "new-name").
key_rename(OldName, NewName) ->
    key_rename(OldName, NewName, []).

%% {ok, RenameInfo} = ipfs_client_3:key_rename("temp", "main", [{force, true}]).
key_rename(OldName, NewName, Options) when (is_list(OldName) orelse is_binary(OldName)) and 
                                          (is_list(NewName) orelse is_binary(NewName)), 
                                          is_list(Options) ->
    try
        Defaults = [
            {force, false},
            {'ipns-base', "base36"}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =/= force -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, OldName}, {arg, NewName}|QueryParams]),
        Url = ?RPC_API ++ "/v0/key/rename" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Id">> := Id, <<"Was">> := Was, <<"Now">> := Now, <<"Overwrite">> := Overwrite} ->
                        {ok, #{id => Id, was => Was, now => Now, overwrite => Overwrite}};
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

%% @doc Remove a keypair
%% Options can include:
%%   - l (boolean): Show extra key information
%%   - ipns_base (string): Key encoding (b58mh|base36|k|base32|...)
key_rm(Name) ->
    key_rm(Name, []).

%% {ok, Removed} = ipfs_client_4:key_rm("mykey", [{l, true}]).
key_rm(Name, Options) when is_list(Name) orelse is_binary(Name), is_list(Options) ->
    try
        Defaults = [
            {l, false},
            {'ipns-base', "base36"}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =/= l -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, Name}|QueryParams]),
        Url = ?RPC_API ++ "/v0/key/rm" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Keys">> := Keys} ->
                        {ok, Keys};
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

%% @doc Change logging level
%% Options can include:
%%   - timeout (integer): Operation timeout in milliseconds
%% ipfs_client_4:log_level("all", "debug").
log_level(Subsystem, Level) ->
    log_level(Subsystem, Level, []).

%% ipfs_client_4:log_level("all", "info", [{timeout, 10000}]).
log_level(Subsystem, Level, Options) when is_list(Subsystem) orelse is_binary(Subsystem),
                                        is_list(Level) orelse is_binary(Level),
                                        is_list(Options) ->
    try
        ValidLevels = ["debug", "info", "warn", "error", "dpanic", "panic", "fatal"],
        case lists:member(Level, ValidLevels) of
            false ->
                {error, invalid_log_level};
            true ->
                Timeout = proplists:get_value(timeout, Options, 5000),
                QueryString = build_query_string([{arg, Subsystem}, {arg, Level}]),
                Url = ?RPC_API ++ "/v0/log/level" ++ QueryString,
                
                case httpc:request(post, 
                                 {Url, [], "application/json", ""},
                                 [{timeout, Timeout}],
                                 [{body_format, binary}]) of
                    {ok, {{_, 200, _}, _, Body}} ->
                        case jiffy:decode(Body, [return_maps]) of
                            #{<<"Message">> := Msg} ->
                                {ok, Msg};
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

%% @doc List available logging subsystems
log_ls() ->
    try
        Url = ?RPC_API ++ "/v0/log/ls",
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 5000}],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Strings">> := Subsystems} ->
                        {ok, Subsystems};
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

%% @doc List IPFS directory contents
%% Options can include:
%%   - headers (boolean): Show table headers
%%   - resolve_type (boolean): Resolve link types (default: true)
%%   - size (boolean): Show sizes (default: true)
%%   - stream (boolean): Stream results
%% {ok, Contents} = ipfs_client_4:ls("/ipfs/Qm...").
ls(IPFSPath) ->
    ls(IPFSPath, []).

%% {ok, Detailed} = ipfs_client_4:ls("/ipns/example.com", [{headers, true},{size, false}]).
ls(IPFSPath, Options) when is_list(IPFSPath) orelse is_binary(IPFSPath), is_list(Options) ->
    try
        Defaults = [
            {headers, false},
            {'resolve-type', true},
            {size, true},
            {stream, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryParams = lists:filtermap(
            fun({Key, Value}) ->
                case Value of
                    false when Key =:= 'resolve-type' -> {true, {Key, false}};
                    false when Key =:= size -> {true, {Key, false}};
                    false -> false;
                    _ -> {true, {Key, Value}}
                end
            end, MergedOpts),
        
        QueryString = build_query_string([{arg, IPFSPath}|QueryParams]),
        Url = ?RPC_API ++ "/v0/ls" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [{timeout, 30000}],  
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jiffy:decode(Body, [return_maps]) of
                    #{<<"Objects">> := Objects} ->
                        {ok, Objects};
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

%% @doc Decode multibase-encoded data
%% Options can include:
%%   - timeout (integer): Operation timeout in milliseconds
%% {ok, Decoded} = ipfs_client_4:multibase_decode("/path/to/encoded.file").
%% ipfs_client_4:multibase_decode(<<"uaGVsbG8gd29ybGQ">>)
multibase_decode(Data) ->
    multibase_decode(Data, []).

multibase_decode(Data, Options) when is_binary(Data) ->
    try
        Timeout = proplists:get_value(timeout, Options, 5000),
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"encoded_file\"; filename=\"data\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            Data, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        Url = ?RPC_API ++ "/v0/multibase/decode",
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
                         [{timeout, Timeout}],
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
    end;

multibase_decode(FilePath, Options) when is_list(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Data} ->
            multibase_decode(Data, Options);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.
%% @doc Encode data into multibase format
%% Options can include:
%%   - b (string): Multibase encoding (default: "base64url")
%%   - timeout (integer): Operation timeout in milliseconds
%% {ok, Encoded} = ipfs_client_4:multibase_encode("/tmp/data.txt", [{b, "base58btc"},{timeout, 8000}]),
multibase_encode(Data) ->
    multibase_encode(Data, []).

%% Handle binary data input
multibase_encode(Data, Options) when is_binary(Data) ->
    try
        Defaults = [{b, "base64url"}],
        MergedOpts = merge_options(Options, Defaults),
        
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"data\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            Data, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        QueryString = build_query_string([{b, proplists:get_value(b, MergedOpts)}]),
        Url = ?RPC_API ++ "/v0/multibase/encode" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
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
    end;

multibase_encode(FilePath, Options) when is_list(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Data} ->
            multibase_encode(Data, Options);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% @doc List available multibase encodings
%% Options can include:
%%   - prefix (boolean): Include single-letter prefixes
%%   - numeric (boolean): Include numeric codes
%% {ok, Encodings} = ipfs_client_4:multibase_list().
multibase_list() ->
    multibase_list([]).

%% {ok, FullList} = ipfs_client_4:multibase_list([{prefix, true},{numeric, true}]).
multibase_list(Options) when is_list(Options) ->
    try
        Defaults = [
            {prefix, false},
            {numeric, false}
        ],
        
        MergedOpts = merge_options(Options, Defaults),
        
        QueryString = build_query_string([
            {prefix, proplists:get_value(prefix, MergedOpts)},
            {numeric, proplists:get_value(numeric, MergedOpts)}
        ]),
        Url = ?RPC_API ++ "/v0/multibase/list" ++ QueryString,
        
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

%% @doc Transcode multibase data between encodings
%% {ok, NewEncoding} = ipfs_client_4:multibase_transcode("/path/to/encoded.file").
multibase_transcode(Data) ->
    multibase_transcode(Data, []).

%% {ok, Result} = ipfs_client_4:multibase_transcode(<<"uaGVsbG8gd29ybGQ=">>, [{b, "base58btc"}]).
multibase_transcode(Data, Options) when is_binary(Data) ->
    try
        Defaults = [
            {b, "base64url"},
            {timeout, 5000}
        ],
        MergedOpts = merge_options(Options, Defaults),
        
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"encoded_file\"; filename=\"data\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            Data, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        QueryString = build_query_string([{b, proplists:get_value(b, MergedOpts)}]),
        Url = ?RPC_API ++ "/v0/multibase/transcode" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
                         [{timeout, proplists:get_value(timeout, MergedOpts)}],
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
    end;

multibase_transcode(FilePath, Options) when is_list(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Data} ->
            multibase_transcode(Data, Options);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
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