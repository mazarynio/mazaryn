-module(ipfs_client_1).
-author("Zaryn Technologies").
-export([block_get/1, block_get_raw/1, extract_json/1, add_file/2, add_file/3, bitswap_ledger/1, bitswap_stat/0, bitswap_stat/1,
bitswap_wantlist/0, bitswap_wantlist/1, block_put/2, block_put/3, block_rm/1, block_rm/2, block_stat/1, bootstrap/0, bootstrap_list/0, bootstrap_add/1,
bootstrap_add/2, bootstrap_add_default/0, bootstrap_rm/1, bootstrap_rm_all/0, cat/1, cat/2, cat/3, cat/4, cid_to_base32/1, cid_bases/0, cid_bases/1,
cid_bases/2, cid_codecs/0, cid_codecs/1, cid_codecs/2, cid_format/1, cid_format/2]).

-define(RPC_API, "http://localhost:5001/api").
-define(DEFAULT_ADD_OPTS, [
    {pin, true},
    {progress, false},
    {quiet, false},
    {quieter, false},
    {silent, false},
    {trickle, false},
    {"only-hash", false},
    {"wrap-with-directory", false},
    {"raw-leaves", false},
    {"nocopy", false},
    {"fscache", false},
    {"cid-version", 0}
]).

-define(DEFAULT_BLOCK_PUT_OPTS, [
    {<<"cid-codec">>, <<"raw">>},
    {<<"pin">>, false},
    {<<"allow-big-block">>, false},
    {<<"mhlen">>, -1}
]).

%% Get a raw IPFS block by CID 
block_get(CID) ->
    Url = ?RPC_API ++ "/v0/block/get?arg=" ++ binary_to_list(iolist_to_binary(CID)),
    case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, {status_code, StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Store data as an IPFS block with default options
%% {ok, Data} = file:read_file("rebar.config").
%% ipfs_client:block_put("rebar.config", Data).
block_put(Filename, Data) ->
    block_put(Filename, Data, []).

block_put(Filename, Data, ?DEFAULT_BLOCK_PUT_OPTS) ->
    block_put(Filename, Data, []);

%% Store data as an IPFS block with custom options
block_put(Filename, Data, Options) when is_binary(Data) ->
    try
        DefaultOptions = [
            {<<"cid-codec">>, <<"raw">>},
            {<<"pin">>, false},
            {<<"allow-big-block">>, false},
            {<<"mhlen">>, -1}
        ],
        
        MergedOpts = merge_options(DefaultOptions, Options),
        QueryString = build_query_string(MergedOpts),
        Url = ?RPC_API ++ "/v0/block/put" ++ QueryString,
        
        Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
        ContentType = "multipart/form-data; boundary=" ++ Boundary,
        
        FormData = [
            "--", Boundary, "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"", Filename, "\"\r\n",
            "Content-Type: application/octet-stream\r\n\r\n",
            Data, "\r\n",
            "--", Boundary, "--\r\n"
        ],
        
        HttpcOptions = [{body_format, binary}],
        RequestOptions = [],
        
        case httpc:request(post, 
                         {Url, [], ContentType, iolist_to_binary(FormData)},
                         RequestOptions,
                         HttpcOptions) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Key">> := CID, <<"Size">> := Size} -> 
                        {ok, #{cid => CID, size => Size}};
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

block_rm(CID) ->
    block_rm(CID, [], false).

%% Remove IPFS block with options
block_rm(CID, Options) when is_list(Options) ->
    block_rm(CID, Options, false);
block_rm(CID, Force) when is_boolean(Force) ->
    block_rm(CID, [], Force).

%% Core removal function
block_rm(CID, Options, Force) when is_list(CID) orelse is_binary(CID) ->
    try
        FinalOptions = case Force of
            true -> [{force, true} | Options];
            false -> Options
        end,
        
        QueryString = build_query_string([{arg, CID} | FinalOptions]),
        Url = ?RPC_API ++ "/v0/block/rm" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                Response = jsx:decode(Body),
                case maps:get(<<"Error">>, Response, <<>>) of
                    <<>> -> 
                        {ok, Response};
                    ErrorMsg ->
                        {error, ErrorMsg, Response}
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

%% Get block statistics
block_stat(CID) when is_list(CID); is_binary(CID) ->
    try
        EncodedCID = uri_string:quote(CID),
        Url = ?RPC_API ++ "/v0/block/stat?arg=" ++ EncodedCID,
        
        case httpc:request(post, 
                          {Url, [], "application/json", ""},
                          [],
                          [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Key">> := Key, <<"Size">> := Size} ->
                        {ok, #{key => Key, size => Size}};
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

%% List bootstrap peers
bootstrap() ->
    try
        Url = ?RPC_API ++ "/v0/bootstrap",
        case httpc:request(post, 
                          {Url, [], "application/json", ""},
                          [],
                          [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Peers">> := Peers} ->
                        {ok, Peers};
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

%% Add a peer to bootstrap list
%% ipfs_client:bootstrap_add("/ip4/127.0.0.1/tcp/42511/p2p/12D3KooWZ3HDjKqk41CYfT6Y9q8xaKBoVodd8VmqdAyJet9VxPPe").
bootstrap_add(Peer) when is_list(Peer); is_binary(Peer) ->
    bootstrap_add(Peer, []).

%% Add with options
bootstrap_add(Peer, Options) ->
    try
        QueryString = build_query_string([{arg, Peer} | Options]),
        Url = ?RPC_API ++ "/v0/bootstrap/add" ++ QueryString,
        case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Peers">> := Peers} -> {ok, Peers};
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

%% Add default bootstrap nodes
bootstrap_add_default() ->
    try
        Url = ?RPC_API ++ "/v0/bootstrap/add/default",
        case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Peers">> := Peers} -> 
                        {ok, Peers};
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

%% List all bootstrap peers
bootstrap_list() ->
    try
        Url = ?RPC_API ++ "/v0/bootstrap/list",
        case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Peers">> := Peers} -> {ok, Peers};
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

bootstrap_rm(Peer) when is_list(Peer); is_binary(Peer) ->
    bootstrap_rm(Peer, []).

%% Remove with options
bootstrap_rm(Peer, Options) ->
    try
        QueryString = build_query_string([{arg, Peer} | Options]),
        Url = ?RPC_API ++ "/v0/bootstrap/rm" ++ QueryString,
        case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Peers">> := Peers} -> {ok, Peers};
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

bootstrap_rm_all() ->
    try
        Url = ?RPC_API ++ "/v0/bootstrap/rm/all",
        case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Peers">> := Peers} -> 
                        {ok, Peers};  
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

%% Show IPFS object data
cat(Arg) ->
    cat(Arg, undefined, undefined, true).

cat(Arg, Offset) ->
    cat(Arg, Offset, undefined, true).

%% {ok, PartialData} = ipfs_client:cat("QmHashHere", 50, 100).
cat(Arg, Offset, Length) ->
    cat(Arg, Offset, Length, true).

%% {ok, Data} = ipfs_client:cat("QmHashHere", undefined, undefined, false).
cat(Arg, Offset, Length, Progress) when is_list(Arg) orelse is_binary(Arg) ->
    try
        BaseParams = [{arg, Arg}, {progress, Progress}],
        Params = case {Offset, Length} of
            {undefined, undefined} -> BaseParams;
            {O, undefined} -> [{offset, O} | BaseParams];
            {undefined, L} -> [{length, L} | BaseParams];
            {O, L} -> [{offset, O}, {length, L} | BaseParams]
        end,
        
        QueryString = build_query_string(Params),
        Url = ?RPC_API ++ "/v0/cat" ++ QueryString,
        
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

%% Convert CIDs to Base32 CID version 1.
cid_to_base32(CID) when is_list(CID) orelse is_binary(CID) ->
    try
        QueryString = build_query_string([{arg, CID}]),
        Url = ?RPC_API ++ "/v0/cid/base32" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Formatted">> := Formatted} = Response ->
                        case maps:get(<<"ErrorMsg">>, Response, <<>>) of
                            <<>> -> 
                                {ok, Formatted};
                            ErrorMsg -> 
                                {error, ErrorMsg, Response}
                        end;
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

%% List available multibase encodings
cid_bases() ->
    cid_bases(false, false).

%% {ok, FullBases} = ipfs_client:cid_bases([{prefix, true}, {numeric, true}]).
cid_bases(Options) when is_list(Options) ->
    Prefix = proplists:get_value(prefix, Options, false),
    Numeric = proplists:get_value(numeric, Options, false),
    cid_bases(Prefix, Numeric).

cid_bases(Prefix, Numeric) ->
    try
        QueryString = build_query_string([
            {prefix, Prefix},
            {numeric, Numeric}
        ]),
        Url = ?RPC_API ++ "/v0/cid/bases" ++ QueryString,
        
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

%% List available CID multicodecs.
cid_codecs() ->
    cid_codecs(false, false).

%% {ok, FilteredCodecs} = ipfs_client:cid_codecs([{numeric, true}, {supported, true}]).
cid_codecs(Options) when is_list(Options) ->
    Numeric = proplists:get_value(numeric, Options, false),
    Supported = proplists:get_value(supported, Options, false),
    cid_codecs(Numeric, Supported).

cid_codecs(Numeric, Supported) ->
    try
        QueryString = build_query_string([
            {numeric, Numeric},
            {supported, Supported}
        ]),
        Url = ?RPC_API ++ "/v0/cid/codecs" ++ QueryString,
        
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

%% Format and convert a CID in various useful ways.
cid_format(CID) ->
    cid_format(CID, []).

%% Format CID with custom options
%% ipfs_client:cid_format("QmTpKHAj6xwu3BeNEbPVmrESXMbqXzMsp1pXbyeEHtqnfV", [{v, "1"}]).
cid_format(CID, Options) when is_list(CID) orelse is_binary(CID) ->
    try
        EncodedCID = uri_string:quote(CID),
        BaseParams = [{arg, EncodedCID}],
        FinalParams = case proplists:get_value(f, Options, "%s") of
            "%s" -> BaseParams;
            F -> [{f, uri_string:quote(F)} | BaseParams]
        end ++ lists:foldl(fun
            ({K, V}, Acc) when K =:= v; K =:= mc; K =:= b ->
                [{K, uri_string:quote(V)} | Acc];
            (_, Acc) -> Acc
        end, [], Options),

        QueryString = build_query_string(FinalParams),
        Url = ?RPC_API ++ "/v0/cid/format" ++ QueryString,
        
        case httpc:request(post, 
                         {Url, [], "application/json", ""},
                         [],
                         [{body_format, binary}]) of
            {ok, {{_, 200, _}, _, Body}} ->
                case jsx:decode(Body) of
                    #{<<"Formatted">> := Formatted} = Response ->
                        case maps:get(<<"ErrorMsg">>, Response, <<>>) of
                            <<>> -> {ok, Formatted};
                            ErrorMsg -> {error, ErrorMsg, Response}
                        end;
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

block_get_raw(CID) ->
    Url = ?RPC_API ++ "/v0/block/get?arg=" ++ CID,
    case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, {status_code, StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Returns {error, Reason} on failure
extract_json(BlockData) ->
    try
        <<10, _Length:8, 8, 2, 18, JsonSize:8, JsonData:JsonSize/binary, _Rest/binary>> = BlockData,
        jsx:decode(JsonData)
    catch
        error:_ ->
            {error, {cannot_parse_block, BlockData}}
    end.

%% Add a file to IPFS with default options
%% {ok, FileData} = file:read_file("rebar.config").
%% ipfs_client:add_file("rebar.config", FileData).
add_file(Filename, FileData) ->
    add_file(Filename, FileData, []).

%% Add a file to IPFS with custom options
add_file(Filename, FileData, Options) ->
    Url = ?RPC_API ++ "/v0/add",
    
    MergedOpts = merge_options(?DEFAULT_ADD_OPTS, Options),
    QueryString = build_query_string(MergedOpts),
    
    Boundary = "------" ++ integer_to_list(erlang:unique_integer([positive])),
    ContentType = "multipart/form-data; boundary=" ++ Boundary,
    
    FormData = [
        "--", Boundary, "\r\n",
        "Content-Disposition: form-data; name=\"file\"; filename=\"", Filename, "\"\r\n",
        "Content-Type: application/octet-stream\r\n\r\n",
        FileData, "\r\n",
        "--", Boundary, "--\r\n"
    ],
    
    FullUrl = Url ++ QueryString,
    case httpc:request(post, {FullUrl, [], ContentType, list_to_binary(FormData)}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            jsx:decode(Body);
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, {status_code, StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get the Bitswap ledger for a specific peer
%% ipfs_client:bitswap_ledger("12D3VooWGH2j4bWCJUNzb9NzkT6BLufcVYXvjyT4oZe5V5PTucr2")
bitswap_ledger(PeerID) ->
    Url = ?RPC_API ++ "/v0/bitswap/ledger?arg=" ++ PeerID,
    case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            jsx:decode(Body);
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, {status_code, StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Show some diagnostic information on the bitswap agent.
bitswap_stat() ->
    bitswap_stat([]).

%% ipfs_client:bitswap_stat([{human, true}, {verbose, true}]).
bitswap_stat(Options) ->
    Url = ?RPC_API ++ "/v0/bitswap/stat",
    QueryString = build_query_string(Options),
    FullUrl = Url ++ QueryString,
    case httpc:request(post, {FullUrl, [], "application/json", ""}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            jsx:decode(Body);
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, {status_code, StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

bitswap_wantlist() ->
    bitswap_wantlist([]).

bitswap_wantlist(PeerID) when is_list(PeerID); is_binary(PeerID) ->
    bitswap_wantlist([{peer, PeerID}]);

bitswap_wantlist(Options) when is_list(Options) ->
    try
        Url = ?RPC_API ++ "/v0/bitswap/wantlist",
        QueryString = case Options of
            [] -> "";
            _ -> "?" ++ uri_string:compose_query(Options)
        end,
        FullUrl = Url ++ QueryString,
        
        io:format("DEBUG: Making request to ~s~n", [FullUrl]),
        
        case httpc:request(post, {FullUrl, [], "application/json", ""}, [], 
                         [{body_format, binary}, {timeout, 5000}]) of
            {ok, {{_, 200, _}, _Headers, Body}} ->
                io:format("DEBUG: Received response: ~p~n", [Body]),
                jsx:decode(Body);
            {ok, {{_, StatusCode, _}, _Headers, Body}} ->
                io:format("ERROR: Bad status ~p: ~p~n", [StatusCode, Body]),
                {error, {status_code, StatusCode, Body}};
            {error, Reason} ->
                io:format("ERROR: HTTP error: ~p~n", [Reason]),
                {error, Reason}
        end
    catch
        error ->
            io:format("CRASH: ~p:~p~n~p~n", [error, error, error]),
            {error, {crash, error, error}}
    end.

%% Helper functions

merge_options(Defaults, Options) ->
    lists:ukeymerge(1, 
        lists:ukeysort(1, Options), 
        lists:ukeysort(1, Defaults)).

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