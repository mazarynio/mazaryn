-module(ipfs_client).
-author("Zaryn Technologies").
-export([block_get/1, block_get_raw/1, extract_json/1, add_file/2, add_file/3, bitswap_ledger/1, bitswap_stat/0, bitswap_stat/1,
bitswap_wantlist/0, bitswap_wantlist/1]).

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

%% Get a raw IPFS block by CID 
block_get(CID) ->
    Url = ?RPC_API ++ "/v0/block/get?arg=" ++ CID,
    case httpc:request(post, {Url, [], "application/json", ""}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            try
                <<10, _Length:8, 8, 2, 18, JsonSize:8, JsonData:JsonSize/binary, _Rest/binary>> = Body,
                jsx:decode(JsonData)
            catch
                error:_ ->
                    {error, {cannot_parse_block, Body}}
            end;
        {ok, {{_, StatusCode, _}, _Headers, Body}} ->
            {error, {status_code, StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
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