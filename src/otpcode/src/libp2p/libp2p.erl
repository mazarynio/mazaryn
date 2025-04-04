-module(libp2p).
-export([
    create_node/0,
    create_node/1,
    list_nodes/0,
    delete_node/1,
    get_addresses/1,
    get_single_address/1,
    get_peerid/1,
    ping_peer/2,
    connect_to_peer/2,
    get_peers/1,
    subscribe_to_topic/2,
    unsubscribe_from_topic/2,
    publish_message/3,
    get_subscriptions/1,
    add_file_to_ipfs/2,
    get_file_from_ipfs/2
]).

-define(BASE_URL, "http://localhost:3000").

ensure_inets_started() ->
    case lists:member(inets, application:which_applications()) of
        true -> ok;
        false -> application:start(inets)
    end,
    case lists:member(ssl, application:which_applications()) of
        true -> ok;
        false -> application:start(ssl)
    end.

ensure_jsx_loaded() ->
    case code:which(jsx) of
        non_existing ->
            io:format("Warning: JSX module not found. JSON handling may be limited.~n");
        _ -> ok
    end.

%% {ok, _} = libp2p:create_node("node1").
%% {ok, _} = libp2p:create_node("node2").
%% Addr1 = libp2p:get_single_address("node1").
%% Addr2 = libp2p:get_single_address("node2").
%% libp2p:connect_to_peer("node1", Addr2).
%% libp2p:ping_peer("node1", Addr2).
create_node() ->
    create_node(generate_node_id()).

create_node(NodeId) when is_list(NodeId) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Body = jsx:encode([{nodeId, list_to_binary(NodeId)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes", [], "application/json", Body}, [], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Result = jsx:decode(list_to_binary(ResponseBody)),
            {ok, Result};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error creating node: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error creating node: ~p~n", [Reason]),
            {error, Reason}
    end;
create_node(NodeId) when is_binary(NodeId) ->
    create_node(binary_to_list(NodeId)).

list_nodes() ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error listing nodes: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error listing nodes: ~p~n", [Reason]),
            {error, Reason}
    end.

delete_node(NodeId) ->
    ensure_inets_started(),
    case httpc:request(delete, {?BASE_URL ++ "/nodes/" ++ NodeId, []}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error deleting node: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error deleting node: ~p~n", [Reason]),
            {error, Reason}
    end.

get_addresses(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/addresses", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error getting addresses: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error getting addresses: ~p~n", [Reason]),
            {error, Reason}
    end.

get_single_address(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/single-address", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            Body;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error getting single address: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error getting single address: ~p~n", [Reason]),
            {error, Reason}
    end.

get_peerid(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/peerid", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            Body;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error getting peer ID: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error getting peer ID: ~p~n", [Reason]),
            {error, Reason}
    end.

ping_peer(NodeId, RemoteAddr) ->
    ensure_inets_started(),
    Body = jsx:encode([{remoteAddr, list_to_binary(RemoteAddr)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ping", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error pinging peer: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error pinging peer: ~p~n", [Reason]),
            {error, Reason}
    end.

connect_to_peer(NodeId, RemoteAddr) ->
    ensure_inets_started(),
    io:format("Attempting to connect node ~p to ~p~n", [NodeId, RemoteAddr]),
    OwnPeerId = get_peerid(NodeId),
    case is_error(OwnPeerId) of
        true ->
            io:format("Error getting own peer ID for node ~p~n", [NodeId]),
            try_connect(NodeId, RemoteAddr);
        false ->
            case string:find(RemoteAddr, OwnPeerId) of
                nomatch ->
                    io:format("Attempting connection to remote peer~n"),
                    try_connect(NodeId, RemoteAddr);
                _ ->
                    io:format("Error: Cannot connect to self. This is our own node.~n"),
                    {error, self_connection}
            end
    end.

is_error({error, _}) -> true;
is_error(_) -> false.

try_connect(NodeId, RemoteAddr) ->
    Body = jsx:encode([{remoteAddr, list_to_binary(RemoteAddr)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/connect", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Connection failed: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error connecting to peer: ~p~n", [Reason]),
            {error, Reason}
    end.

get_peers(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/peers", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error getting peers: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error getting peers: ~p~n", [Reason]),
            {error, Reason}
    end.

subscribe_to_topic(NodeId, Topic) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Body = jsx:encode([{topic, list_to_binary(Topic)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/subscribe", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error subscribing to topic: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error subscribing to topic: ~p~n", [Reason]),
            {error, Reason}
    end.

unsubscribe_from_topic(NodeId, Topic) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Body = jsx:encode([{topic, list_to_binary(Topic)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/unsubscribe", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error unsubscribing from topic: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error unsubscribing from topic: ~p~n", [Reason]),
            {error, Reason}
    end.

publish_message(NodeId, Topic, Message) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Body = jsx:encode([{topic, list_to_binary(Topic)}, {message, list_to_binary(Message)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/publish", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error publishing message: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error publishing message: ~p~n", [Reason]),
            {error, Reason}
    end.

get_subscriptions(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/subscriptions", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error getting subscriptions: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error getting subscriptions: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Add a file to IPFS
add_file_to_ipfs(NodeId, FileContent) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Body = jsx:encode([{fileContent, list_to_binary(FileContent)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipfs/add", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error adding file to IPFS: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error adding file to IPFS: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Get a file from IPFS
get_file_from_ipfs(NodeId, Cid) when is_list(Cid) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipfs/get/" ++ Cid, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error retrieving file from IPFS: HTTP status ~p, Body: ~p~n", [Status, ErrorBody]),
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Error retrieving file from IPFS: ~p~n", [Reason]),
            {error, Reason}
    end.

generate_node_id() ->
    RandomBytes = crypto:strong_rand_bytes(4),
    "node-" ++ binary_to_hex(RandomBytes).

binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).