-module(go_libp2p).
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

%% Ensure inets and ssl applications are started
ensure_inets_started() ->
    case lists:member(inets, application:which_applications()) of
        true -> ok;
        false -> application:start(inets)
    end,
    case lists:member(ssl, application:which_applications()) of
        true -> ok;
        false -> application:start(ssl)
    end.

%% Ensure jsx is loaded for JSON handling
ensure_jsx_loaded() ->
    case code:which(jsx) of
        non_existing ->
            io:format("Warning: JSX module not found. JSON handling may be limited.~n");
        _ -> ok
    end.

%% Create a new node with a random ID
create_node() ->
    create_node(generate_node_id()).

%% Create a new node with a specific ID
create_node(NodeId) when is_list(NodeId) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Url = ?BASE_URL ++ "/nodes?id=" ++ NodeId, 
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            % Parse the JSON response
            {ok, jsx:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% List all active nodes
list_nodes() ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Delete a node by ID
delete_node(NodeId) ->
    ensure_inets_started(),
    case httpc:request(delete, {?BASE_URL ++ "/nodes/" ++ NodeId, []}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get addresses of a node
get_addresses(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/addresses", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get a single address of a node
get_single_address(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/single-address", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            % Parse the JSON response
            case jsx:decode(list_to_binary(Body), [return_maps]) of
                #{<<"address">> := Address} ->
                    Address;
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get the peer ID of a node
get_peerid(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/peerid", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            Body;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Ping a remote peer
ping_peer(NodeId, RemoteAddr) ->
    ensure_inets_started(),
    Body = jsx:encode([{remoteAddr, list_to_binary(RemoteAddr)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ping", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Connect to a remote peer
connect_to_peer(NodeId, RemoteAddr) ->
    ensure_inets_started(),
    Body = jsx:encode([{remoteAddr, list_to_binary(RemoteAddr)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/connect", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get discovered peers of a node
get_peers(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/peers", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Subscribe to a PubSub topic
subscribe_to_topic(NodeId, Topic) ->
    ensure_inets_started(),
    Body = jsx:encode([{topic, list_to_binary(Topic)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/subscribe", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Unsubscribe from a PubSub topic
unsubscribe_from_topic(NodeId, Topic) ->
    ensure_inets_started(),
    Body = jsx:encode([{topic, list_to_binary(Topic)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/unsubscribe", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Publish a message to a PubSub topic
publish_message(NodeId, Topic, Message) ->
    ensure_inets_started(),
    Body = jsx:encode([{topic, list_to_binary(Topic)}, {message, list_to_binary(Message)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/publish", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get subscriptions of a node
get_subscriptions(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/subscriptions", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Add a file to IPFS
add_file_to_ipfs(NodeId, FileContent) ->
    ensure_inets_started(),
    Body = jsx:encode([{fileContent, list_to_binary(FileContent)}]),
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipfs/add", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get a file from IPFS
get_file_from_ipfs(NodeId, Cid) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipfs/get/" ++ Cid, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Generate a random node ID
generate_node_id() ->
    RandomBytes = crypto:strong_rand_bytes(4),
    "node-" ++ binary_to_hex(RandomBytes).

%% Convert binary to hexadecimal string
binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).