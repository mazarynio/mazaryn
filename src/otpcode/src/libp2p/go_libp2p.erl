-module(go_libp2p).
-export([
    create_node/0,
    create_node/1,
    list_nodes/0,
    delete_node/1, get_peer_info/1, get_peer_info/2,
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

%% Get peer information by nodeID
get_peer_info(NodeId) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Url = ?BASE_URL ++ "/nodes/peer-info?nodeID=" ++ NodeId,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get peer information by nodeID and peerID
get_peer_info(NodeId, PeerId) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Url = ?BASE_URL ++ "/nodes/peer-info?nodeID=" ++ NodeId ++ "&peerID=" ++ PeerId,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jsx:decode(list_to_binary(Body), [return_maps])};
        {ok, {{_, 500, _}, _, _ErrorBody}} ->
            case check_ipfs_api() of
                {ok, _} ->
                    {error, {500, "Failed to query peer info from IPFS API"}};
                {error, Reason} ->
                    {error, {500, "IPFS API is not accessible: " ++ Reason}}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

check_ipfs_api() ->
    Url = "http://localhost:5001/api/v0/id",
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            {ok, "IPFS API is accessible"};
        {ok, {{_, Status, _}, _, _ErrorBody}} ->
            {error, "IPFS API returned status: " ++ integer_to_list(Status)};
        {error, Reason} ->
            {error, "Failed to connect to IPFS API: " ++ Reason}
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

%% Get a single address of a node, including the peer ID
get_single_address(NodeId) ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/single-address", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            case jsx:decode(list_to_binary(Body), [return_maps]) of
                #{<<"address">> := Address} ->
                    AddressStr = binary_to_list(Address),
                    case get_peerid(NodeId) of
                        {ok, PeerId} ->
                            PeerIdStr = case is_binary(PeerId) of
                                          true -> binary_to_list(PeerId);
                                          false -> PeerId
                                       end,
                            FullAddr = AddressStr ++ "/p2p/" ++ PeerIdStr,
                            {ok, list_to_binary(FullAddr)};
                        {error, Reason} ->
                            {error, Reason}
                    end;
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
    Url = ?BASE_URL ++ "/nodes/" ++ uri_string:quote(NodeId) ++ "/peerid",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body}; 
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Ping a remote peer
ping_peer(NodeId, {ok, RemoteAddr}) ->
    ensure_inets_started(),
    RemoteAddrStr = case is_binary(RemoteAddr) of
                      true -> binary_to_list(RemoteAddr);
                      false -> RemoteAddr
                   end,
    EncodedAddr = uri_string:quote(RemoteAddrStr),
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ping?addr=" ++ EncodedAddr,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            ResponseBinary = list_to_binary(ResponseBody),
            ResponseUtf8 = unicode:characters_to_list(ResponseBinary, utf8),
            {ok, ResponseUtf8};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end;
ping_peer(_, {error, Reason}) ->
    {error, Reason}.

connect_to_peer(NodeId, {ok, RemoteAddr}) ->
    ensure_inets_started(),
    RemoteAddrStr = case is_binary(RemoteAddr) of
                      true -> binary_to_list(RemoteAddr);
                      false -> RemoteAddr
                   end,
    EncodedAddr = uri_string:quote(RemoteAddrStr),
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/connect?addr=" ++ EncodedAddr,
    
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            io:format("ResponseBody: ~p~n", [ResponseBody]), 
            {ok, jsx:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end;
connect_to_peer(_, {error, Reason}) ->
    {error, Reason}.

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
    EncodedTopic = uri_string:quote(Topic),
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/subscribe?topic=" ++ EncodedTopic,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Unsubscribe from a PubSub topic
unsubscribe_from_topic(NodeId, Topic) ->
    ensure_inets_started(),
    EncodedTopic = uri_string:quote(Topic),
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/unsubscribe?topic=" ++ EncodedTopic,
    case httpc:request(delete, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Publish a message to a PubSub topic
publish_message(NodeId, Topic, Message) ->
    ensure_inets_started(),
    EncodedTopic = uri_string:quote(Topic),
    EncodedMessage = uri_string:quote(Message),
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/publish?topic=" ++ EncodedTopic ++ "&message=" ++ EncodedMessage,
    case httpc:request(put, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get subscriptions of a node
get_subscriptions(NodeId) ->
    ensure_inets_started(),
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/pubsub/subscriptions",
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody), [return_maps])};
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
            {ok, jsx:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get a file from IPFS
get_file_from_ipfs(NodeId, Cid) ->
    ensure_inets_started(),
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipfs/get/" ++ Cid,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            % Decode the JSON response
            case jsx:decode(list_to_binary(Body), [return_maps]) of
                #{<<"fileContent">> := FileContent} ->
                    % Convert binary to string (list of characters)
                    FileContentStr = binary_to_list(FileContent),
                    {ok, FileContentStr};
                _ ->
                    {error, invalid_response}
            end;
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