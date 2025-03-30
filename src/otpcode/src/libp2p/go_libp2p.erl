-module(go_libp2p).
-author("Zaryn Technologies").
-export([
    create_node/0, make_post_request/2,
    create_node/1,
    list_nodes/0,
    delete_node/1, get_peer_info/1, get_peer_info/2, connect_to_ipfs_network/2, get_ipfs_singleaddr/0, get_ipfs_multiaddr/0,
    get_addresses/1, publish_to_ipns/2, resolve_ipns/2, get_network_status/1, get_file_metadata/2,
    get_single_address/1, dht_find_peer/2, dht_find_provs/2, dht_provide/2, add_file_to_ipfs/1, get_file_from_ipfs/1,
    get_peerid/1, add_dag_node/1, link_dag_nodes/1, get_dag_node/1, resolve_dag_path/2, traverse_dag/1,
    ping_peer/2, bitswap_wantlist/0, bitswap_stat/0, bitswap_ledger/1, 
    connect_to_peer/2, mfs_mkdir/1, mfs_write/2, mfs_ls/1, mfs_read/1, mfs_rm/1, mfs_cp/2, mfs_mv/2,
    get_peers/1, ensure_inets_started/0,
    subscribe_to_topic/2,
    unsubscribe_from_topic/2,
    publish_message/3,
    get_subscriptions/1,
    add_file_to_ipfs/2,
    get_file_from_ipfs/2
]).

-define(BASE_URL, "http://localhost:3000").
-define(DEFAULT_NODE, "default").



%% Ensure inets and ssl applications are started
ensure_inets_started() ->
    InetsStarted = lists:keymember(inets, 1, application:which_applications()),
    case InetsStarted of
        true -> ok;
        false -> application:start(inets)
    end,
    SslStarted = lists:keymember(ssl, 1, application:which_applications()),
    case SslStarted of
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

make_post_request(Url, Body) ->
    ensure_inets_started(),
    Headers = [{"Content-Type", "application/json"}],
    case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, ResponseBody};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Create a new node with a random ID
create_node() ->
    create_node(generate_node_id()).


ensure_jiffy_loaded() ->
    case code:ensure_loaded(jiffy) of
        {module, jiffy} -> ok;
        {error, _} -> 
            error_logger:warning_msg("Jiffy module not found. JSON handling may be limited."),
            {error, jiffy_not_found}
    end.

%% Update your create_node function to use Jiffy
create_node(NodeId) when is_list(NodeId) ->
    ensure_inets_started(),
    case ensure_jiffy_loaded() of
        ok ->
            Url = ?BASE_URL ++ "/nodes?id=" ++ NodeId,
            case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
                {ok, {{_, 200, _}, _, ResponseBody}} ->
                    try
                        NodeInfo = jiffy:decode(list_to_binary(ResponseBody), [return_maps]),
                        case get_ipfs_singleaddr() of
                            {ok, IpfsSingleAddr} ->
                                case connect_to_ipfs_network(NodeId, IpfsSingleAddr) of
                                    {ok, ConnectionResult} ->
                                        {ok, #{<<"node_info">> => NodeInfo, <<"connection_result">> => ConnectionResult}};
                                    {error, ConnectionError} ->
                                        {ok, #{<<"node_info">> => NodeInfo, <<"connection_error">> => ConnectionError}}
                                end;
                            {error, IpfsError} ->
                                {ok, #{<<"node_info">> => NodeInfo, <<"ipfs_error">> => IpfsError}}
                        end
                    catch
                        error:Reason ->
                            {error, {invalid_response, json_decode_failed, Reason}}
                    end;
                {ok, {{_, Status, _}, _, ErrorBody}} ->
                    {error, {http_error, Status, ErrorBody}};
                {error, Reason} ->
                    {error, {http_request_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% List all active nodes
list_nodes() ->
    ensure_inets_started(),
    case httpc:request(get, {?BASE_URL ++ "/nodes", []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jiffy:decode(list_to_binary(Body))};
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
            {ok, jiffy:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get peer information by nodeID
get_peer_info(NodeId) ->
    ensure_inets_started(),
    ensure_jiffy_loaded(),
    Url = ?BASE_URL ++ "/nodes/peer-info?nodeID=" ++ NodeId,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jiffy:decode(list_to_binary(Body), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get peer information by nodeID and peerID
get_peer_info(NodeId, PeerId) ->
    ensure_inets_started(),
    ensure_jiffy_loaded(),
    Url = ?BASE_URL ++ "/nodes/peer-info?nodeID=" ++ NodeId ++ "&peerID=" ++ PeerId,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, jiffy:decode(list_to_binary(Body), [return_maps])};
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
            {ok, jiffy:decode(list_to_binary(Body))};
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
            case jiffy:decode(list_to_binary(Body), [return_maps]) of
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
            {ok, jiffy:decode(list_to_binary(ResponseBody), [return_maps])};
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
            {ok, jiffy:decode(list_to_binary(Body))};
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
            {ok, jiffy:decode(list_to_binary(ResponseBody), [return_maps])};
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
            {ok, jiffy:decode(list_to_binary(ResponseBody), [return_maps])};
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
            {ok, jiffy:decode(list_to_binary(ResponseBody), [return_maps])};
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
            {ok, jiffy:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get the single address of the IPFS node
get_ipfs_singleaddr() ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/id",
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            try
                Decoded = jiffy:decode(list_to_binary(ResponseBody), [return_maps]),
                case Decoded of
                    #{<<"Addresses">> := [IpfsMultiaddr | _]} ->
                        {ok, binary_to_list(IpfsMultiaddr)}; 
                    _ ->
                        {error, {invalid_response, no_addresses_found}}
                end
            catch
                _:_ ->
                    {error, {invalid_response, json_decode_failed}}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {http_error, Status, ErrorBody}};
        {error, Reason} ->
            {error, {http_request_failed, Reason}}
    end.

get_ipfs_multiaddr() ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/id",
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jiffy:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.


%% Connect the libp2p node to the IPFS network
connect_to_ipfs_network(NodeId, IpfsMultiaddr) ->
    ensure_inets_started(),
    Url = "http://localhost:3000/nodes/" ++ NodeId ++ "/connect-ipfs?addr=" ++ uri_string:quote(IpfsMultiaddr),
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jiffy:decode(list_to_binary(ResponseBody), [return_maps])};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Add a file to IPFS
add_file_to_ipfs(NodeId, FileContent) ->
    ensure_inets_started(),
    ensure_jiffy_loaded(),  
    FileContentBinary = if
        is_list(FileContent), is_integer(hd(FileContent)) -> list_to_binary(FileContent);
        is_binary(FileContent) -> FileContent;
        true -> list_to_binary(io_lib:format("~p", [FileContent]))
    end,
    Body = jiffy:encode({[{<<"fileContent">>, FileContentBinary}]}), 
    case httpc:request(post, {?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipfs/add", [], "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jiffy:decode(list_to_binary(ResponseBody), [return_maps]),
            CidBinary = maps:get(<<"cid">>, Response),
            binary_to_list(CidBinary);
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

add_file_to_ipfs(FileContent) ->
    add_file_to_ipfs(?DEFAULT_NODE, FileContent).

%% Get a file from IPFS
get_file_from_ipfs(NodeId, Cid) ->
    ensure_inets_started(),
    ensure_jiffy_loaded(),  
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipfs/get/" ++ Cid,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            case jiffy:decode(list_to_binary(Body), [return_maps]) of  
                #{<<"fileContent">> := FileContent} ->
                    binary_to_list(FileContent);
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_file_from_ipfs(Cid) ->
    get_file_from_ipfs(?DEFAULT_NODE, Cid).

%% Create a directory in MFS
mfs_mkdir(Path) ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/files/mkdir?arg=" ++ Path,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Write a file to MFS
mfs_write(FilePath, MfsPath) ->
    ensure_inets_started(),
    case file:read_file(FilePath) of
        {ok, FileData} ->
            Url = "http://localhost:5001/api/v0/files/write?arg=" ++ MfsPath ++ "&create=true",

            Boundary = "----------boundary" ++ integer_to_list(erlang:system_time()),
            ContentType = "multipart/form-data; boundary=" ++ Boundary,
            
            FormStart = list_to_binary("--" ++ Boundary ++ "\r\n" ++
                      "Content-Disposition: form-data; name=\"file\"; filename=\"" ++ 
                      filename:basename(FilePath) ++ "\"\r\n" ++
                      "Content-Type: application/octet-stream\r\n\r\n"),
            FormEnd = list_to_binary("\r\n--" ++ Boundary ++ "--\r\n"),
            
            FormData = [FormStart, FileData, FormEnd],
            
            case httpc:request(post, {Url, [], ContentType, FormData}, [], []) of
                {ok, {{_, 200, _}, _, _}} ->
                    ok;
                {ok, {{_, Status, _}, _, ErrorBody}} ->
                    {error, {Status, ErrorBody}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% List files and directories in MFS
mfs_ls(Path) ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/files/ls?arg=" ++ Path ++ "&long=true",
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jiffy:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Read a file from MFS
mfs_read(MfsPath) ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/files/read?arg=" ++ MfsPath,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, list_to_binary(ResponseBody)};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Remove a file or directory from MFS
mfs_rm(Path) ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/files/rm?arg=" ++ Path,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Copy a file or directory in MFS
mfs_cp(SourcePath, DestPath) ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/files/cp?arg=" ++ SourcePath ++ "&arg=" ++ DestPath,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Move a file or directory in MFS
mfs_mv(SourcePath, DestPath) ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/files/mv?arg=" ++ SourcePath ++ "&arg=" ++ DestPath,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

publish_to_ipns(NodeId, Cid) ->
    ensure_inets_started(),
    ensure_jiffy_loaded(),  
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipns/publish?nodeID=" ++ NodeId ++ "&cid=" ++ Cid,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            try
                #{<<"ipns_name">> := IpnsName} = jiffy:decode(list_to_binary(ResponseBody), [return_maps]),
                {ok, IpnsName}
            catch
                _:_ ->
                    {error, {invalid_response, json_decode_failed}}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {http_error, Status, ErrorBody}};
        {error, Reason} ->
            {error, {http_request_failed, Reason}}
    end.

resolve_ipns(NodeId, IpnsName) ->
    ensure_inets_started(),
    ensure_jiffy_loaded(),  
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/ipns/resolve?nodeID=" ++ NodeId ++ "&ipnsName=" ++ IpnsName,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            try
                #{<<"resolved_path">> := ResolvedPath} = jiffy:decode(list_to_binary(ResponseBody), [return_maps]),
                {ok, ResolvedPath}
            catch
                _:_ ->
                    {error, {invalid_response, json_decode_failed}}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {http_error, Status, ErrorBody}};
        {error, Reason} ->
            {error, {http_request_failed, Reason}}
    end.

get_network_status(NodeId) ->
    ensure_inets_started(),
    ensure_jsx_loaded(),
    Url = ?BASE_URL ++ "/nodes/" ++ NodeId ++ "/network-status?nodeID=" ++ NodeId,
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            try
                NetworkStatus = jiffy:decode(list_to_binary(ResponseBody), [return_maps]),
                {ok, NetworkStatus}
            catch
                _:_ ->
                    {error, {invalid_response, json_decode_failed}}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {http_error, Status, ErrorBody}};
        {error, Reason} ->
            {error, {http_request_failed, Reason}}
    end.

%% Get metadata of a file from IPFS
get_file_metadata(NodeId, Cid) ->
    ensure_inets_started(),
    BaseUrl = get_node_base_url(NodeId),
    Url = BaseUrl ++ "/dag/stat?arg=" ++ Cid,
    io:format("Sending request to URL: ~p~n", [Url]), 
    case httpc:request(post, {Url, [], [], []}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            JsonObjects = binary:split(Body, <<"\n">>, [global, trim]),
            DecodedObjects = lists:map(fun(Json) ->
                jiffy:decode(Json, [return_maps])
            end, JsonObjects),
            {ok, DecodedObjects};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            io:format("Error response: Status=~p, Body=~p~n", [Status, ErrorBody]), 
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            io:format("Request failed: ~p~n", [Reason]), 
            {error, Reason}
    end.

get_node_base_url(_NodeId) ->
    "http://localhost:5001/api/v0".

%% DHT Find Peer
dht_find_peer(NodeId, PeerId) ->
    ensure_inets_started(),
    case get_peerid(NodeId) of
        {ok, _PeerId} ->
            Url = "http://localhost:5001/api/v0/routing/findpeer?arg=" ++ PeerId,
            case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
                {ok, {{_, 200, _}, _, Body}} ->
                    {ok, jiffy:decode(list_to_binary(Body), [return_maps])};
                {ok, {{_, Status, _}, _, ErrorBody}} ->
                    {error, {Status, ErrorBody}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% DHT Find Providers
dht_find_provs(NodeId, Cid) ->
    ensure_inets_started(),
    case get_peerid(NodeId) of
        {ok, _PeerId} ->
            Url = "http://localhost:5001/api/v0/routing/findprovs?arg=" ++ Cid,
            case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
                {ok, {{_, 200, _}, _, Body}} ->
                    {ok, jiffy:decode(list_to_binary(Body))};
                {ok, {{_, Status, _}, _, ErrorBody}} ->
                    {error, {Status, ErrorBody}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% DHT Provide
dht_provide(NodeId, Cid) ->
    ensure_inets_started(),
    case get_peerid(NodeId) of
        {ok, _PeerId} ->
            Url = "http://localhost:5001/api/v0/routing/provide?arg=" ++ Cid,
            case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
                {ok, {{_, 200, _}, _, Body}} ->
                    {ok, jiffy:decode(list_to_binary(Body), [return_maps])};
                {ok, {{_, Status, _}, _, ErrorBody}} ->
                    {error, {Status, ErrorBody}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Add a DAG node to IPFS
add_dag_node(Data) ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/dag/put?store-codec=dag-json",
    JsonData = jiffy:encode(Data),
    
    Boundary = "----------boundary" ++ integer_to_list(erlang:system_time()),
    FormData = "--" ++ Boundary ++ "\r\n" ++
              "Content-Disposition: form-data; name=\"object data\"; filename=\"data.json\"\r\n" ++
              "Content-Type: application/json\r\n\r\n" ++
              binary_to_list(JsonData) ++ "\r\n" ++
              "--" ++ Boundary ++ "--\r\n",
    
    ContentType = "multipart/form-data; boundary=" ++ Boundary,
    Headers = [{"Content-Type", ContentType}],
    
    case httpc:request(post, {Url, Headers, ContentType, FormData}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            DecodedBody = jiffy:decode(list_to_binary(ResponseBody)),
            Cid = maps:get(<<"Cid">>, DecodedBody),
            {ok, Cid};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Create a parent DAG node that links to child nodes
link_dag_nodes(ChildCids) ->
    Links = lists:map(fun(Cid) -> 
        case is_map(Cid) andalso maps:is_key(<<"/">>, Cid) of
            true -> 
                Cid;
            false ->
                CidValue = if
                    is_binary(Cid) -> Cid;
                    is_list(Cid) -> list_to_binary(Cid)
                end,
                #{<<"/">> => CidValue}
        end
    end, ChildCids),  
    ParentData = #{<<"links">> => Links},
    add_dag_node(ParentData).

%% Retrieve a DAG node by its CID
get_dag_node(Cid) ->
    ensure_inets_started(),
    FormattedCid = case is_binary(Cid) of
        true -> binary_to_list(Cid);
        false -> 
            case is_map(Cid) andalso maps:is_key(<<"/">>, Cid) of
                true -> binary_to_list(maps:get(<<"/">>, Cid));
                false -> Cid  
            end
    end,
    
    Url = "http://localhost:5001/api/v0/dag/get?arg=" ++ FormattedCid,
    
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jiffy:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Resolve a path within a DAG
resolve_dag_path(Cid, Path) ->
    ensure_inets_started(),
    
    FormattedCid = format_cid(Cid),
    FormattedPath = format_path(Path),
    
    Url = "http://localhost:5001/api/v0/dag/resolve?arg=" ++ FormattedCid ++ FormattedPath,
    
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jiffy:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.
    
% Helper function to format path properly
format_path(Path) when is_list(Path), Path =/= [] ->
    case hd(Path) of
        $/ -> Path;
        _ -> "/" ++ Path
    end;
format_path(Path) when is_binary(Path), byte_size(Path) > 0 ->
    <<FirstByte:8, _/binary>> = Path,
    case FirstByte of
        $/ -> binary_to_list(Path);
        _ -> "/" ++ binary_to_list(Path)
    end;
format_path(Path) when is_binary(Path) ->
    binary_to_list(Path);
format_path(Path) ->
    Path.

%% traverse the DAG
traverse_dag(Cid) ->
    case get_dag_node(Cid) of
        {ok, Node} ->
            case maps:get(<<"links">>, Node, []) of
                [] ->
                    {ok, Node};
                Links ->
                    ChildResults = lists:map(fun(Link) ->
                        LinkCid = case maps:is_key(<<"/">>, Link) of
                            true -> 
                                #{<<"/">> := CidValue} = Link,
                                CidValue;
                            false -> 
                                case maps:is_key(<<"Cid">>, Link) of
                                    true -> maps:get(<<"Cid">>, Link);
                                    false -> Link 
                                end
                        end,
                        {ok, ChildNode} = traverse_dag(LinkCid),
                        ChildNode
                    end, Links),
                    {ok, #{<<"node">> => Node, <<"children">> => ChildResults}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Helper function to format CID in a consistent way
format_cid(Cid) ->
    case is_binary(Cid) of
        true -> binary_to_list(Cid);
        false -> 
            case is_map(Cid) andalso maps:is_key(<<"/">>, Cid) of
                true -> binary_to_list(maps:get(<<"/">>, Cid));
                false -> 
                    case is_map(Cid) andalso maps:is_key(<<"Cid">>, Cid) of
                        true -> binary_to_list(maps:get(<<"Cid">>, Cid));
                        false -> 
                            if is_list(Cid) -> Cid;
                               true -> erlang:error({invalid_cid_format, Cid})
                            end
                    end
            end
    end.

%% Retrieve the list of blocks your node is currently requesting
bitswap_wantlist() ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/bitswap/wantlist",
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get statistics about Bitswap activity
bitswap_stat() ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/bitswap/stat",
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, ErrorBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Retrieve the ledger for a specific peer
bitswap_ledger(PeerId) ->
    ensure_inets_started(),
    Url = "http://localhost:5001/api/v0/bitswap/ledger?arg=" ++ PeerId,
    case httpc:request(post, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(list_to_binary(ResponseBody))};
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