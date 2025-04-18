-module(ipfs_cluster).
-author("Zaryn Technologies").
-export([
    get_cluster_info/0,get_cluster_version/0,list_cluster_peers/0,remove_cluster_peer/1,add_to_cluster/1,list_allocations/0,get_allocation/1,
    list_pins/0,get_pin_status/1,pin_to_cluster/1,pin_path_to_cluster/2,unpin_from_cluster/1,unpin_path_from_cluster/2,recover_pin/1,
    recover_all_pins/0,get_metrics/0,get_metric/1,get_alerts/0,get_connection_graph/0,get_bandwidth_stats/0,trigger_ipfs_gc/0,generate_token/0,
    health_check/0, get_content_size/1
]).

-define(CLUSTER_BASE_URL, "http://localhost:9094").

%%% Cluster Information Endpoints %%%

get_cluster_info() ->
    request(get, "/id").

get_cluster_version() ->
    request(get, "/version").

%%% Peer Management Endpoints %%%

list_cluster_peers() ->
    request(get, "/peers").

remove_cluster_peer(PeerId) ->
    request(delete, "/peers/" ++ PeerId).

%%% Content Management Endpoints %%%

add_to_cluster(Path) ->
    request(post, "/add", [{arg, Path}]).

%%% Allocation Management Endpoints %%%

list_allocations() ->
    request(get, "/allocations").

get_allocation(Cid) ->
    request(get, "/allocations/" ++ uri_encode(Cid)).

%%% Pin Management Endpoints %%%

list_pins() ->
    request(get, "/pins").

get_pin_status(Cid) ->
    request(get, "/pins/" ++ uri_encode(Cid)).

pin_to_cluster(Cid) ->
    request(post, "/pins/" ++ uri_encode(Cid)).

pin_path_to_cluster(PathType, Path) when PathType =:= ipfs; PathType =:= ipns; PathType =:= ipld ->
    PathTypeStr = atom_to_list(PathType),
    request(post, "/pins/" ++ PathTypeStr ++ "/" ++ uri_encode(Path)).

unpin_from_cluster(Cid) ->
    request(delete, "/pins/" ++ uri_encode(Cid)).

unpin_path_from_cluster(PathType, Path) when PathType =:= ipfs; PathType =:= ipns; PathType =:= ipld ->
    PathTypeStr = atom_to_list(PathType),
    request(delete, "/pins/" ++ PathTypeStr ++ "/" ++ uri_encode(Path)).

recover_pin(Cid) ->
    request(post, "/pins/" ++ uri_encode(Cid) ++ "/recover").

recover_all_pins() ->
    request(post, "/pins/recover").

%%% Monitoring and Health Endpoints %%%

get_metrics() ->
    request(get, "/monitor/metrics").

get_metric(Metric) ->
    request(get, "/monitor/metrics/" ++ Metric).

get_alerts() ->
    request(get, "/health/alerts").

get_connection_graph() ->
    request(get, "/health/graph").

get_bandwidth_stats() ->
    request(get, "/health/bandwidth").

health_check() ->
    case request_raw(get, "/health") of
        {ok, {{_, 204, _}, _, _}} -> {ok, healthy};
        Error -> Error
    end.

%%% Maintenance Endpoints %%%

trigger_ipfs_gc() ->
    request(post, "/ipfs/gc").

generate_token() ->
    request(post, "/token").

get_content_size(Cid) ->
    case get_allocation(Cid) of
        {ok, AllocationData} ->
            try
                Size = maps:get(size, AllocationData, 0),
                {ok, Size}
            catch
                _:_ -> {error, invalid_size_data}
            end;
        Error -> Error
    end.

%%% Internal Helper Functions %%%

request(Method, Path) ->
    request(Method, Path, []).

request(Method, Path, Params) ->
    ensure_inets_started(),
    ensure_ssl_started(),
    Url = build_url(Path, Params),
    case request_raw(Method, Url) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, decode_json(ResponseBody)};
        {ok, {{_, 204, _}, _, _}} ->
            {ok, no_content};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, decode_json(ErrorBody)}};
        {error, Reason} ->
            {error, Reason}
    end.

request_raw(get, Url) ->
    httpc:request(get, {Url, []}, [{timeout, 30000}], []);
request_raw(post, Url) ->
    httpc:request(post, {Url, [], "application/json", <<>>}, [{timeout, 30000}], []);
request_raw(delete, Url) ->
    httpc:request(delete, {Url, []}, [{timeout, 30000}], []).

build_url(Path, []) ->
    ?CLUSTER_BASE_URL ++ Path;
build_url(Path, Params) ->
    QueryString = uri_query_string(Params),
    ?CLUSTER_BASE_URL ++ Path ++ "?" ++ QueryString.

uri_query_string(Params) ->
    string:join([Key ++ "=" ++ uri_encode(Value) || {Key, Value} <- Params], "&").

uri_encode(Term) when is_atom(Term) ->
    uri_encode(atom_to_list(Term));
uri_encode(Term) when is_integer(Term) ->
    uri_encode(integer_to_list(Term));
uri_encode(Term) when is_list(Term) ->
    uri_string:quote(Term);
uri_encode(Term) when is_binary(Term) ->
    uri_encode(binary_to_list(Term)).

ensure_inets_started() ->
    case lists:keymember(inets, 1, application:which_applications()) of
        false -> application:start(inets);
        true -> ok
    end.

ensure_ssl_started() ->
    case lists:keymember(ssl, 1, application:which_applications()) of
        false -> application:start(ssl);
        true -> ok
    end.

decode_json(Data) when is_list(Data) ->
    decode_json(list_to_binary(Data));
decode_json(Data) when is_binary(Data) ->
    try jiffy:decode(Data, [return_maps]) of
        Result -> Result
    catch
        _:_ -> Data
    end.