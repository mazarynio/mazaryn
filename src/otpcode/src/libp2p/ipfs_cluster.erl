-module(ipfs_cluster).
-author("Zaryn Technologies").
-export([
    get_cluster_info/0,
    get_cluster_version/0,
    list_cluster_peers/0,
    remove_cluster_peer/1,
    add_to_cluster/1,
    list_pins/0,
    get_pin_status/1,
    pin_to_cluster/1,
    unpin_from_cluster/1,
    recover_pin/1,
    recover_all_pins/0,
    get_cluster_health/0,
    get_metrics/0,
    get_metric/1,
    get_alerts/0,
    get_connection_graph/0,
    get_bandwidth_stats/0,
    trigger_ipfs_gc/0,
    generate_token/0
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
    request(post, "/add?arg=" ++ uri_string:quote(Path)).

list_pins() ->
    request(get, "/pins").

get_pin_status(Cid) ->
    request(get, "/pins/" ++ uri_string:quote(Cid)).

pin_to_cluster(Cid) ->
    request(post, "/pins/" ++ uri_string:quote(Cid)).

unpin_from_cluster(Cid) ->
    request(delete, "/pins/" ++ uri_string:quote(Cid)).

recover_pin(Cid) ->
    request(post, "/pins/" ++ uri_string:quote(Cid) ++ "/recover").

recover_all_pins() ->
    request(post, "/pins/recover").

get_cluster_health() ->
    case request(get, "/health") of
        {ok, {{_, 204, _}, _, _}} -> {ok, healthy};
        Error -> Error
    end.

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

%%% Maintenance Endpoints %%%

trigger_ipfs_gc() ->
    request(post, "/ipfs/gc").

generate_token() ->
    request(post, "/token").

%%% Internal Helper Functions %%%

request(Method, Path) ->
    ensure_inets_started(),
    Url = ?CLUSTER_BASE_URL ++ Path,
    case httpc:request(Method, {Url, [], "application/json", <<>>}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, decode_json(ResponseBody)};
        {ok, {{_, 204, _}, _, _}} ->
            {ok, no_content};
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {Status, decode_json(ErrorBody)}};
        {error, Reason} ->
            {error, Reason}
    end.

ensure_inets_started() ->
    case lists:keymember(inets, 1, application:which_applications()) of
        false -> application:start(inets);
        true -> ok
    end.

decode_json(Data) ->
    try jiffy:decode(list_to_binary(Data)), [return_maps] of
        Result -> Result
    catch
        _:_ -> Data
    end.