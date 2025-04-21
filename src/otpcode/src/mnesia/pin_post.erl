-module(pin_post).
-author("Zaryn Technologies").
-export([remote_pin_post/1, remote_pin_post/2, remote_pin_post/3, verify_pin_health/1, get_pin_info/1,
         unpin_post/1, unpin_post/2, get_gateway_url/1, get_gateway_url/2, bulk_pin_posts/1,
         bulk_pin_posts/2, get_pin_statistics/0, get_pin_statistics/1, schedule_pin_operation/3,
         update_pinning_metrics/3, start_link/0]).

-define(SERVER, pin_post_server).

%% @doc Start the GenServer
start_link() ->
    gen_server:start_link({local, ?SERVER}, pin_post_server, [], []).

%% @doc Pins a post to IPFS cluster with default options
remote_pin_post(PostID) ->
    gen_server:call(?SERVER, {remote_pin_post, PostID, #{}}).

%% @doc Pins a post with options or to a specific service
remote_pin_post(PostID, Options) when is_map(Options) ->
    gen_server:call(?SERVER, {remote_pin_post, PostID, undefined, Options});
remote_pin_post(PostID, Service) when is_list(Service) orelse is_binary(Service) ->
    gen_server:call(?SERVER, {remote_pin_post, PostID, Service, #{}}).

%% @doc Pins a post's content and media to a specific IPFS cluster service with options
remote_pin_post(PostID, Service, Options) when (is_list(PostID) orelse is_binary(PostID)),
                                              is_map(Options) ->
    gen_server:call(?SERVER, {remote_pin_post, PostID, Service, Options}).

%% @doc Verifies the health of a pin
verify_pin_health(PinID) ->
    gen_server:call(?SERVER, {verify_pin_health, PinID}).

%% @doc Get detailed pin information
get_pin_info(PinID) ->
    gen_server:call(?SERVER, {get_pin_info, PinID}).

%% @doc Unpin a post
unpin_post(PostID) ->
    gen_server:call(?SERVER, {unpin_post, PostID, #{}}).

%% @doc Unpin a post with options
unpin_post(PostID, Options) ->
    gen_server:call(?SERVER, {unpin_post, PostID, Options}).

%% @doc Get gateway URL for a CID
get_gateway_url(CID) ->
    gen_server:call(?SERVER, {get_gateway_url, CID, #{}}).

%% @doc Get gateway URL for a CID with options
get_gateway_url(CID, Options) ->
    gen_server:call(?SERVER, {get_gateway_url, CID, Options}).

%% @doc Pin multiple posts in bulk
bulk_pin_posts(PostIDs) ->
    gen_server:cast(?SERVER, {bulk_pin_posts, PostIDs, #{}}).

%% @doc Pin multiple posts in bulk with options
bulk_pin_posts(PostIDs, Options) ->
    gen_server:cast(?SERVER, {bulk_pin_posts, PostIDs, Options}).

%% @doc Get pinning statistics
get_pin_statistics() ->
    gen_server:call(?SERVER, {get_pin_statistics, #{}}).

%% @doc Get pinning statistics with filters
get_pin_statistics(Filters) ->
    gen_server:call(?SERVER, {get_pin_statistics, Filters}).

%% @doc Schedule a pin operation for future execution
schedule_pin_operation(PostID, ScheduleTime, Options) ->
    gen_server:cast(?SERVER, {schedule_pin_operation, PostID, ScheduleTime, Options}).

%% @doc Update metrics for pinning operations
update_pinning_metrics(PostID, Author, ElapsedTime) ->
    gen_server:cast(?SERVER, {update_pinning_metrics, PostID, Author, ElapsedTime}).