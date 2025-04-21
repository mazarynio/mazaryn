-module(rate_limiter).
-author("Zaryn Technologies").

-export([
    start/0,
    check_limit/4,
    reset_limit/3,
    get_limit_info/3,
    update_limit_config/3,
    get_all_limits/1,
    cleanup_expired/0,
    start_link/0
]).

-define(SERVER, rate_limiter_server).

%% @doc Start the GenServer
start() ->
    rate_limiter_server:start_link().

%% @doc Start the GenServer (for supervisor integration)
start_link() ->
    gen_server:start_link({local, ?SERVER}, rate_limiter_server, [], []).

%% @doc Check if a specific operation for a user has reached the rate limit
-spec check_limit(term(), term(), non_neg_integer(), non_neg_integer()) ->
    {ok, {non_neg_integer(), non_neg_integer()}} | {error, {rate_limit_exceeded, non_neg_integer()}}.
check_limit(IdentityId, Operation, MaxCount, PeriodSeconds) ->
    gen_server:call(?SERVER, {check_limit, IdentityId, Operation, MaxCount, PeriodSeconds}).

%% @doc Reset the rate limit counter for a specific operation and identity
-spec reset_limit(term(), term(), non_neg_integer()) -> ok.
reset_limit(IdentityId, Operation, PeriodSeconds) ->
    gen_server:cast(?SERVER, {reset_limit, IdentityId, Operation, PeriodSeconds}).

%% @doc Get information about current rate limit usage
-spec get_limit_info(term(), term(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
get_limit_info(IdentityId, Operation, PeriodSeconds) ->
    gen_server:call(?SERVER, {get_limit_info, IdentityId, Operation, PeriodSeconds}).

%% @doc Update rate limit configuration for a specific operation type
-spec update_limit_config(term(), non_neg_integer(), non_neg_integer()) -> ok.
update_limit_config(OperationType, MaxCount, PeriodSeconds) ->
    gen_server:cast(?SERVER, {update_limit_config, OperationType, MaxCount, PeriodSeconds}).

%% @doc Get all current limits for a specific identity
-spec get_all_limits(term()) -> list().
get_all_limits(IdentityId) ->
    gen_server:call(?SERVER, {get_all_limits, IdentityId}).

%% @doc Clean up expired rate limit entries
-spec cleanup_expired() -> {removed_count, non_neg_integer()}.
cleanup_expired() ->
    gen_server:call(?SERVER, cleanup_expired).