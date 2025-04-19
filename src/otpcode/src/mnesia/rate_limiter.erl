-module(rate_limiter).
-author("Zaryn Technologies").

%% API
-export([
    start/0,
    check_limit/4,
    reset_limit/3,
    get_limit_info/3,
    update_limit_config/3,
    get_all_limits/1,
    cleanup_expired/0
]).

%% For supervisor integration
-export([init/1]).

-define(LIMITER_TABLE, rate_limiter_usage).
-define(CONFIG_TABLE, rate_limiter_config).
-define(CLEANUP_INTERVAL, 3600000). % 1 hour in milliseconds

%%====================================================================
%% API functions
%%====================================================================

%% @doc Initialize the rate limiter system
-spec start() -> ok.
start() ->
    % Create ETS tables if they don't exist
    case ets:info(?LIMITER_TABLE) of
        undefined -> 
            ets:new(?LIMITER_TABLE, [named_table, public, set, 
                                    {write_concurrency, true}, 
                                    {read_concurrency, true}]);
        _ -> ok
    end,
    
    case ets:info(?CONFIG_TABLE) of
        undefined -> 
            ets:new(?CONFIG_TABLE, [named_table, public, set, 
                                   {write_concurrency, true}, 
                                   {read_concurrency, true}]);
        _ -> ok
    end,
    
    % Start periodic cleanup process
    spawn(fun() -> cleanup_loop() end),
    
    ok.

%% @doc Check if a specific operation for a user has reached the rate limit
%% @param IdentityId The user ID or identity to check limit for
%% @param Operation Type of operation being rate-limited (atom or string)
%% @param MaxCount Maximum number of operations allowed in the time period
%% @param PeriodSeconds Time period in seconds for the rate limit
%% @return {ok, {RemainingOperations, ResetTimeSeconds}} | {error, {rate_limit_exceeded, TimeLeftSeconds}}
-spec check_limit(term(), term(), non_neg_integer(), non_neg_integer()) -> 
    {ok, {non_neg_integer(), non_neg_integer()}} | {error, {rate_limit_exceeded, non_neg_integer()}}.
check_limit(IdentityId, Operation, MaxCount, PeriodSeconds) ->
    Key = make_key(IdentityId, Operation),
    Now = erlang:system_time(second),
    WindowStart = Now - PeriodSeconds,
    
    % Get current usage or initialize if not exists
    CurrentUsage = case ets:lookup(?LIMITER_TABLE, Key) of
        [{Key, Count1, LastUsed1, WindowStartTime1}] when WindowStartTime1 > WindowStart ->
            % Still within the window
            {Count1, LastUsed1, WindowStartTime1};
        [{Key, _Count1, _LastUsed1, WindowStartTime1}] when WindowStartTime1 =< WindowStart ->
            % Window has expired, reset counter
            NewEntry = {1, Now, Now},
            ets:insert(?LIMITER_TABLE, {Key, 1, Now, Now}),
            NewEntry;
        [{Key, Count1, LastUsed1}] ->
            % Entry exists but in the old format (missing WindowStartTime)
            % Convert to new format and continue
            NewEntry = {Count1, LastUsed1, LastUsed1},
            ets:insert(?LIMITER_TABLE, {Key, Count1, LastUsed1, LastUsed1}),
            NewEntry;
        [{Key, Count1}] ->
            % Entry exists but in the very old format (only Count)
            % Convert to new format and continue
            NewEntry = {Count1, Now, Now},
            ets:insert(?LIMITER_TABLE, {Key, Count1, Now, Now}),
            NewEntry;
        [] ->
            % First use
            NewEntry = {1, Now, Now},
            ets:insert(?LIMITER_TABLE, {Key, 1, Now, Now}),
            NewEntry
    end,
    
    {Count, _LastUsed, WindowStartTime} = CurrentUsage,
    
    % Calculate when the rate limit will reset
    ResetTime = WindowStartTime + PeriodSeconds,
    ResetInSeconds = max(0, ResetTime - Now),
    
    if
        Count > MaxCount ->
            % Rate limit exceeded
            {error, {rate_limit_exceeded, ResetInSeconds}};
        Count == MaxCount ->
            % This request consumes the last allowed operation
            try
                ets:update_counter(?LIMITER_TABLE, Key, {2, 1}),  % Increment count
                ets:update_element(?LIMITER_TABLE, Key, {3, Now}),  % Update last used time
                {ok, {0, ResetInSeconds}}
            catch
                error:badarg ->
                    % The key might have been deleted or modified in the meantime
                    % Re-insert and try again with a fresh record
                    ets:insert(?LIMITER_TABLE, {Key, Count + 1, Now, WindowStartTime}),
                    {ok, {0, ResetInSeconds}}
            end;
        true ->
            % Under the limit, increment and allow
            RemainingOps = MaxCount - Count, % Calculate this outside try/catch
            try
                ets:update_counter(?LIMITER_TABLE, Key, {2, 1}),  % Increment count
                ets:update_element(?LIMITER_TABLE, Key, {3, Now}),  % Update last used time
                {ok, {RemainingOps, ResetInSeconds}}
            catch
                error:badarg ->
                    % The key might have been deleted or modified in the meantime
                    % Re-insert and try again with a fresh record
                    ets:insert(?LIMITER_TABLE, {Key, Count + 1, Now, WindowStartTime}),
                    {ok, {RemainingOps - 1, ResetInSeconds}} % Subtract 1 since we're incrementing Count
            end
    end.

%% @doc Reset the rate limit counter for a specific operation and identity
%% @param IdentityId The user ID or identity
%% @param Operation Type of operation
%% @return ok
-spec reset_limit(term(), term(), non_neg_integer()) -> ok.
reset_limit(IdentityId, Operation, PeriodSeconds) ->
    Key = make_key(IdentityId, Operation),
    Now = erlang:system_time(second),
    ets:insert(?LIMITER_TABLE, {Key, 0, Now, Now - PeriodSeconds}),
    ok.

%% @doc Get information about current rate limit usage
%% @param IdentityId The user ID or identity
%% @param Operation Type of operation
%% @param PeriodSeconds Time period in seconds for the rate limit
%% @return {Count, RemainingTime, WindowStartTime}
-spec get_limit_info(term(), term(), non_neg_integer()) -> 
    {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
get_limit_info(IdentityId, Operation, PeriodSeconds) ->
    Key = make_key(IdentityId, Operation),
    Now = erlang:system_time(second),
    
    case ets:lookup(?LIMITER_TABLE, Key) of
        [{Key, Count, _LastUsed, WindowStartTime}] ->
            ResetTime = WindowStartTime + PeriodSeconds,
            RemainingTime = max(0, ResetTime - Now),
            {Count, RemainingTime, WindowStartTime};
        [{Key, Count, LastUsed}] ->
            % Convert to new format
            ets:insert(?LIMITER_TABLE, {Key, Count, LastUsed, LastUsed}),
            ResetTime = LastUsed + PeriodSeconds,
            RemainingTime = max(0, ResetTime - Now),
            {Count, RemainingTime, LastUsed};
        [{Key, Count}] ->
            % Convert to new format
            ets:insert(?LIMITER_TABLE, {Key, Count, Now, Now}),
            ResetTime = Now + PeriodSeconds,
            RemainingTime = max(0, ResetTime - Now),
            {Count, RemainingTime, Now};
        [] ->
            {0, 0, Now}
    end.

%% @doc Update rate limit configuration for a specific operation type
%% @param OperationType The operation type to configure
%% @param MaxCount Maximum number of operations allowed
%% @param PeriodSeconds Time period in seconds
%% @return ok
-spec update_limit_config(term(), non_neg_integer(), non_neg_integer()) -> ok.
update_limit_config(OperationType, MaxCount, PeriodSeconds) ->
    ets:insert(?CONFIG_TABLE, {OperationType, MaxCount, PeriodSeconds}),
    ok.

%% @doc Get all current limits for a specific identity
%% @param IdentityId The user ID or identity
%% @return List of {Operation, Count, RemainingTime, WindowStartTime} tuples
-spec get_all_limits(term()) -> list().
get_all_limits(IdentityId) ->
    % Create a match pattern for this identity
    IdBin = if
        is_binary(IdentityId) -> IdentityId;
        is_list(IdentityId) -> list_to_binary(IdentityId);
        is_atom(IdentityId) -> atom_to_binary(IdentityId, utf8);
        true -> term_to_binary(IdentityId)
    end,
    
    MatchHead = {'$1', '$2', '$3', '$4'},
    Guard = {'=:=', {'hd', {'split', '$1', <<"_">>}}, {const, IdBin}},
    Result = [{{'$1', '$2', '$3', '$4'}}],
    
    ets:select(?LIMITER_TABLE, [{MatchHead, [Guard], Result}]).

%% @doc Clean up expired rate limit entries
%% @return {removed_count, Count}
-spec cleanup_expired() -> {removed_count, non_neg_integer()}.
cleanup_expired() ->
    Now = erlang:system_time(second),
    
    % Get default periods for different operations
    DefaultPeriods = get_default_periods(),
    
    % Initialize removal count
    Count = ets:foldl(
        fun({Key, _Count, _LastUsed, WindowStartTime}, Acc) ->
            case binary:split(Key, <<"_">>) of
                [_IdentityId, Operation] ->
                    % Get the period for this operation
                    Period = get_period_for_operation(Operation, DefaultPeriods),
                    
                    % Check if entry has expired
                    case Now - WindowStartTime > Period of
                        true ->
                            ets:delete(?LIMITER_TABLE, Key),
                            Acc + 1;
                        false ->
                            Acc
                    end;
                _ ->
                    % Malformed key, clean it up
                    ets:delete(?LIMITER_TABLE, Key),
                    Acc + 1
            end
        end,
        0,
        ?LIMITER_TABLE
    ),
    
    {removed_count, Count}.

%%====================================================================
%% Supervisor callback
%%====================================================================

%% @doc Initialize the rate limiter for supervisor
-spec init(term()) -> {ok, {map(), list()}}.
init(_Args) ->
    % For supervisor compatibility
    {ok, {{one_for_one, 5, 10}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Create a key for the rate limiter ETS table
-spec make_key(term(), term()) -> binary().
make_key(IdentityId, Operation) ->
    IdBin = if
        is_binary(IdentityId) -> IdentityId;
        is_list(IdentityId) -> list_to_binary(IdentityId);
        is_atom(IdentityId) -> atom_to_binary(IdentityId, utf8);
        true -> term_to_binary(IdentityId)
    end,
    
    OpBin = if
        is_binary(Operation) -> Operation;
        is_list(Operation) -> list_to_binary(Operation);
        is_atom(Operation) -> atom_to_binary(Operation, utf8);
        true -> term_to_binary(Operation)
    end,
    
    <<IdBin/binary, "_", OpBin/binary>>.

%% @doc Run a continuous cleanup loop
-spec cleanup_loop() -> no_return().
cleanup_loop() ->
    timer:sleep(?CLEANUP_INTERVAL),
    cleanup_expired(),
    cleanup_loop().

%% @doc Get default time periods for different operations
-spec get_default_periods() -> map().
get_default_periods() ->
    % Either load from config or use hardcoded defaults
    case application:get_env(mazaryn, rate_limit_periods) of
        {ok, Periods} when is_map(Periods) ->
            Periods;
        _ ->
            #{
                pin_operation => 3600,   % 1 hour
                post_creation => 3600,   % 1 hour 
                file_upload => 3600,     % 1 hour
                api_call => 60,          % 1 minute
                login_attempt => 600     % 10 minutes
            }
    end.

%% @doc Get period for a specific operation
-spec get_period_for_operation(binary() | atom(), map()) -> non_neg_integer().
get_period_for_operation(Operation, DefaultPeriods) ->
    OpAtom = if
        is_binary(Operation) -> binary_to_atom(Operation, utf8);
        is_list(Operation) -> list_to_atom(Operation);
        is_atom(Operation) -> Operation;
        true -> undefined
    end,
    
    % First check config table
    case ets:lookup(?CONFIG_TABLE, OpAtom) of
        [{_, _, Period}] when is_integer(Period), Period > 0 ->
            Period;
        _ ->
            % Fall back to defaults
            maps:get(OpAtom, DefaultPeriods, 3600)
    end.