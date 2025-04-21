-module(rate_limiter_server).
-author("Zaryn Technologies").

-behaviour(gen_server).
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LIMITER_TABLE, rate_limiter_usage).
-define(CONFIG_TABLE, rate_limiter_config).
-define(CLEANUP_INTERVAL, 3600000). % 1 hour in milliseconds
-define(SERVER, ?MODULE).

-record(state, {
    cleanup_timer :: reference() | undefined
}).

%% @doc Start the GenServer
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Initialize the GenServer
init([]) ->
    case ets:info(?LIMITER_TABLE) of
        undefined ->
            ets:new(?LIMITER_TABLE, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]);
        _ ->
            ok
    end,
    case ets:info(?CONFIG_TABLE) of
        undefined ->
            ets:new(?CONFIG_TABLE, [named_table, public, set, {write_concurrency, true}, {read_concurrency, true}]);
        _ ->
            ok
    end,
    
    TimerRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    
    {ok, #state{cleanup_timer = TimerRef}}.

%% @doc Handle synchronous calls
handle_call({check_limit, IdentityId, Operation, MaxCount, PeriodSeconds}, _From, State) ->
    Result = check_limit(IdentityId, Operation, MaxCount, PeriodSeconds),
    {reply, Result, State};

handle_call({get_limit_info, IdentityId, Operation, PeriodSeconds}, _From, State) ->
    Result = get_limit_info(IdentityId, Operation, PeriodSeconds),
    {reply, Result, State};

handle_call({get_all_limits, IdentityId}, _From, State) ->
    Result = get_all_limits(IdentityId),
    {reply, Result, State};

handle_call(cleanup_expired, _From, State) ->
    {Pid, MonitorRef} = spawn_monitor(fun() ->
        Result = cleanup_expired(),
        gen_server:cast(?SERVER, {cleanup_complete, Result})
    end),
    {reply, {ok, {cleanup_started, Pid, MonitorRef}}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
handle_cast({reset_limit, IdentityId, Operation, PeriodSeconds}, State) ->
    spawn(fun() ->
        reset_limit(IdentityId, Operation, PeriodSeconds)
    end),
    {noreply, State};

handle_cast({update_limit_config, OperationType, MaxCount, PeriodSeconds}, State) ->
    spawn(fun() ->
        update_limit_config(OperationType, MaxCount, PeriodSeconds)
    end),
    {noreply, State};

handle_cast({cleanup_complete, {removed_count, Count}}, State) ->
    logger:info("Cleanup completed, removed ~p entries", [Count]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(cleanup_expired, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        TimerRef -> erlang:cancel_timer(TimerRef)
    end,
    
    {Pid, MonitorRef} = spawn_monitor(fun() ->
        Result = cleanup_expired(),
        gen_server:cast(?SERVER, {cleanup_complete, Result})
    end),
    
    NewTimerRef = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    
    logger:debug("Scheduled cleanup process ~p with ref ~p", [Pid, MonitorRef]),
    {noreply, State#state{cleanup_timer = NewTimerRef}};

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    logger:warning("Cleanup started by ~p with ref ~p terminated: ~p", [Pid, MonitorRef, Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the GenServer
terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        TimerRef -> erlang:cancel_timer(TimerRef)
    end,
    ok.

%% @doc Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Check if a specific operation for a user has reached the rate limit
check_limit(IdentityId, Operation, MaxCount, PeriodSeconds) ->
    Key = make_key(IdentityId, Operation),
    Now = erlang:system_time(second),
    WindowStart = Now - PeriodSeconds,
    
    {CurrentCount, _CurrentLastUsed, CurrentWindowStartTime} = case ets:lookup(?LIMITER_TABLE, Key) of
        [{Key, Count1, LastUsed1, WindowStartTime1}] when WindowStartTime1 > WindowStart ->
            {Count1, LastUsed1, WindowStartTime1};
        [{Key, _Count1, _LastUsed1, WindowStartTime1}] when WindowStartTime1 =< WindowStart ->
            NewEntry = {1, Now, Now},
            ets:insert(?LIMITER_TABLE, {Key, 1, Now, Now}),
            NewEntry;
        [{Key, Count1, LastUsed1}] ->
            NewEntry = {Count1, LastUsed1, LastUsed1},
            ets:insert(?LIMITER_TABLE, {Key, Count1, LastUsed1, LastUsed1}),
            NewEntry;
        [{Key, Count1}] ->
            NewEntry = {Count1, Now, Now},
            ets:insert(?LIMITER_TABLE, {Key, Count1, Now, Now}),
            NewEntry;
        [] ->
            NewEntry = {1, Now, Now},
            ets:insert(?LIMITER_TABLE, {Key, 1, Now, Now}),
            NewEntry
    end,
    
    ResetTime = CurrentWindowStartTime + PeriodSeconds,
    ResetInSeconds = max(0, ResetTime - Now),
    
    if
        CurrentCount > MaxCount ->
            logger:warning("Rate limit exceeded for ~p, operation ~p", [IdentityId, Operation]),
            {error, {rate_limit_exceeded, ResetInSeconds}};
        CurrentCount == MaxCount ->
            try
                ets:update_counter(?LIMITER_TABLE, Key, {2, 1}),
                ets:update_element(?LIMITER_TABLE, Key, {3, Now}),
                {ok, {0, ResetInSeconds}}
            catch
                error:badarg ->
                    ets:insert(?LIMITER_TABLE, {Key, CurrentCount + 1, Now, CurrentWindowStartTime}),
                    {ok, {0, ResetInSeconds}}
            end;
        true ->
            RemainingOps = MaxCount - CurrentCount,
            try
                ets:update_counter(?LIMITER_TABLE, Key, {2, 1}),
                ets:update_element(?LIMITER_TABLE, Key, {3, Now}),
                {ok, {RemainingOps, ResetInSeconds}}
            catch
                error:badarg ->
                    ets:insert(?LIMITER_TABLE, {Key, CurrentCount + 1, Now, CurrentWindowStartTime}),
                    {ok, {RemainingOps - 1, ResetInSeconds}}
            end
    end.

%% @doc Reset the rate limit counter
reset_limit(IdentityId, Operation, PeriodSeconds) ->
    Key = make_key(IdentityId, Operation),
    Now = erlang:system_time(second),
    ets:insert(?LIMITER_TABLE, {Key, 0, Now, Now - PeriodSeconds}),
    logger:info("Rate limit reset for ~p, operation ~p", [IdentityId, Operation]),
    ok.

%% @doc Get information about current rate limit usage
get_limit_info(IdentityId, Operation, PeriodSeconds) ->
    Key = make_key(IdentityId, Operation),
    Now = erlang:system_time(second),
    
    case ets:lookup(?LIMITER_TABLE, Key) of
        [{Key, Count, _LastUsed, WindowStartTime}] ->
            ResetTime = WindowStartTime + PeriodSeconds,
            RemainingTime = max(0, ResetTime - Now),
            {Count, RemainingTime, WindowStartTime};
        [{Key, Count, LastUsed}] ->
            ets:insert(?LIMITER_TABLE, {Key, Count, LastUsed, LastUsed}),
            ResetTime = LastUsed + PeriodSeconds,
            RemainingTime = max(0, ResetTime - Now),
            {Count, RemainingTime, LastUsed};
        [{Key, Count}] ->
            ets:insert(?LIMITER_TABLE, {Key, Count, Now, Now}),
            ResetTime = Now + PeriodSeconds,
            RemainingTime = max(0, ResetTime - Now),
            {Count, RemainingTime, Now};
        [] ->
            {0, 0, Now}
    end.

%% @doc Update rate limit configuration
update_limit_config(OperationType, MaxCount, PeriodSeconds) ->
    ets:insert(?CONFIG_TABLE, {OperationType, MaxCount, PeriodSeconds}),
    logger:info("Updated rate limit config for ~p: max_count=~p, period=~p", 
                [OperationType, MaxCount, PeriodSeconds]),
    ok.

%% @doc Get all current limits for a specific identity
get_all_limits(IdentityId) ->
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
cleanup_expired() ->
    Now = erlang:system_time(second),
    DefaultPeriods = get_default_periods(),
    
    Count = ets:foldl(
        fun({Key, _Count, _LastUsed, WindowStartTime}, Acc) ->
            case binary:split(Key, <<"_">>) of
                [_IdentityId, Operation] ->
                    Period = get_period_for_operation(Operation, DefaultPeriods),
                    case Now - WindowStartTime > Period of
                        true ->
                            ets:delete(?LIMITER_TABLE, Key),
                            Acc + 1;
                        false ->
                            Acc
                    end;
                _ ->
                    ets:delete(?LIMITER_TABLE, Key),
                    Acc + 1
            end
        end,
        0,
        ?LIMITER_TABLE
    ),
    
    {removed_count, Count}.

%% @doc Create a key for the rate limiter ETS table
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

%% @doc Get default time periods for different operations
get_default_periods() ->
    case application:get_env(mazaryn, rate_limit_periods) of
        {ok, Periods} when is_map(Periods) ->
            Periods;
        _ ->
            #{
                pin_operation => 3600,
                post_creation => 3600,
                file_upload => 3600,
                api_call => 60,
                login_attempt => 600
            }
    end.

%% @doc Get period for a specific operation
get_period_for_operation(Operation, DefaultPeriods) ->
    OpAtom = if
        is_binary(Operation) -> binary_to_atom(Operation, utf8);
        is_list(Operation) -> list_to_atom(Operation);
        is_atom(Operation) -> Operation;
        true -> undefined
    end,
    
    case ets:lookup(?CONFIG_TABLE, OpAtom) of
        [{_, _, Period}] when is_integer(Period), Period > 0 ->
            Period;
        _ ->
            maps:get(OpAtom, DefaultPeriods, 3600)
    end.