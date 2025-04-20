-module(storage_quota_server).
-behaviour(gen_server).
-author("Zaryn Technologies").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         calculate_content_size_internal/1, calculate_media_size_internal/1]).

-include("../records.hrl").

-record(state, {
    active_operations = #{} :: map() 
}).

%% @doc Start the GenServer
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Initialize the GenServer
init([]) ->
    {ok, #state{}}.

%% @doc Handle synchronous calls
handle_call({check_storage_quota, UserID, BusinessID}, _From, State) ->
    Result = check_storage_quota_internal(UserID, BusinessID),
    {reply, Result, State};

handle_call({update_storage_quota, Quota, SizeBytes}, _From, State) ->
    try
        Result = update_storage_quota_internal(Quota, SizeBytes),
        {reply, Result, State}
    catch
        error:{badrecord, Invalid} ->
            logger:error("Invalid quota record: ~p", [Invalid]),
            {reply, {error, invalid_quota_record}, State}
    end;

handle_call({release_storage_quota, UserID, BusinessID, SizeBytes}, _From, State) ->
    Result = release_storage_quota_internal(UserID, BusinessID, SizeBytes),
    {reply, Result, State};

handle_call({update_quota_after_unpin, Quota, SizeBytes}, _From, State) ->
    Result = update_quota_after_unpin_internal(Quota, SizeBytes),
    {reply, Result, State};

handle_call({create_default_user_quota, UserID}, _From, State) ->
    Quota = create_default_user_quota_internal(UserID),
    {reply, Quota, State};

handle_call({create_default_business_quota, BusinessID}, _From, State) ->
    Quota = create_default_business_quota_internal(BusinessID),
    {reply, Quota, State};

handle_call({calculate_content_size, CID}, From, State) ->
    RequestId = generate_request_id(),
    {CallerPid, _Tag} = From, 
    Ref = make_ref(),
    {Pid, MonRef} = spawn_monitor(fun() ->
        Size = calculate_content_size_internal(CID),
        CallerPid ! {content_size_result, RequestId, Ref, Size},
        gen_server:cast(?MODULE, {operation_complete, RequestId, Size})
    end),
    NewActiveOps = maps:put(RequestId, {Pid, MonRef}, State#state.active_operations),
    {reply, {ok, #{request_id => RequestId, ref => Ref}}, State#state{active_operations = NewActiveOps}};

handle_call({calculate_media_size, MediaCIDs}, From, State) ->
    RequestId = generate_request_id(),
    {CallerPid, _Tag} = From,  
    Ref = make_ref(),
    {Pid, MonRef} = spawn_monitor(fun() ->
        Size = calculate_media_size_internal(MediaCIDs),
        CallerPid ! {media_size_result, RequestId, Ref, Size},
        gen_server:cast(?MODULE, {operation_complete, RequestId, Size})
    end),
    NewActiveOps = maps:put(RequestId, {Pid, MonRef}, State#state.active_operations),
    {reply, {ok, #{request_id => RequestId, ref => Ref}}, State#state{active_operations = NewActiveOps}};

handle_call({downgrade_to_free_tier, Quota}, _From, State) ->
    Result = downgrade_to_free_tier_internal(Quota),
    {reply, Result, State};

handle_call({check_payment_status, Quota}, _From, State) ->
    RequestId = generate_request_id(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Result = check_payment_status_internal(Quota),
        gen_server:cast(?MODULE, {operation_complete, RequestId, Result})
    end),
    NewActiveOps = maps:put(RequestId, {Pid, Ref}, State#state.active_operations),
    {reply, {ok, #{request_id => RequestId}}, State#state{active_operations = NewActiveOps}};

handle_call({add_days_to_date, Date, Days}, _From, State) ->
    Result = add_days_to_date_internal(Date, Days),
    {reply, Result, State}.

%% @doc Handle asynchronous casts
handle_cast({reset_quota_for_new_cycle, Quota}, State) ->
    spawn(fun() ->
        NewQuota = reset_quota_for_new_cycle_internal(Quota),
        mnesia:write(NewQuota)
    end),
    {noreply, State};

handle_cast({operation_complete, RequestId, Result}, State) ->
    spawn(fun() ->
        logger:debug("Operation ~p completed with result: ~p", [RequestId, Result])
    end),
    NewActiveOps = maps:remove(RequestId, State#state.active_operations),
    {noreply, State#state{active_operations = NewActiveOps}}.

%% @doc Handle info messages
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    case find_active_operation(Pid, Ref, State#state.active_operations) of
        {RequestId, _} ->
            logger:error("Operation ~p failed: ~p", [RequestId, Reason]),
            NewActiveOps = maps:remove(RequestId, State#state.active_operations),
            {noreply, State#state{active_operations = NewActiveOps}};
        not_found ->
            {noreply, State}
    end.

%% @doc Terminate the GenServer
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Check if user or business has available quota
check_storage_quota_internal(UserID, undefined) ->
    Fun = fun() ->
        case mnesia:read({storage_quota, UserID}) of
            [Quota] ->
                validate_quota(Quota);
            [] ->
                DefaultQuota = create_default_user_quota_internal(UserID),
                mnesia:write(DefaultQuota),
                {ok, DefaultQuota}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            logger:debug("Quota check for user ~p: ~p", [UserID, Result]),
            Result;
        {aborted, Reason} ->
            logger:error("Quota check transaction failed for user ~p: ~p", [UserID, Reason]),
            {error, {transaction_failed, Reason}}
    end;

check_storage_quota_internal(_, BusinessID) when BusinessID =/= undefined ->
    Fun = fun() ->
        case mnesia:read({storage_quota, BusinessID}) of
            [Quota] ->
                validate_quota(Quota);
            [] ->
                DefaultQuota = create_default_business_quota_internal(BusinessID),
                mnesia:write(DefaultQuota),
                {ok, DefaultQuota}
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            logger:debug("Quota check for business ~p: ~p", [BusinessID, Result]),
            Result;
        {aborted, Reason} ->
            logger:error("Quota check transaction failed for business ~p: ~p", [BusinessID, Reason]),
            {error, {transaction_failed, Reason}}
    end.

%% @doc Validate if quota allows another pin operation
validate_quota(Quota) ->
    Now = calendar:universal_time(),
    case Quota#storage_quota.billing_cycle_end > Now of
        false ->
            case Quota#storage_quota.tier_level of
                <<"free">> ->
                    validate_limits(Quota);
                _ ->
                    case check_payment_status_internal(Quota) of
                        {ok, paid} ->
                            NewQuota = reset_quota_for_new_cycle_internal(Quota),
                            mnesia:write(NewQuota),
                            validate_limits(NewQuota);
                        {ok, unpaid} ->
                            FreeQuota = downgrade_to_free_tier_internal(Quota),
                            mnesia:write(FreeQuota),
                            validate_limits(FreeQuota);
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        true ->
            validate_limits(Quota)
    end.

%% @doc Validate storage and pin limits
validate_limits(Quota) ->
    StorageUsed = Quota#storage_quota.storage_used_bytes,
    StorageLimit = Quota#storage_quota.storage_limit_bytes,
    PinCount = Quota#storage_quota.pin_count,
    PinLimit = Quota#storage_quota.pin_limit,
    
    case StorageUsed >= StorageLimit of
        true -> {error, quota_exceeded};
        false ->
            case PinCount >= PinLimit of
                true -> {error, pin_limit_reached};
                false -> {ok, Quota}
            end
    end.

%% @doc Update storage quota after successful pin
update_storage_quota_internal(Quota, SizeBytes) ->
    Fun = fun() ->
        NewStorageUsed = Quota#storage_quota.storage_used_bytes + SizeBytes,
        NewPinCount = Quota#storage_quota.pin_count + 1,
        UpdatedQuota = Quota#storage_quota{
            storage_used_bytes = NewStorageUsed,
            pin_count = NewPinCount
        },
        mnesia:write(UpdatedQuota),
        {ok, UpdatedQuota}
    end,
    case mnesia:transaction(Fun) of
        {atomic, {ok, UpdatedQuota}} ->
            {ok, UpdatedQuota};
        {aborted, Reason} ->
            logger:error("Quota update transaction failed: ~p", [Reason]),
            {error, {transaction_failed, Reason}}
    end.

%% @doc Release storage quota after unpinning content
release_storage_quota_internal(UserID, BusinessID, SizeBytes) ->
    case BusinessID of
        undefined ->
            case mnesia:read({storage_quota, UserID}) of
                [Quota] ->
                    update_quota_after_unpin_internal(Quota, SizeBytes);
                [] ->
                    {error, quota_not_found}
            end;
        _ ->
            case mnesia:read({storage_quota, BusinessID}) of
                [Quota] ->
                    update_quota_after_unpin_internal(Quota, SizeBytes);
                [] ->
                    {error, quota_not_found}
            end
    end.

%% @doc Update quota after unpinning content
update_quota_after_unpin_internal(Quota, SizeBytes) ->
    NewStorageUsed = max(0, Quota#storage_quota.storage_used_bytes - SizeBytes),
    NewPinCount = max(0, Quota#storage_quota.pin_count - 1),
    
    UpdatedQuota = Quota#storage_quota{
        storage_used_bytes = NewStorageUsed,
        pin_count = NewPinCount
    },
    
    mnesia:write(UpdatedQuota),
    {ok, UpdatedQuota}.

%% @doc Create default quota for new users
create_default_user_quota_internal(UserID) ->
    Now = calendar:universal_time(),
    BillingEnd = add_days_to_date_internal(Now, 30),
    
    #storage_quota{
        user_id = UserID,
        business_id = undefined,
        storage_used_bytes = 0,
        storage_limit_bytes = 1024 * 1024 * 100, % 100MB 
        pin_count = 0,
        pin_limit = 50,
        tier_level = <<"free">>,
        cost_per_gb = 0,
        billing_cycle_start = Now,
        billing_cycle_end = BillingEnd
    }.

%% @doc Create default quota for business accounts
create_default_business_quota_internal(BusinessID) ->
    Now = calendar:universal_time(),
    BillingEnd = add_days_to_date_internal(Now, 30),
    
    #storage_quota{
        user_id = undefined,
        business_id = BusinessID,
        storage_used_bytes = 0,
        storage_limit_bytes = 1024 * 1024 * 250, % 250MB 
        pin_count = 0,
        pin_limit = 100, 
        tier_level = <<"free">>,
        cost_per_gb = 0,
        billing_cycle_start = Now,
        billing_cycle_end = BillingEnd
    }.

%% @doc Calculate the size of a content CID in bytes
calculate_content_size_internal(undefined) ->
    logger:debug("No CID provided for size calculation, returning 0"),
    0;
calculate_content_size_internal(CID) when is_list(CID) orelse is_binary(CID) ->
    try
        case ipfs_cluster:get_content_size(CID) of
            {ok, Size} when is_integer(Size), Size >= 0 ->
                logger:debug("Size for CID ~p: ~p bytes", [CID, Size]),
                Size;
            {ok, InvalidSize} ->
                logger:warning("Invalid size ~p for CID ~p, using fallback", [InvalidSize, CID]),
                estimate_content_size(CID);
            {error, Reason} ->
                logger:warning("Failed to get size for CID ~p: ~p, using fallback", [CID, Reason]),
                estimate_content_size(CID)
        end
    catch
        Class:error:Stacktrace ->
            logger:error("Exception in content size calculation for CID ~p: ~p:~p~n~p", [CID, Class, error, Stacktrace]),
            estimate_content_size(CID)
    end;
calculate_content_size_internal(Invalid) ->
    logger:error("Invalid CID format ~p, returning 0", [Invalid]),
    0.

%% @doc Calculate the total size of a list of media CIDs in bytes
calculate_media_size_internal([]) ->
    logger:debug("No media CIDs provided, returning 0"),
    0;
calculate_media_size_internal(MediaCIDs) ->
    logger:debug("Calculating media size for CIDs: ~p", [MediaCIDs]),
    try
        Size = lists:foldl(fun(CID, Acc) ->
            case ipfs_cluster:get_content_size(CID) of
                {ok, S} when is_integer(S), S >= 0 ->
                    logger:debug("Size for CID ~p: ~p bytes", [CID, S]),
                    Acc + S;
                {error, Reason} ->
                    logger:warning("Failed to get size for CID ~p: ~p", [CID, Reason]),
                    Acc;
                Invalid ->
                    logger:warning("Invalid size for CID ~p: ~p", [CID, Invalid]),
                    Acc
            end
        end, 0, MediaCIDs),
        logger:debug("Total media size: ~p bytes", [Size]),
        Size
    catch
        Error:Reason ->
            logger:error("Error calculating media size: ~p", [{Error, Reason}]),
            0
    end;
calculate_media_size_internal(Invalid) ->
    logger:error("Invalid media CIDs format ~p, returning 0", [Invalid]),
    0.

%% @doc Estimate content size based on CID or configuration
estimate_content_size(_CID) ->
    FallbackSize = application:get_env(mazaryn, default_content_size, 1024 * 50),
    logger:info("Using estimated content size: ~p bytes", [FallbackSize]),
    FallbackSize.

%% @doc Reset quota for new billing cycle
reset_quota_for_new_cycle_internal(Quota) ->
    Now = calendar:universal_time(),
    BillingEnd = add_days_to_date_internal(Now, 30),
    
    Quota#storage_quota{
        billing_cycle_start = Now,
        billing_cycle_end = BillingEnd
    }.

%% @doc Downgrade account to free tier
downgrade_to_free_tier_internal(Quota) ->
    case Quota#storage_quota.business_id of
        undefined ->
            Quota#storage_quota{
                storage_limit_bytes = 1024 * 1024 * 100, % 100MB
                pin_limit = 50,
                tier_level = <<"free">>,
                cost_per_gb = 0
            };
        _ ->
            Quota#storage_quota{
                storage_limit_bytes = 1024 * 1024 * 250, % 250MB
                pin_limit = 100,
                tier_level = <<"free">>,
                cost_per_gb = 0
            }
    end.

%% @doc Check if user/business has valid payment
check_payment_status_internal(Quota) ->
    AccountID = case Quota#storage_quota.business_id of
        undefined -> Quota#storage_quota.user_id;
        BusinessID -> BusinessID
    end,
    
    try
        case billing:check_subscription_status(AccountID) of
            {ok, Status} -> {ok, Status};
            {error, Reason} -> {error, Reason};
            _ -> {ok, paid}
        end
    catch
        Class:error:Stacktrace ->
            logger:error("Exception in payment status check for account ~p: ~p:~p~n~p", [AccountID, Class, error, Stacktrace]),
            {error, payment_check_failed}
    end.

%% @doc Add days to a date
add_days_to_date_internal(Date, Days) ->
    Seconds = calendar:datetime_to_gregorian_seconds(Date),
    NewSeconds = Seconds + (Days * 24 * 60 * 60),
    calendar:gregorian_seconds_to_datetime(NewSeconds).

%% @doc Generate a unique request ID
generate_request_id() ->
    Unique = integer_to_list(erlang:phash2({os:timestamp(), self()})),
    "req_" ++ Unique.

%% @doc Find active operation by Pid and Ref
find_active_operation(Pid, Ref, ActiveOps) ->
    Filtered = maps:filter(fun(_, {P, R}) -> P == Pid andalso R == Ref end, ActiveOps),
    case maps:to_list(Filtered) of
        [{Key, Value}] -> {Key, Value};
        [] -> not_found
    end.