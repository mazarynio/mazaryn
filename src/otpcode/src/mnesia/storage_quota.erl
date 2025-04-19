-module(storage_quota).
-author("Zaryn Technologies").
-export([
    check_storage_quota/2,update_storage_quota/2,create_default_user_quota/1,create_default_business_quota/1,calculate_content_size/1,
    calculate_media_size/1,downgrade_to_free_tier/1,reset_quota_for_new_cycle/1,check_payment_status/1,add_days_to_date/2, release_storage_quota/3,
    update_quota_after_unpin/2
]).

-include("../records.hrl").

%% @doc Check if user or business has available quota
check_storage_quota(UserID, undefined) ->
    case mnesia:read({storage_quota, UserID}) of
        [Quota] ->
            validate_quota(Quota);
        [] ->
            DefaultQuota = create_default_user_quota(UserID),
            mnesia:write(DefaultQuota),
            {ok, DefaultQuota}
    end;
check_storage_quota(_, BusinessID) when BusinessID =/= undefined ->
    case mnesia:read({storage_quota, BusinessID}) of
        [Quota] ->
            validate_quota(Quota);
        [] ->
            DefaultQuota = create_default_business_quota(BusinessID),
            mnesia:write(DefaultQuota),
            {ok, DefaultQuota}
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
                    case check_payment_status(Quota) of
                        {ok, paid} ->
                            NewQuota = reset_quota_for_new_cycle(Quota),
                            mnesia:write(NewQuota),
                            validate_limits(NewQuota);
                        {ok, unpaid} ->
                            FreeQuota = downgrade_to_free_tier(Quota),
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
update_storage_quota(Quota, SizeBytes) ->
    NewStorageUsed = Quota#storage_quota.storage_used_bytes + SizeBytes,
    NewPinCount = Quota#storage_quota.pin_count + 1,
    
    UpdatedQuota = Quota#storage_quota{
        storage_used_bytes = NewStorageUsed,
        pin_count = NewPinCount
    },
    
    mnesia:write(UpdatedQuota),
    {ok, UpdatedQuota}.

%% @doc Release storage quota after unpinning content
release_storage_quota(UserID, BusinessID, SizeBytes) ->
    case BusinessID of
        undefined ->
            case mnesia:read({storage_quota, UserID}) of
                [Quota] ->
                    update_quota_after_unpin(Quota, SizeBytes);
                [] ->
                    {error, quota_not_found}
            end;
        _ ->
            case mnesia:read({storage_quota, BusinessID}) of
                [Quota] ->
                    update_quota_after_unpin(Quota, SizeBytes);
                [] ->
                    {error, quota_not_found}
            end
    end.

%% @doc Update quota after unpinning content
update_quota_after_unpin(Quota, SizeBytes) ->
    NewStorageUsed = max(0, Quota#storage_quota.storage_used_bytes - SizeBytes),
    NewPinCount = max(0, Quota#storage_quota.pin_count - 1),
    
    UpdatedQuota = Quota#storage_quota{
        storage_used_bytes = NewStorageUsed,
        pin_count = NewPinCount
    },
    
    mnesia:write(UpdatedQuota),
    {ok, UpdatedQuota}.

%% @doc Create default quota for new users
create_default_user_quota(UserID) ->
    Now = calendar:universal_time(),
    BillingEnd = add_days_to_date(Now, 30),
    
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
create_default_business_quota(BusinessID) ->
    Now = calendar:universal_time(),
    BillingEnd = add_days_to_date(Now, 30),
    
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

%% @doc Calculate the size of a content CID in bytes.
%% Returns 0 for undefined CIDs, the actual size if available, or an estimated size based on content type.
%% Logs warnings for errors or fallbacks.
calculate_content_size(undefined) ->
    logger:debug("No CID provided for size calculation, returning 0"),
    0;
calculate_content_size(ContentCID) when is_list(ContentCID) orelse is_binary(ContentCID) ->
    case ipfs_cluster:get_content_size(ContentCID) of
        {ok, Size} when is_integer(Size), Size >= 0 ->
            logger:debug("Size for CID ~p: ~p bytes", [ContentCID, Size]),
            Size;
        {ok, InvalidSize} ->
            logger:warning("Invalid size ~p for CID ~p, using fallback", [InvalidSize, ContentCID]),
            estimate_content_size(ContentCID);
        {error, Reason} ->
            logger:warning("Failed to get size for CID ~p: ~p, using fallback", [ContentCID, Reason]),
            estimate_content_size(ContentCID)
    end;
calculate_content_size(Invalid) ->
    logger:error("Invalid CID format ~p, returning 0", [Invalid]),
    0.

%% @doc Estimate content size based on CID or configuration.
estimate_content_size(_CID) ->
    % Use configuration for fallback size, defaulting to 50KB
    FallbackSize = application:get_env(mazaryn, default_content_size, 1024 * 50),
    logger:info("Using estimated content size: ~p bytes", [FallbackSize]),
    FallbackSize.

%% @doc Calculate the total size of a list of media CIDs in bytes.
%% Returns 0 for empty lists, sums actual sizes when available, or uses estimated sizes for failures.
%% Processes CIDs in parallel for performance.
calculate_media_size([]) ->
    logger:debug("No media CIDs provided, returning 0"),
    0;
calculate_media_size(MediaCIDs) when is_list(MediaCIDs) ->
    % Use parallel map to process CIDs concurrently
    Sizes = pmap(
        fun(CID) ->
            case calculate_single_media_size(CID) of
                Size when is_integer(Size), Size >= 0 -> Size;
                _ -> 0
            end
        end,
        MediaCIDs
    ),
    TotalSize = lists:sum(Sizes),
    logger:debug("Total media size for ~p CIDs: ~p bytes", [length(MediaCIDs), TotalSize]),
    TotalSize;
calculate_media_size(Invalid) ->
    logger:error("Invalid media CIDs format ~p, returning 0", [Invalid]),
    0.

%% @doc Calculate size for a single media CID.
calculate_single_media_size(CID) when is_list(CID) orelse is_binary(CID) ->
    case ipfs_cluster:get_content_size(CID) of
        {ok, Size} when is_integer(Size), Size >= 0 ->
            logger:debug("Size for media CID ~p: ~p bytes", [CID, Size]),
            Size;
        {ok, InvalidSize} ->
            logger:warning("Invalid size ~p for media CID ~p, using fallback", [InvalidSize, CID]),
            estimate_media_size(CID);
        {error, Reason} ->
            logger:warning("Failed to get size for media CID ~p: ~p, using fallback", [CID, Reason]),
            estimate_media_size(CID)
    end;
calculate_single_media_size(Invalid) ->
    logger:error("Invalid media CID format ~p, returning 0", [Invalid]),
    0.

%% @doc Estimate media size based on CID or configuration.
estimate_media_size(_CID) ->
    % Use configuration for fallback size, defaulting to 100KB
    FallbackSize = application:get_env(mazaryn, default_media_size, 1024 * 100),
    logger:info("Using estimated media size: ~p bytes", [FallbackSize]),
    FallbackSize.

%% @doc Parallel map for processing lists concurrently.
pmap(Fun, List) ->
    Parent = self(),
    Refs = [spawn_monitor(fun() -> Parent ! {self(), Fun(Item)} end) || Item <- List],
    [receive
         {Pid, Result} -> Result
     after 60000 ->
         logger:error("Timeout processing item in pmap for PID ~p", [Pid]),
         0
     end || {Pid, _Ref} <- Refs].

%% @doc Reset quota for new billing cycle
reset_quota_for_new_cycle(Quota) ->
    Now = calendar:universal_time(),
    BillingEnd = add_days_to_date(Now, 30),
    
    Quota#storage_quota{
        billing_cycle_start = Now,
        billing_cycle_end = BillingEnd
    }.

%% @doc Downgrade account to free tier
downgrade_to_free_tier(Quota) ->
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
check_payment_status(Quota) ->
    AccountID = case Quota#storage_quota.business_id of
        undefined -> Quota#storage_quota.user_id;
        BusinessID -> BusinessID
    end,
    
    case billing:check_subscription_status(AccountID) of
        {ok, Status} -> {ok, Status};
        {error, Reason} -> {error, Reason};
        _ -> {ok, paid} 
    end.

%% @doc Add days to a date
add_days_to_date(Date, Days) ->
    Seconds = calendar:datetime_to_gregorian_seconds(Date),
    NewSeconds = Seconds + (Days * 24 * 60 * 60),
    calendar:gregorian_seconds_to_datetime(NewSeconds).