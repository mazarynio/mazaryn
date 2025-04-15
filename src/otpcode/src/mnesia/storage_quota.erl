-module(storage_quota).
-author("Zaryn Technologies").
-export([
    check_storage_quota/2,update_storage_quota/2,create_default_user_quota/1,create_default_business_quota/1,calculate_content_size/1,
    calculate_media_size/1,downgrade_to_free_tier/1,reset_quota_for_new_cycle/1,check_payment_status/1,add_days_to_date/2
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

%% @doc Calculate content size in bytes
calculate_content_size(undefined) ->
    0;
calculate_content_size(ContentCID) ->
    case ipfs_cluster:get_content_size(ContentCID) of
        {ok, Size} when is_integer(Size) -> Size;
        _ -> 1024 * 50 
    end.

%% @doc Calculate size of all media files
calculate_media_size([]) ->
    0;
calculate_media_size(MediaCIDs) ->
    lists:foldl(
        fun(CID, Acc) ->
            Size = case ipfs_cluster:get_content_size(CID) of
                {ok, S} when is_integer(S) -> S;
                _ -> 1024 * 100 
            end,
            Acc + Size
        end,
        0,
        MediaCIDs
    ).

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