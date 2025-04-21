-module(storage_quota).
-author("Zaryn Technologies").
-export([
    check_storage_quota/2, update_storage_quota/2, create_default_user_quota/1, create_default_business_quota/1,
    calculate_content_size/1, calculate_media_size/1, downgrade_to_free_tier/1, reset_quota_for_new_cycle/1,
    check_payment_status/1, add_days_to_date/2, release_storage_quota/3, update_quota_after_unpin/2, start_link/0
]).

-define(SERVER, storage_quota_server).

%% @doc Start the GenServer
start_link() ->
    gen_server:start_link({local, ?SERVER}, storage_quota_server, [], []).

%% @doc Check if user or business has available quota
check_storage_quota(UserID, BusinessID) ->
    gen_server:call(?SERVER, {check_storage_quota, UserID, BusinessID}).

%% @doc Update storage quota after successful pin
update_storage_quota(Quota, SizeBytes) ->
    gen_server:call(?SERVER, {update_storage_quota, Quota, SizeBytes}).

%% @doc Release storage quota after unpinning content
release_storage_quota(UserID, BusinessID, SizeBytes) ->
    gen_server:call(?SERVER, {release_storage_quota, UserID, BusinessID, SizeBytes}).

%% @doc Update quota after unpinning content
update_quota_after_unpin(Quota, SizeBytes) ->
    gen_server:call(?SERVER, {update_quota_after_unpin, Quota, SizeBytes}).

%% @doc Create default quota for new users
create_default_user_quota(UserID) ->
    gen_server:call(?SERVER, {create_default_user_quota, UserID}).

%% @doc Create default quota for business accounts
create_default_business_quota(BusinessID) ->
    gen_server:call(?SERVER, {create_default_business_quota, BusinessID}).

%% @doc Calculate the size of a content CID in bytes
calculate_content_size(CID) ->
    gen_server:call(?SERVER, {calculate_content_size, CID}).

%% @doc Calculate the total size of a list of media CIDs in bytes
calculate_media_size(MediaCIDs) ->
    gen_server:call(?SERVER, {calculate_media_size, MediaCIDs}).

%% @doc Reset quota for new billing cycle
reset_quota_for_new_cycle(Quota) ->
    gen_server:cast(?SERVER, {reset_quota_for_new_cycle, Quota}).

%% @doc Downgrade account to free tier
downgrade_to_free_tier(Quota) ->
    gen_server:call(?SERVER, {downgrade_to_free_tier, Quota}).

%% @doc Check if user/business has valid payment
check_payment_status(Quota) ->
    gen_server:call(?SERVER, {check_payment_status, Quota}).

%% @doc Add days to a date
add_days_to_date(Date, Days) ->
    gen_server:call(?SERVER, {add_days_to_date, Date, Days}).